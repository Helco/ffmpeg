/*
 * Bink demuxer
 * Copyright (c) 2022 Hermann Noll (hermann.noll@hotmail.com)
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * DigiChrome demuxer
 */

#include <inttypes.h>
#include <ctype.h>

#include "internal.h"

#define DIGICHROME_MAX_TRACKS 256
#define DIGICHROME_SECTION_SIZE 4
#define DIGICHROME_AUDIO_SAMPLE_RATE 19800
#define DIGICHROME_DEFAULT_TICKSPERFRAME 1577 // roughly 12.5 FPS
#define DIGICHROME_AUDIO_HEADER_SIZE 9

enum DigiChromeBlockType {
    DIGICHROME_TYPE_VIDEO = 0x81,
    DIGICHROME_TYPE_AUDIO = 0xA2,
    DIGICHROME_TYPE_COMBINED = 0xF1,

    DIGICHROME_TYPE_NOTSET = -1
};

typedef struct DigiChromeBlockHeader {
    int type;
    int track;
    int size;
} DigiChromeBlockHeader;

typedef struct DigiChromeDemuxContext {
    int remainingCombinedSize;
    DigiChromeBlockHeader storedHeader;
    int64_t nextTimestamp;
    int videoStreamIndices[DIGICHROME_MAX_TRACKS];
    int audioStreamIndices[DIGICHROME_MAX_TRACKS];
} DigiChromeDemuxContext;

static int probe(const AVProbeData *p)
{
    size_t filenameLen = strlen(p->filename);
    if (filenameLen < 4)
        return 0;
    if (p->filename[filenameLen - 4] == '.' &&
        tolower(p->filename[filenameLen - 3]) == 'a' &&
        tolower(p->filename[filenameLen - 2]) == 'v' &&
        tolower(p->filename[filenameLen - 1]) == 'c')
        return AVPROBE_SCORE_EXTENSION;
    return 0;
}

static int read_header(AVFormatContext *s)
{
    DigiChromeDemuxContext *digiChrome = s->priv_data;
    digiChrome->storedHeader.type = DIGICHROME_TYPE_NOTSET;
    for (int i = 0; i < DIGICHROME_MAX_TRACKS; i++) {
        digiChrome->videoStreamIndices[i] = -1;
        digiChrome->audioStreamIndices[i] = -1;
    }

    s->ctx_flags |= AVFMTCTX_NOHEADER;
    return 0;
}

static int create_video_stream(AVFormatContext *s, int track)
{
    AVStream *st = avformat_new_stream(s, NULL);
    if (!st)
        return AVERROR(ENOMEM);
    st->codecpar->codec_type = AVMEDIA_TYPE_VIDEO;
    st->codecpar->codec_id = AV_CODEC_ID_DIGICHROME;
    avpriv_set_pts_info(st, 64, 1, DIGICHROME_AUDIO_SAMPLE_RATE);
    st->avg_frame_rate.num = DIGICHROME_AUDIO_SAMPLE_RATE;
    st->avg_frame_rate.den = DIGICHROME_DEFAULT_TICKSPERFRAME;
    
    DigiChromeDemuxContext *digiChrome = s->priv_data;
    digiChrome->videoStreamIndices[track] = st->index;
    return st->index;
}

static int create_audio_stream(AVFormatContext *s, int track)
{
    AVStream *st = avformat_new_stream(s, NULL);
    if (!st)
        return AVERROR(ENOMEM);
    st->codecpar->codec_type = AVMEDIA_TYPE_AUDIO;
    st->codecpar->codec_id = AV_CODEC_ID_PCM_U8;
    st->codecpar->ch_layout = (AVChannelLayout)AV_CHANNEL_LAYOUT_MONO;
    st->codecpar->bits_per_coded_sample = 8;
    st->codecpar->sample_rate = DIGICHROME_AUDIO_SAMPLE_RATE;
    st->codecpar->format = AV_SAMPLE_FMT_U8;
    avpriv_set_pts_info(st, 64, 1, DIGICHROME_AUDIO_SAMPLE_RATE);
    st->avg_frame_rate.num = DIGICHROME_DEFAULT_TICKSPERFRAME;
    st->avg_frame_rate.den = DIGICHROME_AUDIO_SAMPLE_RATE;

    DigiChromeDemuxContext *digiChrome = s->priv_data;
    digiChrome->audioStreamIndices[track] = st->index;
    return st->index;
}

static void read_block_header(DigiChromeBlockHeader *header, AVIOContext *pb)
{
    header->type = avio_r8(pb);
    header->track = avio_r8(pb);
    header->size = avio_rl16(pb);
}

/**
 * @brief Tries to read the section header of the next audio packet.
 * 
 * Audio packets come immediately after video packets in all examined samples 
 * that do contain both audio and video. In these cases the duration of the 
 * audio packet also determines the duration of the video frame.
 * 
 * @param s The format context
 * @param track The track ID for which to find an audio packet
 * @return int The duration of the audio packet or an error
 */
static int try_read_audio_duration(AVFormatContext *s, int track)
{
    DigiChromeDemuxContext *digiChrome = s->priv_data;
    AVIOContext *pb = s->pb;

    read_block_header(&digiChrome->storedHeader, pb);
    if (pb->eof_reached ||
        digiChrome->storedHeader.type != DIGICHROME_TYPE_AUDIO ||
        digiChrome->storedHeader.track != track)
        return AVERROR_EOF;

    return digiChrome->storedHeader.size - DIGICHROME_AUDIO_HEADER_SIZE;
}

static int read_packet(AVFormatContext *s, AVPacket *pkt)
{
    AVIOContext *pb = s->pb;
    DigiChromeDemuxContext *digiChrome = s->priv_data;

    DigiChromeBlockHeader header = digiChrome->storedHeader;
    if (header.type == DIGICHROME_TYPE_NOTSET) {
        if (digiChrome->remainingCombinedSize < DIGICHROME_SECTION_SIZE) {
            avio_skip(pb, digiChrome->remainingCombinedSize);
            digiChrome->remainingCombinedSize = 0;
        }
        read_block_header(&header, pb);
    }
    digiChrome->storedHeader.type = DIGICHROME_TYPE_NOTSET;

    if (avio_feof(pb))
        return AVERROR_EOF;

    switch (header.type) {
        case DIGICHROME_TYPE_COMBINED:
            if (digiChrome->remainingCombinedSize > 0) {
                av_log(s, AV_LOG_ERROR, "nested combined blocks\n");
                return AVERROR_INVALIDDATA;
            }
            digiChrome->remainingCombinedSize = header.size;
            return read_packet(s, pkt);

        case DIGICHROME_TYPE_VIDEO:
            if (digiChrome->remainingCombinedSize > 0) {
                digiChrome->remainingCombinedSize -= DIGICHROME_SECTION_SIZE + header.size;
                if (digiChrome->remainingCombinedSize < 0) {
                    av_log(s, AV_LOG_ERROR, "combined block is too small for contained video block\n");
                    return AVERROR_INVALIDDATA;
                }
            }

            int ret;
            if ((ret = av_get_packet(pb, pkt, header.size)) < 0)
                return ret;

            pkt->stream_index = digiChrome->videoStreamIndices[header.track];
            if (pkt->stream_index < 0)
                pkt->stream_index = create_video_stream(s, header.track);
            if (pkt->stream_index < 0)
                return pkt->stream_index;

            pkt->pts = digiChrome->nextTimestamp;
            pkt->duration = try_read_audio_duration(s, header.track);

            // without audio we just assume the default framerate
            if (pkt->duration < 0)
            {
                pkt->duration = DIGICHROME_DEFAULT_TICKSPERFRAME;
                digiChrome->nextTimestamp += pkt->duration;
            }
            return 0;

        case DIGICHROME_TYPE_AUDIO:
            if (digiChrome->remainingCombinedSize > 0) {
                digiChrome->remainingCombinedSize -= DIGICHROME_SECTION_SIZE + header.size;
                if (digiChrome->remainingCombinedSize < 0) {
                    av_log(s, AV_LOG_ERROR, "combined block is too small for contained audio block\n");
                    return AVERROR_INVALIDDATA;
                }
            }

            avio_skip(pb, DIGICHROME_AUDIO_HEADER_SIZE);
            if ((ret = av_get_packet(pb, pkt, header.size - DIGICHROME_AUDIO_HEADER_SIZE)) < 0)
                return ret;

            pkt->stream_index = digiChrome->audioStreamIndices[header.track];
            if (pkt->stream_index < 0)
                pkt->stream_index = create_audio_stream(s, header.track);
            if (pkt->stream_index < 0)
                return pkt->stream_index;

            pkt->pts = digiChrome->nextTimestamp;
            pkt->duration = pkt->size;
            digiChrome->nextTimestamp += pkt->duration;
            return 0;

        default:
            av_log(s, AV_LOG_ERROR, "unknown block (hex = %x)\n", header.type);
            return AVERROR_INVALIDDATA;
    }
}

const AVInputFormat ff_digichrome_demuxer = {
    .name           = "digichrome",
    .long_name      = NULL_IF_CONFIG_SMALL("DigiChrome"),
    .priv_data_size = sizeof(DigiChromeDemuxContext),
    .read_probe     = probe,
    .read_header    = read_header,
    .read_packet    = read_packet,
    .flags          = AVFMT_SHOW_IDS | AVFMT_VARIABLE_FPS
};
