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

#include "avformat.h"
#include "demux.h"
#include "internal.h"

#define DIGICHROME_MAX_TRACKS 256
#define DIGICHROME_SECTION_SIZE 4
#define DIGICHROME_AUDIO_SAMPLE_RATE 19800
#define DIGICHROME_DEFAULT_TICKSPERFRAME 1577 // roughly 12.5 FPS
#define DIGICHROME_AUDIO_HEADER_SIZE 9

enum DigiChromePacketType {
    DIGICHROME_TYPE_VIDEO = 0x81,
    DIGICHROME_TYPE_AUDIO = 0xA2,
    DIGICHROME_TYPE_COMBINED = 0xF1
};

typedef struct DigiChromeDemuxContext {
    int remainingCombinedSize;
    int64_t prevTimestamp;
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
    digiChrome->prevTimestamp = -1;
    digiChrome->nextTimestamp = 0;
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
    st->codecpar->codec_id = 0x234234234;
    avpriv_set_pts_info(st, 64, DIGICHROME_AUDIO_SAMPLE_RATE, 1);
    st->avg_frame_rate.num = DIGICHROME_DEFAULT_TICKSPERFRAME;
    st->avg_frame_rate.den = DIGICHROME_AUDIO_SAMPLE_RATE;
    
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
    st->codecpar->codec_id = 0x234234234;
    st->codecpar->ch_layout = (AVChannelLayout)AV_CHANNEL_LAYOUT_MONO;
    st->codecpar->bits_per_coded_sample = 8;
    st->codecpar->sample_rate = DIGICHROME_AUDIO_SAMPLE_RATE;
    avpriv_set_pts_info(st, 64, DIGICHROME_AUDIO_SAMPLE_RATE, 1);
    st->avg_frame_rate.num = DIGICHROME_DEFAULT_TICKSPERFRAME;
    st->avg_frame_rate.den = DIGICHROME_AUDIO_SAMPLE_RATE;

    DigiChromeDemuxContext *digiChrome = s->priv_data;
    digiChrome->audioStreamIndices[track] = st->index;
    return st->index;
}

static int read_packet(AVFormatContext *s, AVPacket *pkt)
{
    AVIOContext *pb = s->pb;
    DigiChromeDemuxContext *digiChrome = s->priv_data;

    if (digiChrome->remainingCombinedSize < DIGICHROME_SECTION_SIZE) {
        avio_skip(pb, digiChrome->remainingCombinedSize);
        digiChrome->remainingCombinedSize = 0;
    }
    int type = avio_r8(pb);
    int track = avio_r8(pb);
    int size = avio_rl16(pb);

    switch (type) {
        case DIGICHROME_TYPE_COMBINED:
            if (digiChrome->remainingCombinedSize > 0) {
                av_log(s, AV_LOG_ERROR, "nested combined blocks\n");
                return AVERROR_INVALIDDATA;
            }
            digiChrome->remainingCombinedSize = size;
            return read_packet(s, pkt);

        case DIGICHROME_TYPE_VIDEO:
            if (digiChrome->remainingCombinedSize > 0) {
                digiChrome->remainingCombinedSize -= DIGICHROME_SECTION_SIZE + size;
                if (digiChrome->remainingCombinedSize < 0) {
                    av_log(s, AV_LOG_ERROR, "combined block is too small for contained video block\n");
                    return AVERROR_INVALIDDATA;
                }
            }

            pkt->stream_index = digiChrome->videoStreamIndices[track];
            if (pkt->stream_index < 0)
                pkt->stream_index = create_video_stream(s, track);
            
            // without audio we assume default framerate
            if (digiChrome->prevTimestamp == digiChrome->nextTimestamp)
                digiChrome->nextTimestamp += DIGICHROME_DEFAULT_TICKSPERFRAME;
            pkt->pts = digiChrome->nextTimestamp;
            digiChrome->prevTimestamp = digiChrome->nextTimestamp;

            // usually audio packets come after video packets so we cannot set
            // duration at this point

            return av_get_packet(pb, pkt, size);

        case DIGICHROME_TYPE_AUDIO:
            if (digiChrome->remainingCombinedSize > 0) {
                digiChrome->remainingCombinedSize -= DIGICHROME_SECTION_SIZE + size;
                if (digiChrome->remainingCombinedSize < 0) {
                    av_log(s, AV_LOG_ERROR, "combined block is too small for contained audio block\n");
                    return AVERROR_INVALIDDATA;
                }
            }

            pkt->stream_index = digiChrome->audioStreamIndices[track];
            if (pkt->stream_index < 0)
                pkt->stream_index = create_audio_stream(s, track);

            pkt->pts = digiChrome->nextTimestamp;
            pkt->duration = size - DIGICHROME_AUDIO_HEADER_SIZE;
            digiChrome->nextTimestamp += pkt->duration;

            return av_get_packet(pb, pkt, size);

        default:
            av_log(s, AV_LOG_ERROR, "unknown block (hex = %x)\n", type);
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
