/*
 * DigiChrome audio decoder
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

#include "libavutil/common.h"
#include "avcodec.h"
#include "bytestream.h"
#include "codec_internal.h"
#include "internal.h"

#define DIGICHROME_CATEGORY_8x8 4
#define DIGICHROME_CATEGORY_8x4 2
#define DIGICHROME_TYPE_COPYFULL 91
#define DIGICHROME_TYPE_COPYHALF 92

// category 0 and 1 are treated identically
static const uint8_t Categories[] =
{
    0,   4,   0,   4,   4,   4,   4,   4,
    2,   1,   1,   1,   4,   4,   4,   4,
    2,   2,   1,   1,   1,   2,   1,   2,
    1,   1,   2,   2,   1,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,
    1,   1,   1,   1,   1,   1,   1,   1,
    1,   1,   1,   1,   1,   1,   1,   1,
    4,   4,   4,   4,   4,   4,   4,   4,
    4,   4,   4,   4,   4,   4,   4,   4,
    4,   4,   4,   4,   4,   4,   4,   4,
    4,   4,   4,   4,   2,   1,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0
};

// Depending on the category this is an index to a fixed pattern or a color offset
static const uint8_t Indices[] =
{
    0,   0,   0,   1,   2,   3,   4,   5,
    0,   0,   1,   2,   6,   7,   8,   9,
    1,   2,   3,   4,   5,   3,   6,   4,
    7,   8,   5,   6,   9,   7,   8,   9,
    10,  11,  12,  13,  14,  15,  16,  17,
    18,  19,  20,  21,  22,  23,  24,  25,
    10,  11,  12,  13,  14,  15,  16,  17,
    18,  19,  20,  21,  22,  23,  24,  25,
    2,   3,   4,   5,   6,   7,   8,   9,
    10,  11,  12,  13,  14,  15,  16,  17,
    18,  19,  20,  21,  22,  23,  24,  25,
    1,   1,   1,   0,   0,   0,   0,   0,
    0,   1,   0,   1,   1,   1,   1,   1,
    1,   4,   4,   5,   1,   1,   1,   1,
    2,   3,   3,   4,   4,   5,   5,   4,
    3,   2,   5,   4,   1,   4,   4,   3
};

static const uint8_t ColorPairCount[] =
{
    0,   1,   0,   1,   1,   1,   1,   1,
    1,   4,   4,   5,   1,   1,   1,   1,
    2,   3,   3,   4,   4,   5,   5,   4,
    3,   2,   5,   4,   1,   4,   4,   3,
    8,   7,   7,   6,   7,   6,   6,   5,
    7,   6,   6,   5,   6,   5,   5,   4,
    8,   7,   7,   6,   7,   6,   6,   5,
    7,   6,   6,   5,   6,   5,   5,   4,
    1,   1,   1,   1,   1,   1,   1,   1,
    1,   1,   1,   1,   1,   1,   1,   1,
    1,   1,   1,   1,   1,   1,   1,   1,
    1,   1,   1,   0,   0,   0,   0,   0
};

static const uint64_t FixedPatterns[] =
{
    0xAA55AA55AA55AA55,
    0x0000AA55AA55FFFF,
    0x0A052A15AB57AF5F,
    0x2B172B172B172B17,
    0xAF57AB552A552A05,
    0xFFFFAA55AA550000,
    0xFAF5EAD5A854A050,
    0xE8D4E8D4E8D4E8D4,
    0xA054AA54AAD5EAF5,
    0x00000000AA55AA55,
    0x000102050A152A55,
    0x0A050A050A050A05,
    0x2A550A1502050001,
    0xAA55AA5500000000,
    0xAA54A850A0408000,
    0xA050A050A050A050,
    0x8000A040A850AA54,
    0xAA55AA55FFFFFFFF,
    0xAB55AF57BF5FFF7F,
    0xAF5FAF5FAF5FAF5F,
    0xFF7FBF5FAF57AB55,
    0xFFFFFFFFAA55AA55,
    0xFEFFFAFDEAF5AAD5,
    0xFAF5FAF5FAF5FAF5,
    0xAAD5EAF5FAFDFEFF
};

static const uint8_t ColorOffsets8x4[] =
{
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   2,   2,   0,   0,   2,   2,
    0,   0,   2,   2,   0,   0,   4,   4,
    0,   2,   4,   4,   6,   8,   4,   4,
    0,   2,   4,   4,   6,   6,   4,   4,
    0,   0,   2,   4,   0,   0,   6,   8,
    0,   0,   2,   4,   0,   0,   6,   6,
    0,   0,   2,   2,   4,   6,   2,   2,
    0,   0,   2,   2,   0,   0,   4,   6,
    0,   0,   2,   2,   4,   4,   2,   2,
    0,   2,   4,   6,   8,  10,  12,  14,
    0,   2,   4,   6,   8,  10,  12,  12,
    0,   2,   4,   6,   8,   8,  10,  12,
    0,   2,   4,   6,   8,   8,  10,  10,
    0,   2,   4,   4,   6,   8,  10,  12,
    0,   2,   4,   4,   6,   8,  10,  10,
    0,   2,   4,   4,   6,   6,   8,  10,
    0,   2,   4,   4,   6,   6,   8,   8,
    0,   0,   2,   4,   6,   8,  10,  12,
    0,   0,   2,   4,   6,   8,  10,  10,
    0,   0,   2,   4,   6,   6,   8,  10,
    0,   0,   2,   4,   6,   6,   8,   8,
    0,   0,   2,   2,   4,   6,   8,  10,
    0,   0,   2,   2,   4,   6,   8,   8,
    0,   0,   2,   2,   4,   4,   6,   8,
    0,   0,   2,   2,   4,   4,   6,   6
};

static const uint8_t ColorOffsets4x4[] =
{
    0,   2,   4,   2,   6,   6,   6,   6,
    0,   2,   0,   4,   6,   6,   6,   6,
    0,   2,   4,   6,   8,   8,   8,   8,
    0,   2,   0,   2,   4,   4,   4,   4,
    0,   0,   0,   0,   2,   4,   6,   4,
    0,   0,   0,   0,   2,   4,   2,   6,
    0,   0,   0,   0,   2,   4,   6,   8,
    0,   0,   0,   0,   2,   4,   2,   4,
    0,   0,   0,   0,   2,   2,   2,   2,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   2,   4,   6,   8,  10,  12,  14,
    0,   2,   4,   6,   8,  10,  12,  10,
    0,   2,   4,   6,   8,  10,   8,  12,
    0,   2,   4,   6,   8,  10,   8,  10,
    0,   2,   4,   2,   6,   8,  10,  12,
    0,   2,   4,   2,   6,   8,  10,   8,
    0,   2,   4,   2,   6,   8,   6,  10,
    0,   2,   4,   2,   6,   8,   6,   8,
    0,   2,   0,   4,   6,   8,  10,  12,
    0,   2,   0,   4,   6,   8,  10,   8,
    0,   2,   0,   4,   6,   8,   6,  10,
    0,   2,   0,   4,   6,   8,   6,   8,
    0,   2,   0,   2,   4,   6,   8,  10,
    0,   2,   0,   2,   4,   6,   8,   6,
    0,   2,   0,   2,   4,   6,   4,   8,
    0,   2,   0,   2,   4,   6,   4,   6
};

typedef struct DigiChromeDecoder {
    AVFrame *previousFrame, *curFrame;
    GetByteContext data;
    int prevWidth, prevHeight, curWidth, curHeight; // in blocks
} DigiChromeDecoder;

static int decode_init(AVCodecContext *avctx)
{
    avctx->pix_fmt = AV_PIX_FMT_PAL8;
    return 0;
}

static int decode_close(AVCodecContext *avctx)
{
    DigiChromeDecoder *digiChrome = avctx->priv_data;
    if (digiChrome->previousFrame)
        av_frame_unref(digiChrome->previousFrame);
    if (digiChrome->curFrame)
        av_frame_unref(digiChrome->curFrame);
    return 0;
}

static void copy_clipping(int dstBlock, int dstBlockSize, int srcBlockSize,
                          int *copySize, int *outSrcPixel, int *outDstOffset)
{
    int dstPixel = dstBlock * 8;
    int dstSize = dstBlockSize * 8;
    int srcSize = srcBlockSize * 8;
    int dstOff = 0;
    int srcPixel = dstPixel + (srcSize - dstSize) / 2;
    if (srcPixel < 0) {
        dstOff = -srcPixel;
        *copySize += srcPixel;
        srcPixel = 0;
    }
    *copySize = min(*copySize, srcSize - srcPixel);
    *outSrcPixel = srcPixel;
    *outDstOffset = dstOff;
}

static void decode_block_copy(DigiChromeDecoder *digiChrome, int dstX, int dstY,
                              int copyWidth, int copyHeight, uint8_t *out)
{
    int srcX, srcY,dstOffX, dstOffY;
    copy_clipping(dstX, digiChrome->curWidth, digiChrome->prevWidth,
                  &copyWidth, &srcX, &dstOffX);
    copy_clipping(dstY, digiChrome->curHeight, digiChrome->prevHeight,
                  &copyHeight, &srcY, &dstOffY);
    if (copyWidth <= 0 || copyHeight <= 0)
        return;

    int curLinesize = digiChrome->curFrame->linesize[0];
    int prevLinesize = digiChrome->previousFrame->linesize[0];
    out += dstOffX + dstOffY * curLinesize;
    uint8_t *in = digiChrome->previousFrame->data[0] + srcX + srcY * prevLinesize;
    for (int i = 0; i < copyHeight; i++) {
        memcpy(out, in, copyWidth);
        out += curLinesize;
        in += prevLinesize;
    }
}

static const uint8_t *get_colors(DigiChromeDecoder *digiChrome)
{
    GetByteContext *data = &digiChrome->data;
    int colorCount = 2 * ColorPairCount[bytestream2_get_byteu(data)];
    const uint8_t *colors = data->buffer;
    bytestream2_seek(data, colorCount, SEEK_CUR);
    return colors;
}

static void decode_block8x8_pattern(const uint8_t *colors, uint64_t pattern,
                                    int linesize, uint8_t *out)
{
    for (int y = 0; y < 8; y++, out += linesize - 8) {
        for (int x = 0; x < 8; x++, out++) {
            *out = colors[pattern & 1];
            pattern >>= 1;
        }
    }
}

static void decode_block8x8(DigiChromeDecoder *digiChrome, int x, int y,
                            uint8_t *out)
{
    GetByteContext *data = &digiChrome->data;
    int linesize = digiChrome->curFrame->linesize[0];
    uint8_t type = bytestream2_peek_byteu(data);
    uint8_t fixedPatternI = Indices[type];
    if (fixedPatternI > 0) {
        uint64_t pattern = FixedPatterns[fixedPatternI - 1];
        const uint8_t *colors = get_colors(digiChrome);
        decode_block8x8_pattern(colors, pattern, linesize, out);
    }
    else if (type == DIGICHROME_TYPE_COPYFULL) {
        decode_block_copy(digiChrome, x, y, 8, 8, out);
        bytestream2_get_byteu(data);
    }
    else {
        const uint8_t *colors = get_colors(digiChrome);
        uint64_t pattern = bytestream2_get_le64u(data);
        decode_block8x8_pattern(colors, pattern, linesize, out);
    }
}

static void decode_block8x4(DigiChromeDecoder *digiChrome, int x, int y,
                            uint8_t *out)
{
    GetByteContext *data = &digiChrome->data;
    int linesize = digiChrome->curFrame->linesize[0];
    uint8_t type = bytestream2_peek_byteu(data);
    if (type == DIGICHROME_TYPE_COPYHALF)
    {
        decode_block_copy(digiChrome, x, y, 8, 4, out);
        bytestream2_get_byteu(data);
        return;
    }

    const uint8_t *offsets = ColorOffsets8x4 + 8 * Indices[type];
    const uint8_t *colors = get_colors(digiChrome);
    uint32_t pattern = bytestream2_get_le32u(data);
    for (int i = 0; i < 2; i++, offsets += 4) {
        for (int j = 0; j < 2; j++, out += linesize - 8) {
            for (int x = 0; x < 8; x++, out++, pattern >>= 1)
                *out = colors[offsets[x / 2] + (pattern & 1)];
        }
    }
}

static void decode_block4x4(DigiChromeDecoder *digiChrome, int x, int y,
                            uint8_t *out)
{
    GetByteContext *data = &digiChrome->data;
    int linesize = digiChrome->curFrame->linesize[0];
    uint8_t type = bytestream2_peek_byteu(data);
    if (type == DIGICHROME_TYPE_COPYHALF)
    {
        decode_block_copy(digiChrome, x, y, 4, 4, out);
        bytestream2_get_byteu(data);
        return;
    }

    const uint8_t *offsets = ColorOffsets4x4 + 8 * Indices[type];
    const uint8_t *colors = get_colors(digiChrome);
    uint16_t pattern = bytestream2_get_le32u(data);
    for (int y = 0; y < 4; y++, out += linesize - 8, offsets += 2) {
        for (int x = 0; x < 8; x++, out++, pattern >>= 1)
            *out = colors[offsets[x / 2] + (pattern & 1)];
    }
}

static void decode_block(DigiChromeDecoder *digiChrome, int x, int y,
                        uint8_t *out)
{
    GetByteContext *data = &digiChrome->data;
    uint8_t category = Categories[bytestream2_peek_byteu(data)];
    if (category == DIGICHROME_CATEGORY_8x8) {
        decode_block8x8(digiChrome, x, y, out);
        return;
    }

    if (category == DIGICHROME_CATEGORY_8x4)
        decode_block8x4(digiChrome, x, y, out);
    else {
        decode_block4x4(digiChrome, x, y, out);
        decode_block4x4(digiChrome, x, y, out + 4);
    }

    out += digiChrome->curFrame->linesize[0] * 4;
    category = bytestream2_peek_byteu(data);
    if (category == DIGICHROME_CATEGORY_8x4)
        decode_block8x4(digiChrome, x, y, out);
    else {
        decode_block4x4(digiChrome, x, y, out);
        decode_block4x4(digiChrome, x, y, out + 4);
    }
}

static int decode_blocks(DigiChromeDecoder *digiChrome)
{
    GetByteContext *data = &digiChrome->data;
    int repeatCount = 0, repeatOffset;
    uint8_t *outRow = digiChrome->curFrame->data[0];

    for (int y = 0; y < digiChrome->curHeight; y++) {
        uint8_t *out = outRow;
        for (int x = 0; x < digiChrome->curWidth; x++, out += 8) {
            if (repeatCount > 0) {
                repeatCount--;
                bytestream2_seek(data, repeatOffset, SEEK_SET);
            }
            else if (bytestream2_peek_byteu(data) & 0x80) {
                repeatCount = (bytestream2_get_byteu(data) & 0x7F) + 1;
                repeatOffset = bytestream2_tell(data);
            }

            if (bytestream2_get_bytes_left(data) == 0)
                return AVERROR_INVALIDDATA;
            decode_block(digiChrome, x, y, out);
        }
        outRow += digiChrome->curFrame->linesize[0];
    }
    return 0;
}

static int decode_frame(AVCodecContext *avctx, AVFrame *rframe,
                        int *got_frame, AVPacket *avpkt)
{
    DigiChromeDecoder *digiChrome = avctx->priv_data;
    digiChrome->prevWidth = digiChrome->curWidth;
    digiChrome->prevHeight = digiChrome->curHeight;
    AVFrame* tmpFrame = digiChrome->previousFrame;
    digiChrome->previousFrame = digiChrome->curFrame;
    digiChrome->curFrame = tmpFrame;

    GetByteContext *data = &digiChrome->data;
    bytestream2_init(data, avpkt->data, avpkt->size);
    bytestream2_skip(data, 5);

    int paletteCount = bytestream2_get_byte(data);
    digiChrome->curWidth = bytestream2_get_byte(data);
    digiChrome->curHeight = bytestream2_get_byte(data);
    avctx->width = digiChrome->curWidth * 8;
    avctx->height = digiChrome->curHeight * 8;
    
    // ff_reget_buffer warns about changed dimensions, so recreate manually in these cases
    if (digiChrome->curFrame->data[0] &&
        (digiChrome->curWidth != digiChrome->prevWidth ||
        digiChrome->curHeight != digiChrome->prevHeight)) {
        av_frame_unref(digiChrome->curFrame);
        digiChrome->curFrame = NULL;
    }
    int ret;
    if (!digiChrome->curFrame && !(digiChrome->curFrame = av_frame_alloc()))
        return AVERROR(ENOMEM);
    if ((ret = ff_reget_buffer(avctx, digiChrome->curFrame, 0)) < 0)
        return ret;
    
    uint8_t *palette = (uint8_t*)digiChrome->curFrame->data[1];
    for (int i = 0; i < paletteCount; i++, palette += 4) {
        unsigned int color = bytestream2_get_le16u(data);
        palette[0] = (color >> 0) & 0x1F;
        palette[1] = (color >> 5) & 0x1F;
        palette[2] = (color >> 10) & 0x1F;
        palette[3] = 0xFF;
    }

    if ((ret = decode_blocks(digiChrome)) < 0)
        return ret;
    *got_frame = 1;
    return bytestream2_tell(data);
}

const FFCodec ff_digichrome_decoder = {
    .p.name = "digichrome",
    .p.long_name = NULL_IF_CONFIG_SMALL("DigiChrome"),
    .p.type = AVMEDIA_TYPE_VIDEO,
    .p.id = AV_CODEC_ID_DIGICHROME,
    .priv_data_size = sizeof(DigiChromeDecoder),
    .init = decode_init,
    .close = decode_close,
    FF_CODEC_DECODE_CB(decode_frame),
    .p.capabilities = AV_CODEC_CAP_DR1
};
