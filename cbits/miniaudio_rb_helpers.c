#include <stdlib.h>
#include <string.h>
#include "miniaudio.h"

/* Wrapper around ma_rb for interleaved float32 frames. Byte-based ma_rb API. */

typedef struct hs_ma_rb {
    ma_rb rb;
    ma_uint32 channels;
    size_t bytesPerFrame;
    void* backingBuffer;
    size_t backingBufferSize;
} hs_ma_rb;

hs_ma_rb* hs_ma_rb_create_f32(ma_uint32 capacityInFrames, ma_uint32 channels)
{
    hs_ma_rb* h = (hs_ma_rb*)malloc(sizeof(hs_ma_rb));
    if (!h) return NULL;

    h->channels = channels;
    h->bytesPerFrame = sizeof(float) * (size_t)channels;
    h->backingBufferSize = h->bytesPerFrame * (size_t)capacityInFrames;
    h->backingBuffer = malloc(h->backingBufferSize);

    if (!h->backingBuffer) {
        free(h);
        return NULL;
    }

    /* Use caller-allocated backing buffer. */
    ma_result r = ma_rb_init(h->backingBufferSize, h->backingBuffer, NULL, &h->rb);
    if (r != MA_SUCCESS) {
        free(h->backingBuffer);
        free(h);
        return NULL;
    }

    return h;
}

void hs_ma_rb_destroy(hs_ma_rb* h)
{
    if (!h) return;
    ma_rb_uninit(&h->rb);

    free(h->backingBuffer);
    free(h);
}

/* Write frames from interleaved float array. Returns frames actually written. */
ma_uint32 hs_ma_rb_write_f32(hs_ma_rb* h, const float* frames, ma_uint32 frameCount)
{
    if (!h) return 0;

    ma_uint32 totalWrittenFrames = 0;

    while (totalWrittenFrames < frameCount) {
        ma_uint32 framesRemaining = frameCount - totalWrittenFrames;
        size_t bytesRequested = (size_t)framesRemaining * h->bytesPerFrame;

        void* pWrite = NULL;
        if (ma_rb_acquire_write(&h->rb, &bytesRequested, &pWrite) != MA_SUCCESS) {
            break;
        }

        if (bytesRequested == 0) {
            /* No contiguous space right now. */
            break;
        }

        memcpy(pWrite,
               (const char*)frames + (size_t)totalWrittenFrames * h->bytesPerFrame,
               bytesRequested);

        ma_rb_commit_write(&h->rb, bytesRequested);

        totalWrittenFrames += (ma_uint32)(bytesRequested / h->bytesPerFrame);
    }

    return totalWrittenFrames;
}

/* Read frames into interleaved float array. Returns frames actually read. */
ma_uint32 hs_ma_rb_read_f32(hs_ma_rb* h, float* outFrames, ma_uint32 frameCount)
{
    if (!h) return 0;

    ma_uint32 totalReadFrames = 0;

    while (totalReadFrames < frameCount) {
        ma_uint32 framesRemaining = frameCount - totalReadFrames;
        size_t bytesRequested = (size_t)framesRemaining * h->bytesPerFrame;

        void* pRead = NULL;
        if (ma_rb_acquire_read(&h->rb, &bytesRequested, &pRead) != MA_SUCCESS) {
            break;
        }

        if (bytesRequested == 0) {
            break;
        }

        memcpy((char*)outFrames + (size_t)totalReadFrames * h->bytesPerFrame,
               pRead,
               bytesRequested);

        ma_rb_commit_read(&h->rb, bytesRequested);

        totalReadFrames += (ma_uint32)(bytesRequested / h->bytesPerFrame);
    }

    return totalReadFrames;
}

/* Available bytes -> available frames. */
ma_uint32 hs_ma_rb_available_read(hs_ma_rb* h)
{
    if (!h) return 0;
    size_t bytesAvail = ma_rb_available_read(&h->rb);
    return (ma_uint32)(bytesAvail / h->bytesPerFrame);
}

/* Available bytes -> available frames (write side). */
ma_uint32 hs_ma_rb_available_write(hs_ma_rb* h)
{
    if (!h) return 0;
    size_t bytesAvail = ma_rb_available_write(&h->rb);
    return (ma_uint32)(bytesAvail / h->bytesPerFrame);
}
