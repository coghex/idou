#include <stdlib.h>
#include "miniaudio.h"

ma_result hs_ma_decode_file_f32(
    const char* path,
    ma_uint32 targetChannels,
    float** outSamples,
    ma_uint64* outFrames,
    ma_uint32* outChannels,
    ma_uint32* outSampleRate)
{
    if (!path || !outSamples || !outFrames || !outChannels || !outSampleRate) {
        return MA_INVALID_ARGS;
    }

    *outSamples = NULL;
    *outFrames = 0;
    *outChannels = 0;
    *outSampleRate = 0;

    ma_decoder_config cfg = ma_decoder_config_init(ma_format_f32, targetChannels, 48000);
    ma_decoder dec;
    ma_result r = ma_decoder_init_file(path, &cfg, &dec);
    if (r != MA_SUCCESS) {
        return r;
    }

    ma_uint32 channels = dec.outputChannels;
    ma_uint32 sampleRate = dec.outputSampleRate;

    ma_uint64 capacityFrames = 4096;
    ma_uint64 totalFrames = 0;
    ma_uint64 maxFrames = ((ma_uint64)SIZE_MAX / sizeof(float)) / channels;
    if (capacityFrames > maxFrames) {
        capacityFrames = maxFrames;
    }

    if (channels == 0 || capacityFrames == 0) {
        ma_decoder_uninit(&dec);
        return MA_INVALID_FILE;
    }

    float* samples = (float*)malloc((size_t)(capacityFrames * channels) * sizeof(float));
    if (!samples) {
        ma_decoder_uninit(&dec);
        return MA_OUT_OF_MEMORY;
    }

    for (;;) {
        if (totalFrames >= capacityFrames) {
            if (capacityFrames >= maxFrames) {
                free(samples);
                ma_decoder_uninit(&dec);
                return MA_OUT_OF_MEMORY;
            }
            ma_uint64 grown = capacityFrames * 2;
            if (grown < capacityFrames || grown > maxFrames) {
                grown = maxFrames;
            }
            float* grownPtr = (float*)realloc(samples, (size_t)(grown * channels) * sizeof(float));
            if (!grownPtr) {
                free(samples);
                ma_decoder_uninit(&dec);
                return MA_OUT_OF_MEMORY;
            }
            samples = grownPtr;
            capacityFrames = grown;
        }

        ma_uint64 framesAvail = capacityFrames - totalFrames;
        ma_uint64 framesRead = 0;
        ma_result readResult = ma_decoder_read_pcm_frames(
            &dec,
            samples + (totalFrames * channels),
            framesAvail,
            &framesRead);

        if (readResult != MA_SUCCESS && readResult != MA_AT_END) {
            free(samples);
            ma_decoder_uninit(&dec);
            return readResult;
        }

        if (framesRead == 0) {
            break;
        }
        totalFrames += framesRead;
    }

    ma_decoder_uninit(&dec);

    if (totalFrames == 0) {
        free(samples);
        return MA_INVALID_FILE;
    }

    float* exactPtr = (float*)realloc(samples, (size_t)(totalFrames * channels) * sizeof(float));
    if (exactPtr) {
        samples = exactPtr;
    }

    *outSamples = samples;
    *outFrames = totalFrames;
    *outChannels = channels;
    *outSampleRate = sampleRate;
    return MA_SUCCESS;
}

void hs_ma_free_decoded_audio(float* samples)
{
    free(samples);
}
