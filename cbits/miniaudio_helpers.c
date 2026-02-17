#include <stdlib.h>
#include "miniaudio.h"

/* Allocate/free opaque handles for Haskell. */
ma_device* hs_ma_device_malloc(void) {
    return (ma_device*)malloc(sizeof(ma_device));
}
void hs_ma_device_free(ma_device* dev) {
    free(dev);
}

/* Heap-allocate a config and fill it for playback. */
ma_device_config* hs_ma_device_config_init_playback(
    ma_format format,
    ma_uint32 channels,
    ma_uint32 sampleRate,
    ma_device_data_proc dataCallback,
    void* pUserData)
{
    ma_device_config* cfg = (ma_device_config*)malloc(sizeof(ma_device_config));
    if (!cfg) return NULL;

    *cfg = ma_device_config_init(ma_device_type_playback);
    cfg->playback.format   = format;
    cfg->playback.channels = channels;
    cfg->sampleRate        = sampleRate;
    cfg->dataCallback      = dataCallback;
    cfg->pUserData         = pUserData;
    return cfg;
}

void hs_ma_device_config_free(ma_device_config* cfg)
{
    free(cfg);
}
