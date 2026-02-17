#include "miniaudio.h"

void* hs_ma_device_get_user_data(ma_device* pDevice)
{
    return pDevice ? pDevice->pUserData : NULL;
}
