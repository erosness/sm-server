
// for read/write
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>


// for open flags
#include <fcntl.h>


#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define MIN(a,b) (((a) < (b)) ? (a) : (b))


/*
 * This is the i2c-adapter on which the DSP device can be found. 
 */
#define DSP_DEVICE "/dev/i2c-2"
// The dsp adapter should be configured to run as slave 0x68,
// which, when using 7 bit addressing, becomes 0x34.
#define DSP_ADDR 0x34

//#include "log.h"
#define LOGI(...) fprintf(stderr, __VA_ARGS__)

// taken from linux/i2c-dev.h:
// which is not present in android-build system
#define I2C_SLAVE 0x0703


// Picked out from Sigma-file
// TODO: read these from Sigma-exported `param` files
#define PARAM_ADR_VOL  0x4B
#define PARAM_ADR_STEP 0x4C


/**
 * current file descriptor (normally, a file handle to /dev/i2c-0
 */
int _dsp_fd = 0;

int wr(int fd, const unsigned char* buffer, int len) 
{
  int wrote = write(fd, buffer, len);

  int i;
  char tmp [64];

  char msg [1024];
  sprintf(msg, "*** DSP write (len %d): ", len);

  for(i = 0 ; i < len ; i++) {
    sprintf(tmp, "%02X ", buffer[i]);
    strcat(msg, tmp);
  }

  LOGI("%s, wrote: %d bytes\n", msg, wrote);
  return wrote;
}

int dsp_write(const void* buffer, int len) 
{
  return wr(_dsp_fd, buffer, len);
}

int dsp_mute(const char muted) 
{
  int wrote = 0;
  // L might actually be R, don't know
  const char mute[] = {0x00, 0x53, 0x00, (muted ? 0x00 : 0x80), 0x00, 0x00};


  wrote = wr(_dsp_fd, mute, sizeof (mute));
  if(wrote <= 0) return wrote;
  
  return wrote;
}

int dsp_vol2hwvol(const int volume) 
{
  // TODO: convert from 0 ~ 100 to 0 ~ 0x80
  return MIN((MAX(volume, 0) * (0x8000 / 100)) / 0xff, 0x80);
}

int dsp_vol(const char volume) 
{
  const char volhw = dsp_vol2hwvol(volume);

  // using safeload write sequence for glitch-removal
  // TODO: make nice safeload-write function

  // TODO: support two-byte addresses:
  const char vol1[] =  {0x08, 0x10, 0x00, /*data:*/    0x00, volhw, 0x00, 0x00};
  const char vol2[] =  {0x08, 0x15,       /*address:*/ 0x00, PARAM_ADR_VOL}; 

  wr(_dsp_fd, vol1, sizeof(vol1));
  wr(_dsp_fd, vol2, sizeof(vol2));

  const char step1[] = {0x08, 0x11, 0x00, /*data:*/    0x00,  0x00, 0x08, 0x00};
  const char step2[] = {0x00, 0x16,       /*address:*/ 0x00, PARAM_ADR_STEP};

  wr(_dsp_fd, step1, sizeof(step1));
  wr(_dsp_fd, step2, sizeof(step2));

  const char flush[] = {0x08, 0x1C, 0x00, 0x3C};
  wr(_dsp_fd, flush, sizeof(flush));


  return 0;
}


int dsp_init() 
{
  if(_dsp_fd != 0) return -1;

  if ((_dsp_fd = open(DSP_DEVICE, O_RDWR)) < 0) {
    LOGI("Failed to open the bus.\n");
    /* TODO ERROR HANDLING; you can check errno to see what went wrong */
    return -20;
  }

  LOGI("%s opened (%d)\n", DSP_DEVICE, _dsp_fd);

  if (ioctl(_dsp_fd, I2C_SLAVE, DSP_ADDR) < 0) {
    LOGI("Failed to acquire bus access and/or talk to slave.\n");
    /* TODO ERROR HANDLING; you can check errno to see what went wrong */
    return -30;
  }

  LOGI("dsp %s (slave %d) initialized successfully\n", DSP_DEVICE, DSP_ADDR);
  return 0;
}

int dsp_close() 
{
  if(_dsp_fd != 0)
    return close(_dsp_fd);
  else
    return -1;
}
