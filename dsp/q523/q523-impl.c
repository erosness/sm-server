
#include <stdio.h>

typedef unsigned char byte;

// Taken from SigmaStudio 3.8's help file
// "Using Sigma Studio -> System Implementation"
// Microcontroller code - functions: convert a float number to 5.23 format
void To523(byte* param_hex, float param_dec)
{
  long param223;
  long param227;

  //multiply decimal number by 2^23
  param223 = param_dec * (1 << 23);
  //convert to positive binary
  param227 = param223 + (1 << 27);

  param_hex[3]=(byte)param227;  //get byte 3 (LSBs) of parameter value
  param_hex[2]=(byte)(param227>>8); //get byte 2 of parameter value
  param_hex[1]=(byte)(param227>>16); //get byte 1 of parameter value
  //get byte 0 (MSBs) of parameter value
  param_hex[0]=(byte)(param227>>24);
  //invert sign bit to get correct sign

  param_hex[0] = (param_hex[0] ^ 0x08) | 
    // (patch: sigmatel always outputs 0xf on the 4 first bits
    // these are ignored in q523 but for testing purposes, it's easier
    // to use the same convension as Sigma Studio)
    (param_dec < 0 ? 0xF0 : 0x00) ;

  //param_hex[1] = 0xef;
}
