/*
Utility functions for Photon.
Based on MicroC

Authors:
Phu D Pham (pdp2121)
*/

#include <stdio.h>
#include <math.h>

int get_max(int x, int y) {
  if (x >= y) {
    return x;
  } else {
    return y;
  }
} 

int get_min(int x, int y) {
  if (x <= y){
    return x;
  } else {
    return y;
  }
}
double get_sqrt(double x) {
  return sqrt(x);
}

#ifdef BUILD_TEST
int main()
{
  char s[] = "HELLO WORLD09AZ";
  char *c;
  for ( c = s ; *c ; c++) printbig(*c);
}
#endif
