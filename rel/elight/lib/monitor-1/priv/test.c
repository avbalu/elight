#include <stdlib.h>
typedef   union {
    u_short i;
    u_char buf[2];
} U;
main()
{
  U u;
  u.i = 1500;
  printf("%d %d \n",u.buf[0],u.buf[1]);
  u.i = htons(1500);
  printf("%d %d \n",u.buf[0],u.buf[1]);
  
}
