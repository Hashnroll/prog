#include <time.h>
#include <stdio.h>

int main() {
  time_t biggest=0x7fffffff;

  printf("biggest = %s \n", asctime(gmtime(&biggest)));
  return 0;
}
