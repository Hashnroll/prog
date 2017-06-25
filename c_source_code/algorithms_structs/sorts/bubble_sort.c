#include <stdio.h>


const int n = 5;

void swap(int *a, int *b) {
  int temp;
  temp=*a;
  *a=*b;
  *b=temp;
}
	  
int main() {
  int array[n];
  //initialization of array
  for (int i=0; i<n; ++i)
    array[i]=100-i;
  //printing of array
  for (int i=0; i<n; ++i)
    printf("%d ", array[i]);
  printf("\n");
  //sorting of array
  for (int i=1; i<n; ++i)
    for (int j=n-1; j>=i; --j)
      if (array[j-1]>array[j])
	swap(&array[j-1],&array[j]);
  //printing of array
  for (int i=0; i<n; ++i)
    printf("%d ", array[i]);
  printf("\n");
  return 0;
}
