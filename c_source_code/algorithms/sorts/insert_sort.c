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
  for (int i=1; i<=n-1; ++i) {
    int key=array[i];
    int j=i-1;
    while (array[j]>key && j>=0) {
      array[j+1]=array[j];
      j--;
    }
    array[j+1]=key;
  }  
  //printing of array
  for (int i=0; i<n; ++i)
    printf("%d ", array[i]);
  printf("\n");
  return 0;
}
