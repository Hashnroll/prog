#include <stdio.h>
#include <stdlib.h>
#include <time.h>

const int = 20;
void init_array(int* a) {
  srand(time(NULL)); //generating randomness with changing seed
  for (int i = 0; i < n; ++i) {
    a[i] = rand() % 100;
  }
}

void print_array(int* a) {
  for (int i = 0; i < n; ++i)
    printf("%d ", a[i]);
  printf("\n");
}

//merge routine
void merge(int* a, int p, int q, int r) {
  int n1 = q - p + 1;
  int n2 = r - q;
  int first[n1], second[n2];
  for (int i = 0; i < n1; ++i) {
    first[i] = a[p + i];
  }
  for (int j = 0; j < n2; ++j) {
    second[j] = a[q + 1 + j];
  }
  int i = 0; int j = 0;
  int k = p;
  while (k <= r) {
    if (i == n1) {
      while (j < n2) {
	a[k++] = second[j++];
      }
    }
    else if (j == n2) {
      while (i < n1) {
	a[k++] = first[i++];
      }
    }
    else if (first[i] <= second[j]) {
      a[k++]=first[i];
      i++;
    }
    else {
      a[k++]=second[j];
      j++;
    }
  }
}

//merge sort - recursive procedure
void merge_sort(int* a, int p, int r) {
  if (p < r) {
    int q = (p + r) / 2;
    merge_sort(a, p, q);
    merge_sort(a, q + 1, r);
    merge(a, p, q, r);
  }
}

int main(int argc, char* argv) {
  int a[n];
  init_array(a);
  print_array(a);
  merge_sort(a, 0, n-1);
  print_array(a);
  return 0;
}
