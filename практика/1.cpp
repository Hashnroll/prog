#include <iostream>
using namespace std;

template <typename T>
T less_number(T a, T b) {
  if (a > b)
    return b;
  else
    return a;
}

int main() {
  int ia, ib;
  double da, db;
  char ca, cb;
  
  cout << "For int" << endl;
  cout << "Enter the numbers: ";
  cin >> ia >> ib;
  cout << less_number(ia, ib) << endl << endl;
  
  cout << "For double" << endl;
  cout << "Enter the numbers: ";
  cin >> da >> db;
  cout << less_number(da, db) << endl << endl;
  
  cout << "For char" << endl;
  cout << "Enter the numbers: ";
  cin >> ca >> cb;
  cout << less_number(ca, cb) << endl << endl;
  
  return 0;
}
