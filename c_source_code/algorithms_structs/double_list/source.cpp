#include "list_iterator.h"
#include <iostream>
using namespace std;

int main(){
	double_list<int> l1;
	for (int i = 1; i <= 20; ++i)
		l1.InsertBegin((26*i - 17) % 100);	
	double_list<int> l2;
	l2 = l1.reverse();
	cout << "L1: ";
	printContainer(l1);
	cout << "L2: ";
	printContainer(l2);
	double_list<int>::Iterator it_l1, it_l2;
	it_l2 = l2.begin();
	for (it_l1 = l1.begin(); it_l1 != l1.end(); it_l1++) {
		if (*it_l1 % 2 == 0) {
			while (it_l2 != l2.end() && *it_l2 % 2 == 0)
				it_l2++;
			*it_l1 = *it_l2;
			it_l2++;
		}
	}
	cout << "L1 posle zameni chetnih na nechetnie: ";
	printContainer(l1);
	system("pause");
  return 0;
}
