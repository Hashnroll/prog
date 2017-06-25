#include <iostream>
#include "stack.h"
#include <ctime>
using namespace std;





int main() {
	srand(time(0));

	stack<int> st1(7), st2(10);

	for (int i = 0; i < 7;++i) {
		st1.push(rand()%100 - 50);
	}

	cout << "Stack1: ";
	st1.Print();
	cout << endl;

	int prod = 1;
	while (!st1.isEmpty()) {
		int x = st1.pop();
		if (x < 0) {
			st2.push(x);
			prod *= x;
		}
	}
	cout << "Stack2: ";
	st2.Print();
	cout << endl;
	cout << "Proizvedenie = " << prod<< endl;
	system("pause");
	return 0;
}
