#include <iostream>
#include "hashtable.h"
using namespace std;

int main() {
	element<int> x("abc", 10);
	element<int> y("1", 20);
	element<int> z("2", 30);
	element<int> a("abc", 50);
	element<int> b("5", 20);
	element<int> c("2", 30);

	dict<int> m1(10);
	dict<int> m2(15);

	m1.insert(&x);
	m1.insert(&y);
	m1.insert(&z);
	m1.remove(&z); 
	m1.insert(&a);
	m1.insert(&c);

	m2.insert(&x);
	m2.insert(&y);
	m2.insert(&z);
	m2.insert(&a);
	m2.insert(&b);
	m2.insert(&c);

	int eq = (m1 == m2);  //оператор == возвращает количество одинаковых пар "ключ-значение" в таблицах

	cout << "M1: " << endl;
	m1.print();
	cout << endl;

	cout << "M2: " << endl;
	m2.print();
	cout << endl;

	cout << eq << " odinakovi."<< endl;
	system("pause");
	return 0;
}