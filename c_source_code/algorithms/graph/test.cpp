#include <iostream>
#include <map>
#include <string>
#include "graph.h"

using namespace std;

int main() {
	graph g1, g2;

	g1.input();
	cout<<endl;
	g2.input();
	cout<<endl;

	cout << "Первый граф: " << endl;
	g1.print();
	cout<<endl;

	cout << "Второй граф: " << endl;
	g2.print();
	cout<<endl;

	graph g3;
	g3 = inter_(g1, g2);

	cout << "Пересечение: " << endl;
	g3.print();
	cout<<endl;

	graph g4;
	g4 = union_(g1, g2);

	cout << "Объединение: " << endl;
	g4.print();
	cout<<endl;

	system("pause");
  return 0;
}
