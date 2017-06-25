#include <iostream>
#include <ctime>
#include "tree.h"

using namespace std;

int main()
{
	BinaryTree<int> b;
	srand(time(0));

	for (int i = 0; i < 10; ++i)
	{
		b.insert(rand()%100);
	}
	b.display();
	cout << endl;
	cout << "Kol-vo elementov = " << b.size() << endl;

	system("pause");
	return 0;
}