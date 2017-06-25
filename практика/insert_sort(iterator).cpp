#include "list_iterator.h"

bool Condition(int lhs, int rhs) //Функция с помощью которой мы определяем порядок
{
	return (lhs > rhs);
}

template<typename T> void Swap(T &rhs, T &lhs) //Функция, которая меняет местами два элемента.
{
	T tmp = rhs;
	rhs = lhs;
	lhs = tmp;
}

template <class Iterator, typename T>
void insert_sort(Iterator begin, Iterator end, bool(*condition)(T, T))
{
	Iterator it = begin + 1;
	for (; it != end; ++it) {
		T key = *it;
		Iterator j = it - 1, k = it;
		while (condition(*j, key)) {
			Swap(*k, *j);
			k = j;
			j--;
			if (j.isNull()) {
				break;
			}
		}
		*k = key;
	}
}

int main(){
  double_list<int> l1;
  l1.InsertBegin(1);
  l1.InsertBegin(1);
  l1.RemoveBegin();
  l1.InsertEnd(5);
  l1.InsertEnd(10);
  l1.InsertBegin(9);
	l1.InsertBegin(12);
	l1.InsertBegin(15);
	l1.InsertBegin(17);
	printContainer(l1);
  double_list<int>::Iterator b=l1.begin();
  double_list<int>::Iterator e=l1.end();
  insert_sort(b,e, Condition);
  printContainer(l1);
	system("pause");
  return 0;
}
