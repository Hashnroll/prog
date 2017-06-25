//#include "list_iterator.h"
#include <iostream>
using namespace std;

template <class T>
class double_list
{
private:
	struct node
	{
		T value;
		node* prev;
		node* next;
	};

	node *_head,
		*_tail;

	void Remove(node* link)
	{
		if (!link)
			return;
		Remove(link->next);
		delete link;
	}

public:
	class Iterator
	{
	public:
		Iterator(const Iterator &it) : //����������� �����������.
			_list(it._list),
			_pntr(it._pntr)
		{}
		Iterator(const Iterator &it, int index) :
			_list(it._list),
			_pntr(it._pntr)
		{
			while (index--)
				_pntr = _pntr->next;
		}
		Iterator(const double_list *list, node* pntr) : //����������� �� ���������
			_list(list),
			_pntr(pntr)
		{}
		Iterator() :
			_list(nullptr),
			_pntr(nullptr)
		{}
		Iterator &operator=(const Iterator &it)  //�������� ������������
		{
			_list = it._list;
			_pntr = it._pntr;
			return *this;
		}
		Iterator operator++() //���� ���������
		{
			_pntr = _pntr->next;
			Iterator it = *this;
			return it;
		}
		Iterator operator--() //���� ���������
		{
			if (!_pntr)
				_pntr = _list->_tail;
			else
				_pntr = _pntr->prev;
			Iterator it = *this;
			return it;
		}
		Iterator operator++(int) //���� ���������
		{
			Iterator it = *this;
			_pntr = _pntr->next;
			return it;
		}
		Iterator operator--(int) //���� ���������
		{
			Iterator it = *this;
			if (!_pntr)
				_pntr = _list->_tail;
			else
				_pntr = _pntr->prev;
			return it;
		}
		T &operator*()        //�������� ��������������� ������ ����������
		{                    // ������� ����� ���������
			return _pntr->value;    //����� ��� � � stl
		}
		Iterator operator-(int n) {
			Iterator it = *this;
			while (n-- > 0) {
				it--;
			}
			return it;
		}
		Iterator operator+(int n) {
			Iterator it = *this;
			while (n-- > 0) {
				it++;
			}
			return it;
		}
		int index() {
			node* x = _list->_head;
			int res = 0;
			while (x != _pntr) {
				res++;
				x = x->next;
			}
			return res - 1;
		}
		//�������� ��������� ���� ����������
		bool operator == (const Iterator &iterator)
		{
			return (_pntr == iterator._pntr) && (_list == iterator._list);
		}
		bool operator != (const Iterator &iterator)
		{
			return !(*this == iterator);
		}
		bool isNull() {
			return _pntr == nullptr;
		}
	private:
		const double_list *_list; //��������� �� ����, �������� ����������� ������ ��������
		node* _pntr; //��������� ��  �������, ������� "������" ������ ��������
	};

public:
	double_list() :
		_head(nullptr),
		_tail(nullptr)
	{}
	~double_list()
	{
		Remove(_head);
	}
	void InsertBegin(T val)
	{
		node* x = new node();
		x->value = val;
		x->prev = nullptr;
		x->next = _head;
		if (!isEmpty()) {
			_head->prev = x;
		}
		else
			_tail = x;
		_head = x;
	}
	void InsertEnd(T val)
	{
		node* x = new node();
		x->value = val;
		x->next = nullptr;
		x->prev = _tail;
		if (!isEmpty()) {
			_tail->next = x;
		}
		else
			_head = x;
		_tail = x;
	}
	void RemoveBegin()
	{
		if (isEmpty()) {
			cout << "Error: list is empty." << endl;
			exit(0);
		}
		node *x = _head;
		_head = _head->next;
		_head->prev = nullptr;
		delete x;
	}
	void RemoveEnd()
	{
		if (isEmpty()) {
			cout << "Error: list is empty." << endl;
			exit(0);
		}
		node *x = _tail;
		_tail = _tail->prev;
		_tail->next = nullptr;
		delete x;
	}
	bool isEmpty()
	{
		return _head == nullptr;
	}
	int count()
	{
		node* x = this->first;
		int res = 0;
		while (x) {
			res++;
			x = x->next;
		}
		return res;
	}
	T& operator[](int n)
	{
		node *x = _head;
		while (n-- > 0)
			x = x->next;
		if (x)
			return x->value;
		else {
			cout << "Error: index is out of range." << endl;
			exit(0);
		}
	}
	void copy(const double_list<T> &l)
	{
		_head = new node();      //�������� ������ �������
		_head->value = (l._head)->value;
		_head->prev = nullptr;
		node* x = (l._head)->next;
		node* prevnode = _head;
		while (x) {              //�������� ��� ��������� ��������, ���� x != nullptr
			node* newx = new node();
			newx->value = x->value;
			prevnode->next = newx;
			newx->prev = prevnode;
			prevnode = newx;
			x = x->next;
		}
		prevnode->next = nullptr; //x = nullptr, ������ ����. ������� - last
		_tail = prevnode;
	}
	double_list<T>& operator=(const double_list<T> &l)
	{
		Remove(_head);           //������� ����, �������� ����������� ������ ����
		copy(l);
		return *this;
	}
	Iterator begin()
	{
		Iterator it(this, _head);
		return it;
	}
	Iterator end()
	{
		Iterator it(this, _tail->next);
		return it;
	}
};


bool Condition(int lhs, int rhs) //������� � ������� ������� �� ���������� �������
{
	return (lhs > rhs);
}

template<typename T> void Swap(T &rhs, T &lhs) //�������, ������� ������ ������� ��� ��������.
{
	T tmp = rhs;
	rhs = lhs;
	lhs = tmp;
}

template <class Iterator, typename T>
void qsort(Iterator l, Iterator r, bool(*condition)(T, T))
{
	if (l.index() >= r.index())
		return;
	Iterator mid(l, (r.index() - l.index()) / 2);
	T x = *mid;
	Iterator i = l, j = r;
	while (i.index() <= j.index()) {
		while (*i < x)
			++i;
		while (*j > x)
			--j;
		if (i.index() <= j.index()) {
			Swap(*i, *j);
			++i; --j;
		}
	}
	qsort(l, j, Condition);
	qsort(i, r, Condition);
}

template <typename T, template <typename T> class Container> //������, ������������ ����� ���� � ����������. � ��� ��� ����������
void printContainer(Container<T> &container) //�� ���� ��������� ������ ���������
{
	typename Container<T>::Iterator it = container.begin();  //��������, ����������� �� ������ ����������
	typename Container<T>::Iterator end = container.end();   //��������, ����������� �� ����� ���������� 
	for (; it != end; it++)                                      //�� ��� ���, ���� �� �� ����� �� ����� ����������
		std::cout << *it << ',';                             //������� ��, ��� ������ ���� �������� � �������
	std::cout << std::endl;                                  //��������� �����. ���������� ����� � �.�.
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
  qsort(b,--e, Condition);
  printContainer(l1);
	system("pause");
  return 0;
}
