#pragma once

template <typename T>
struct element {
	T value;
	element* next;
};

template<class T>
class stack
{
private:
	element<T> x;
	T *_root,                  //��������� �� ������ �����, ����� ����� ��� ����� ���� �������
		*_pntr;                  //��������� �� ������� �����
	int _maxSize;              //������������ ����������� �����.
public:
	stack(int size) :             //����������� �� ��������� ������  ��������� ������������ ����������� �����
		_root(new T[_maxSize = size]),  //������ �����, �� ���� ���������� ������������ �����������
		_pntr(_root)                    //��������� �� ������� ����� � ������ ����� ������ �����
	{}
	~stack()             // ���������� - ������� ��� ���������� ������,������� ������������ � ���� ������
	{
		delete _root;
	}
	void push(T item)          //��������� � ���� ��� ���� ������� 
	{
		*_pntr++ = item;       //� ��������� ��������� �� ������� ����� � ��������� ��������� �����
	}
	T pop()                    //������ ������� � ������� �����
	{
		return *--_pntr;       //�� ������� ��������� ��������� �� ������� ����� � ������� ����� ���� �������
	}
	int count() const          //���������� ���������� ��������� � �����
	{
		return _pntr - _root;  //�������� ����������
	}
	int maxSize() const        //���������� ������������ ����������� �����
	{
		return _maxSize;
	}

	class Iterator
	{
	public:
		Iterator(const Iterator &it) : //����������� �����������.
			_stack(it._stack),
			_index(it._index)
		{}
		Iterator(const stack *stack, int index = 0) : //����������� �� ���������
			_stack(stack),
			_index(index)
		{}
		Iterator &operator=(const Iterator &it)  //�������� ������������
		{
			_stack = it._stack;
			_index = it._index;
			return *this;
		}
		void operator++(int) //���� ���������
		{
			_index++;        //����������� �������� ������� ��������
		}
		bool operator()()    //��������. ���������� �� ���������
		{
			return _index != _stack->count() + 1;
		}
		T &operator*()        //�������� ��������������� ������ ����������
		{                    // ������� ����� ���������
			return _stack->_root[_index];//����� ��� � � stl
		}
		//�������� ��������� ���� ����������
		bool operator == (const Iterator &iterator)
		{
			return (_index == iterator._index) && (_stack == iterator._stack);
		}
		bool operator != (const Iterator &iterator)
		{
			return !(*this == iterator);
		}
	private:
		const stack *_stack; //��������� �� ����, �������� ����������� ������ ��������
		int _index;                //������ ��������, ������� "������" ������	��������
	};

	Iterator begin()
	{
		stack::Iterator it(this, 0);
		return it;
	}
	Iterator end()
	{
		stack::Iterator it(this, count());
		return it;
	}

	void Print() {
			stack::Iterator it = this->begin();  //��������, ����������� �� ������ ����������
			stack::Iterator end = this->end();   //��������, ����������� �� ����� ���������� 
			for (; it != end; it++) //�� ��� ���, ���� �� �� ����� �� ����� ����������
				std::cout << *it << ','; //������� ��, ��� ������ ���� �������� � �������
			std::cout << std::endl;  //��������� �����. ���������� ����� � �.�.
	}

	bool isEmpty() {
		return _pntr == _root;
	}
};
