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
	T *_root,                  //Указатель на начало стека, чтобы потом его можно было удалить
		*_pntr;                  //Указатель на вершину стека
	int _maxSize;              //Максимальная вместимость стека.
public:
	stack(int size) :             //Конструктор по умолчанию должен  содержать максимальную вместимость стэка
		_root(new T[_maxSize = size]),  //Начало стэка, за одно запоминаем максимальную вместимость
		_pntr(_root)                    //указатель на вершину стэка в начале равен началу стэка
	{}
	~stack()             // Деструктор - удаляет всю выделенную память,которая используется в этом классе
	{
		delete _root;
	}
	void push(T item)          //Добавляет в стек ещё один элемент 
	{
		*_pntr++ = item;       //и переводит указатель на вершину стека в следующий свободный адрес
	}
	T pop()                    //Достаёт элемент с вершины стека
	{
		return *--_pntr;       //но сначала переводит указатель на вершину стека в которой лежит этот элемент
	}
	int count() const          //Возвращяет количество элементов в стеке
	{
		return _pntr - _root;  //Адресная арифметика
	}
	int maxSize() const        //Возвращает максимальную вместимость стека
	{
		return _maxSize;
	}

	class Iterator
	{
	public:
		Iterator(const Iterator &it) : //Конструктор копирования.
			_stack(it._stack),
			_index(it._index)
		{}
		Iterator(const stack *stack, int index = 0) : //Конструктор по умолчанию
			_stack(stack),
			_index(index)
		{}
		Iterator &operator=(const Iterator &it)  //Оператор присваивания
		{
			_stack = it._stack;
			_index = it._index;
			return *this;
		}
		void operator++(int) //пост инкремент
		{
			_index++;        //Увеличивает значение индекса элемента
		}
		bool operator()()    //проверка. Закончился ли контейнер
		{
			return _index != _stack->count() + 1;
		}
		T &operator*()        //Оператор разименовывания обычно возвращает
		{                    // Элемент этого итератора
			return _stack->_root[_index];//Также как и в stl
		}
		//Оператор сравнения двух итераторов
		bool operator == (const Iterator &iterator)
		{
			return (_index == iterator._index) && (_stack == iterator._stack);
		}
		bool operator != (const Iterator &iterator)
		{
			return !(*this == iterator);
		}
	private:
		const stack *_stack; //Указатель на стек, которому принадлежит данный итератор
		int _index;                //Индекс элемента, который "хранит" данный	итератор
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
			stack::Iterator it = this->begin();  //Итератор, указывающий на начало контейнера
			stack::Iterator end = this->end();   //Итератор, указывающий на конец контейнера 
			for (; it != end; it++) //до тех пор, пока мы не дошли до конца контейнера
				std::cout << *it << ','; //Выводим то, что хранит этот итератор и запятую
			std::cout << std::endl;  //Завершаем вывод. сбрасываем буфер и т.д.
	}

	bool isEmpty() {
		return _pntr == _root;
	}
};
