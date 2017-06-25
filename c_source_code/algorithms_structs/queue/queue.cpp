#include <iostream>
using namespace std;

template <class T> class queue
{
private:
	struct element
	{
		T value;
		element* next;
	};
	element* head;
	element* tail;
	void Remove(element* link)
	{
		if (!link)
			return;
		Remove(link->next);
		delete link;
	}
public:
	queue() {
		head = nullptr;
		tail = nullptr;
	}
	~queue() {
		if (this->isEmpty());
		else
			Remove(head);
	}
	void enqueue(T val) {
		element* x = new element();
		x->value = val;
		x->next = nullptr;
		if (isEmpty())
			head = x;
		else
			tail->next = x;
		tail = x;
	}
	T dequeue() {
		if (this->isEmpty())
			cout << "Error: queue is empty." << endl;
		else {
			T val = head->value;
			element* temp = head;
			head = head->next;
			delete temp;
			return val;
		}
	}
	bool isEmpty()
	{
		return (head == nullptr);
	}
	void Print() {
		element* x = head;
		while (x != nullptr) {
			cout << x->value << endl;
			x = x->next;
		}
	}
};

int main() {
	queue<int> q1;
	q1.enqueue(1); q1.enqueue(2); q1.enqueue(3);
	q1.dequeue();
	q1.Print();
	system("pause");
	return 0;
}