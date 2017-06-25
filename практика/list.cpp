#include <iostream>
using namespace std;

template <class T> class double_list
{
private:
  struct node
  {
    T value;
    node* next;
    node* prev;
  };
  node* first;
  node* last;
  void Remove(node* link)
  {
    if (!link)
      return;
    Remove(link->next);
    delete link;
  }
public:
  double_list()
  {
    first = last = nullptr;
  }
  double_list(const double_list<T> &l) //конструктор копирования, отличается от оператора = тем, что не нужно удалять исходный лист и не нужно возвращать значение
  {
    first = new node();      
    first->value = (l.first)->value; 
    first->prev = nullptr;
    node* x = (l.first)->next;
    node* prevnode = first;
    while (x) {              
      node* newx = new node();
      newx->value = x->value;
      prevnode->next = newx;
      newx->prev = prevnode;
      prevnode = newx;
      x = x->next;
    }
    prevnode->next = nullptr;
    last = prevnode;
  }
  ~double_list()
  {
    Remove(first);
  }
  void InsertBegin(T val)
  {
    node* x = new node();
    x->value = val;
    x->prev = nullptr;
    x->next = first;
    if (!isEmpty()){
      first->prev = x;
    }
    else
      last = x;
    first = x;
  }
  void InsertEnd(T val)
  {
    node* x = new node();
    x->value = val;
    x->next = nullptr;
    x->prev = last;
    if (!isEmpty()){
      last->next = x;
    }
    else
      first = x;
    last = x;
  }
  void RemoveBegin()
  {
    if (isEmpty()) {
      cout << "Error: list is empty." << endl;
      return;
    }
    node *x = first;
    first = first->next;
    first->prev = nullptr;
    delete x;
  }
  void RemoveEnd()
  {
    if (isEmpty()) {
      cout << "Error: list is empty." << endl;
      return;
    }
    node *x = last;
    last = last->prev;
    last->next = nullptr;
    delete x;
  }
  bool isEmpty()
  {
    return (first == nullptr);
  }
  void Print()
  {
    node* x = first;
    while (x != nullptr) {
      cout << x->value << endl;
      x = x->next;
    }
  }
  int size()
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
    if (n > 0 and n < this->size()){ 
      node *x = first;
      while (n-- > 0)
	x = x->next;
      return x->value;
    }
    else {
      cout << "Error: index is out of range." << endl;
      exit(0);
    }
  }
  double_list<T>& operator=(const double_list<T> &l)
  {
    Remove(first);           //удалить лист, которому присваиваем другой лист
    first = new node();      //копируем первый элемент
    first->value = (l.first)->value; 
    first->prev = nullptr;
    node* x = (l.first)->next;
    node* prevnode = first;
    while (x) {              //копируем все остальные элементы, пока x != nullptr
      node* newx = new node();
      newx->value = x->value;
      prevnode->next = newx;
      newx->prev = prevnode;
      prevnode = newx;
      x = x->next;
    }
    prevnode->next = nullptr; //x = nullptr, значит пред. элемент - last
    last = prevnode;
    return *this;
  }
};

int main() {
  double_list<int> l1;
  l1.InsertBegin(1);
  l1.InsertEnd(2);
  l1.InsertBegin(0);
  l1.InsertEnd(3);
  l1.InsertEnd(4);
  l1.InsertEnd(5);
  double_list<int> l2;
  l2 = l1;        
  l2.Print();
  l2[1] = 10;
  cout << endl;
  l2.Print();
  double_list<int> l3(l1);
  cout << endl;
  l3.Print();
  return 0;
}
