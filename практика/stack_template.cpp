#include <iostream>
using namespace std;

template <class T> class stack
{
private:
  struct element
  {
    T value;
    element* next;
  };
  element* top;
  void Remove(element* link)
  {
    if (!link)
      return;
    Remove(link->next);
    delete link;
  }
public:
  stack() {
    top = nullptr;
  }
  ~stack() {
    Remove(top);
  }
  void push(T val) {
    element* x = new element();
    x->value = val;
    x->next = top;
    top = x;
  }
  T pop() {
    T val = top->value;
    element* t1 = top;
    this->top = top->next;
    delete t1;
    return val;
  }
  bool isEmpty()
  {
    return (top == nullptr);
  }
  void Print() {
    element* x = top;
    while (x != nullptr) {
      cout << x->value << endl;
      x = x->next;
    }
  }
};
  
int main() {
  stack<int> st1;
  st1.push(1); st1.push(2); st1.push(3);
  while (!st1.isEmpty())
    cout << st1.pop() << endl;
  cout << st1.isEmpty();
  return 0;
}
