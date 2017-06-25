template<type T> 
bool condition(T lhs, T rhs)
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
void insertSort(Iterator begin, Iterator end, bool (*condition)(T , T)) //Входные параметры - итераторы и фунция для определения порядка.
{
    Iterator it = begin;
    for(; it!=end; it++)
    {
        Iterator p = it;
        for(p++; p!=end; p++)
        {
            if(condition(*it, *p))
                Swap(*it, *p);
        }
    }
}
