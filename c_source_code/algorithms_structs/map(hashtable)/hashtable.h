#pragma once

#include <list>
#include <iterator>
#include <string>
#include "hash.h"
using namespace std;

template <typename T>
struct element {

	string key;
	T value;

	element(string s, T x) : key(s), value(x){}

	bool operator==(element x) {
		return key == x.key && value == x.value;
	}
};


template <class T>
class hashtable {
protected:
	list<element<T>>* arr;	//массив списков для разрешения коллизий(chaining)
	int size;

public:
	hashtable() {
		arr = nullptr;
		size = 0;
	}

	hashtable(int m) {
		arr = new list<element<T>> [m];
		size = m;
	}

	~hashtable() {
		for (int i = 0; i < size; ++i)
			arr[i].clear();
		delete[] arr;
	}

	list<element<T>>* at(string key) {
		int index = my_hash(key, size);
		return &arr[index];
	}
};

template <class T>
class dict : public hashtable<T> {
public:
	dict() : hashtable() {}
	dict(int m) {
		arr = new list<element<T>>[m];
		size = m;
	}


	~dict() {
		for (int i = 0; i < size; ++i)
			arr[i].clear();
	}

	T& operator[](string key) {	//search operation
		list<element<T>>* chain = at(key);
		for (auto it = chain->begin(); it != chain->end(); it++) {
			if (it->key == key) 
				return it->value;
		}
		cout << "Error: element s klyuchom " << key << " ne naiden. Error code = ";
		int x = NULL;
		return x;
	}

	void insert(element<T>* x) {
		string key = x->key;
		list<element<T>>* chain = at(key);
		for (auto it = chain->begin(); it != chain->end(); it++) {
			if (it->key == key) {
				cout << "Error pri vstavke znacheniya "<< x->value << ": klyuch " << key << " uzhe est' v tablice!" << endl << endl;
				return;
			}
		}
		chain->push_front(*x);
	}

	void remove(element<T>* x) {
		list<element<T>>* chain = at(x->key);
		chain->remove(*x);
	}

	int operator==(dict& d) {
		int res = 0;
		for (int i = 0; i < size; i++) {
			list<element<T>> first_chain = arr[i];

			for (auto first_it = first_chain.begin(); first_it != first_chain.end(); first_it++) {
				list<element<T>> second_chain = *(d.at(first_it->key));

				for (auto second_it = second_chain.begin(); second_it != second_chain.end(); second_it++)
					if (first_it->key == second_it->key && first_it->value == second_it->value)
						res++;
			}
		}
		return res;
	}

	void print() {
		for (int i = 0; i < size; i++) {
			list<element<T>> chain = arr[i];

			for (auto it = chain.begin(); it != chain.end(); it++) {
				cout << it->key << " : " << it->value << endl;
			}
		}
	}
};