#include <list>
#include <map>
#include <iostream>
#include "parse.h"


using namespace std;



class vertex {
private:
	list<vertex> neighbours;
	string name;

public:
	vertex() {

	}
	~vertex() {
		neighbours.clear();
	}

	void printNeighbours() {
		for (auto it = neighbours.begin(); it != neighbours.end(); it++)
			cout << (*it).getName() << ", ";
	}

	void setName(string str) {
		name = str;
	}

	string getName() {
		return name;
	}

	list<vertex> getNeighbours() {
		return neighbours;
	}

	void addNeighbour(vertex x) {
		neighbours.push_back(x);
	}

	int amountOfNeighbours() {
		return neighbours.size();
	}
};

class graph {
private:
	map<string, vertex*> adj; //adjacency list(список ребер)

public:
	graph() {

	}
	~graph() {
		adj.clear();
	}

	graph operator=(const graph g) {
		adj.clear();
		copy(g);
		return *this;
	}

	void copy(graph g) {
		map<string, vertex*> g_adj = g.getAdj();
		for (auto it = g_adj.begin(); it != g_adj.end(); it++)
			addAdj(*it);
	}

	map<string, vertex*> getAdj() {
		return adj;
	}

	void addAdj(pair<string, vertex*> v) {
		adj.insert(v);
	}

	void input() {
		int v;
		cout << "Введите кол-во вершин графа: ";
		cin >> v;
		for (int i = 0; i < v; ++i) {
			vertex* x = new vertex;
			string name;
			cout << "Введите имя " << i + 1 << "й вершины(не более 10 символов): ";
			cin >> name;

			x->setName(name);
			addAdj(pair<string, vertex*>(name, x));
		}
		getchar();
		for (auto it = adj.begin(); it != adj.end(); ++it) {
			string neighbours;
			cout << "Для вершины " << it->first << " введите соседние вершины через пробел: ";
			getline(cin, neighbours, '\n');

			vector<string> names = parse(neighbours);

			for (int k = 0; k < names.size(); ++k)
				it->second->addNeighbour(*adj.at(names[k]));
		}
	}

	void print() {
		for (auto it = adj.begin(); it != adj.end(); ++it) {
			cout << "Вершина " << it->second->getName() << " соседствует c ";
			it->second->printNeighbours();
			cout << endl;
		}
	}
};

graph union_(graph g, graph h) { //объединение графов
	graph u;
	u = g;

	map<string, vertex*> u_adj = u.getAdj();
	map<string, vertex*> h_adj = h.getAdj();

	for (auto it_h = h_adj.begin(); it_h != h_adj.end(); ++it_h) {
		bool found_vertex = false;
		for (auto it_u = u_adj.begin(); it_u != u_adj.end(); ++it_u) {
			if (it_u->second->getName() == it_h->second->getName()) { //если имена вершин совпали, нужно дополнить соседей, если они есть
				found_vertex = true;
				list<vertex> cur_neighbours = it_u->second->getNeighbours();
				list<vertex> candidate_neighbours = it_h->second->getNeighbours();
				for (auto it_cand = candidate_neighbours.begin(); it_cand != candidate_neighbours.end(); it_cand++) {
					string cand_name = it_cand->getName();
					bool found_neighbour = false;
					for (auto it_cur = cur_neighbours.begin(); it_cur != cur_neighbours.end(); it_cur++) {
						if (cand_name == it_cur->getName()) {
							found_neighbour = true;
							break;
						}
					}
					if (!found_neighbour)
						it_u->second->addNeighbour(*it_cand);
				}
			}
		}
		if (!found_vertex)
			u.addAdj(*it_h);
	}

	return u;
}

graph inter_(graph g, graph h) {
	graph u;

	map<string, vertex*> g_adj = g.getAdj();
	map<string, vertex*> h_adj = h.getAdj();
	for (auto it_g = g_adj.begin(); it_g != g_adj.end(); it_g++) {
		for (auto it_h = h_adj.begin(); it_h != h_adj.end(); it_h++) {
			if (it_g->second->getName() == it_h->second->getName()) {
				string name = it_g->second->getName();
				vertex* x = new vertex;
				x->setName(name);

				list<vertex> g_neighbours = it_g->second->getNeighbours();
				list<vertex> h_neighbours = it_h->second->getNeighbours();

				for (auto it_g_neig = g_neighbours.begin(); it_g_neig != g_neighbours.end(); it_g_neig++)
					for (auto it_h_neig = h_neighbours.begin(); it_h_neig != h_neighbours.end(); it_h_neig++)
						if (it_g_neig->getName() == it_h_neig->getName())
							x->addNeighbour(*it_g_neig);

				u.addAdj(pair<string, vertex*>(name, x));
			}
		}
	}
	return u;
}
