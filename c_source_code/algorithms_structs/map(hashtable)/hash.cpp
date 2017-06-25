#include <string>

int my_hash(std::string key, int m) {
	long int id = 0;
	int i;
	for (i = 0; i < key.length() - 1; i++)
		id += (int)key[i];

	return id % m;
}