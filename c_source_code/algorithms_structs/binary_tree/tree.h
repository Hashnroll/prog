#pragma once

#include <string>
#include <iterator>
#include <algorithm>

template <class T>
class BinaryTree
{
public:
	struct node
	{
		T val;
		node* left;
		node* right;
		int count = 0;
	};

	node* tree_min(node* cur)
	{
		while (cur->left)
			cur = cur->left;
		return cur;
	}

	node* tree_max(node* cur)
	{
		while (cur->right)
			cur = cur->right;
		return cur;
	}

	node* insert(node* cur, T val)
	{
		if (!cur)
		{
			cur = new node;
			cur->val = val;
			cur->left = NULL;
			cur->right = NULL;
		}
		if (val > cur->val)
			cur->right = insert(cur->right, val);
		else if (val < cur->val)
			cur->left = insert(cur->left, val);
		else if (val == cur->val)
			cur->count++;
		return cur;
	}

	node* remove(node* cur, T val)
	{
		if (!cur)
			return NULL;
		if (val > cur->val)
			cur->right = remove(cur->right, val);
		else if (val < cur->val)
			cur->left = remove(cur->left, val);
		else
		{
			if (cur->left == NULL && cur->right == NULL) //1: no child
			{
				delete cur;
				cur = NULL;
			}
			else if (cur->left == NULL)  //2: no left child
			{
				node* temp = cur;
				cur = cur->right;
				delete temp;
			}
			else if (cur->right == NULL)  //2: no right child
			{
				node* temp = cur;
				cur = cur->left;
				delete temp;
			}
			else
			{
				node* succ = tree_min(cur->right);
				cur->val = succ->val;
				cur->right = remove(cur->right, succ->val);
			}
		}
		return cur;
	}

	int size(node* cur)
	{
		if (cur != NULL)
		{
			return size(cur->left) + size(cur->right) + 1;
		}
		else
			return 0;
	}

	int size() {
		return size(root);
	}

	node* find(node* cur, T val)
	{
		if (!cur)
			return NULL;
		else if (val == cur->val)
			return cur;
		if (val > cur->val)
			return find(cur->right, val);
		else if (val < cur->val)
			return find(cur->left, val);
	}

	void inorder(node* cur)
	{
		if (cur != NULL)
		{
			inorder(cur->left);
			std::cout << cur->val << ", ";
			inorder(cur->right);
		}
	}

	BinaryTree()
	{
		root = nullptr;
	}
	~BinaryTree()
	{

	}

	node* search(T val)
	{
		return find(root, val);
	}

	void insert(T val)
	{
		root = insert(root, val);
	}
	void remove(T val)
	{
		root = remove(root, val);
	}
	void display()
	{
		inorder(root);
		std::cout << std::endl;
	}

private:
	node* root;
};