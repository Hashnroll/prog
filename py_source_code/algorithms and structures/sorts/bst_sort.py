import bst

def Bst_sort(a):
    B = bst.BinarySearchTree()
    i = 0
    while i < len(a):
        B.Insert(bst.CreateNode(a[i]))
        i += 1
    del a[:]
    Bst_inorder_to_list(B.root, a)

def Bst_inorder_to_list(n, a):
    if n.leftChild != None:
        Bst_inorder_to_list(n.leftChild, a)
    a.append(n.key)
    if n.rightChild != None:
        Bst_inorder_to_list(n.rightChild, a)
    


    
    
