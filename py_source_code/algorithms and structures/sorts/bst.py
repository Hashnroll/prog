class BinarySearchTree:

    def __init__(self):
        self.root = None
        
    def Insert(self, z):
        y = None
        x = self.root
        while x != None:
            y = x
            if z.key < x.key:
                x = x.leftChild
            else:
                x = x.rightChild
        z.parent = y
        if y == None:
            self.root = z
        elif z.key < y.key:
            y.leftChild = z
        else:
            y.rightChild = z

    def Transplant(self, u, v):
        if u.parent == None:
            self.root = v
        elif u == u.parent.leftChild:
            u.parent.leftChild = v
        else:
            u.parent.rightChild = v
        if v != None:
            v.parent = u.parent

    def Delete(self, z):
        if z.leftChild == None:
            self.Transplant(z, z.rightChild)
        elif z.rightChild == None:
            self.Transplant(z, z.leftChild)
        else:
            y = z.rightChild.Tree_min() #finding successor
            if y.parent != z:
                self.Transplant(y, y.rightChild)
                y.rightChild = z.rightChild
                y.rightChild.parent = y
            self.Transplant(z, y)
            y.leftChild = z.leftChild
            y.leftChild.parent = y

class Node:

    def __init__(self):
        self.leftChild = None
        self.rightChild = None
        self.parent = None
        self.key = None

    def Tree_min(self):
        x = self
        while x.leftChild != None:
            x = x.leftChild
        return x

    def Tree_max(self):
        x = self
        while x.rightChild != none:
            x = x.rightChild
        return x

def CreateNode(key):
    n = Node()
    n.key = key
    return n


    
        
        
