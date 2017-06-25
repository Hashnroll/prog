def heapsort(a):
    build_max_heap(a)
    end = len(a) - 1
    for i in range(end, 0, -1):
        a[i], a[0] = a[0], a[i]
        end = end - 1
        max_heapify(a, 0, end)

def build_max_heap(a):
    length = len(a)
    start = length // 2 - 1
    for i in range(start, -1, -1):
        max_heapify(a, i, length - 1)
    
def max_heapify(a, i, end):
    largest = i
    left = 2 * i + 1
    right = 2 * (i + 1)
    if left <= end and a[left] > a[largest]:
        largest = left
    if right <= end and a[right] > a[largest]:
        largest = right
    if largest != i:
        a[largest], a[i] = a[i], a[largest]
        max_heapify(a, largest, end)
