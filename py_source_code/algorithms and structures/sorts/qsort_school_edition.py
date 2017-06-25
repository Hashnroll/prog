def _qsort(a, l, r):
    if l >= r:
        return
    mid = (l + r)/2
    x = a[mid]
    i, j = l, r
    while i <= j:
        while a[i] < x:
            i += 1
        while a[j] > x:
            j -= 1
        if i <= j:
            a[i], a[j] = a[j], a[i]
            i += 1
            j -= 1
    _qsort(a, l, j)
    _qsort(a, i, r)

def qsort(a):
    _qsort(a, 0, len(a) - 1)

        
