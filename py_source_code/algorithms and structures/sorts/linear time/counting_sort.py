count = {}

k = 100 #max value of integer

def counting_sort(a):
    for j in range(k):
        count[j]=0

    for j in range(len(a)):
        count[a[j]]+=1

    res = []
    for j in range(k):
        while count[j]>0:
            res.append(j)
            count[j]-=1

    print(res)

#version different from above. it does sorting with bringing satellite data
"""L=[]

k=10 #max value of key

def counting_sort(a):
    for j in range(k): #L should be a list of k empty lists
        L.append([])
        
    for j in range(len(a)):
        L[key(a[j])].append(a[j])
    output=[]
    for i in range(k):
        output.extend(L[i])"""
