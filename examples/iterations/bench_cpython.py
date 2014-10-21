#lang python

def bench_for(n):
  L = []
  for i in range(1,n+1):
    L.append(2*i)
  return all(L)

def bench_listcomp(n):
  return all([2*i for i in range(1,n+1)])

def bench_gen(n):
  gen = (2*i for i in range(1,n+1))
  return all(gen)


from time import time

a = time()
bench_for(1000)
b = time()
print b-a, "seconds"

a = time()
bench_for(10000)
b = time()
print b-a, "seconds"

a = time()
bench_for(100000)
b = time()
print b-a, "seconds"

a = time()
bench_for(1000000)
b = time()
print b-a, "seconds"

##############################

a = time()
bench_listcomp(1000)
b = time()
print b-a, "seconds"

a = time()
bench_listcomp(10000)
b = time()
print b-a, "seconds"

a = time()
bench_listcomp(100000)
b = time()
print b-a, "seconds"

a = time()
bench_listcomp(1000000)
b = time()
print b-a, "seconds"

##############################

a = time()
bench_gen(1000)
b = time()
print b-a, "seconds"

a = time()
bench_gen(10000)
b = time()
print b-a, "seconds"

a = time()
bench_gen(100000)
b = time()
print b-a, "seconds"

a = time()
bench_gen(1000000)
b = time()
print b-a, "seconds"