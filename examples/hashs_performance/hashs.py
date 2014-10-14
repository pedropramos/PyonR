import random
import time

alphabet = "abcdefghijklmnopqrstuvwxyz"

def random_word(n):
  return ''.join([random.choice(alphabet) for i in range(n)])

words = [random_word(3) for k in xrange(1000000)]
d = {}

a = time.time()
for w in words:
  if w in d:
    d[w] = d[w] + 1
  else:
    d[w] = 1
b = time.time()
print b-a, 'seconds'
