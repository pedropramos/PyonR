gcc -fPIC -c others.c -I/usr/include/python2.7/
gcc -shared others.o -L/usr/lib -lpython2.7 -o others.so