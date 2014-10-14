gcc -c others.c -I/Python27/include
gcc -shared others.o -L/Python27/libs -lpython27 -o others.dll