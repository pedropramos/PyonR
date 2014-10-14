#include "Python.h"

void (Py_INCREF)(PyObject *o) { Py_INCREF(o); }
void (Py_XINCREF)(PyObject *o) { Py_XINCREF(o); }
void (Py_DECREF)(PyObject *o) { Py_DECREF(o); }
void (Py_XDECREF)(PyObject *o) { Py_XDECREF(o); }
void (Py_CLEAR)(PyObject *o) { Py_CLEAR(o); }

FILE* get_stdin()  { return stdin;  }
FILE* get_stdout() { return stdout; }
FILE* get_stderr() { return stderr; }

PyObject* get_PyNone() { return Py_None; }