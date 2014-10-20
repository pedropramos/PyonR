#ifdef _WIN32
#  define MODULE_API __declspec(dllexport)
#else
#  define MODULE_API
#endif

#include "Python.h"

MODULE_API void (Py_INCREF)(PyObject *o) { Py_INCREF(o); }
MODULE_API void (Py_XINCREF)(PyObject *o) { Py_XINCREF(o); }
MODULE_API void (Py_DECREF)(PyObject *o) { Py_DECREF(o); }
MODULE_API void (Py_XDECREF)(PyObject *o) { Py_XDECREF(o); }
MODULE_API void (Py_CLEAR)(PyObject *o) { Py_CLEAR(o); }

MODULE_API FILE* get_stdin()  { return stdin;  }
MODULE_API FILE* get_stdout() { return stdout; }
MODULE_API FILE* get_stderr() { return stderr; }

MODULE_API PyObject* get_PyNone() { return Py_None; }