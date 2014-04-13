#include "/usr/include/python2.7/Python.h"
#include <stdio.h>

char * repr(PyObject *obj) {
  return PyString_AsString(PyObject_Str(obj));
}
