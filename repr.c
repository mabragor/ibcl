#include "/usr/include/python2.7/Python.h"
#include <stdio.h>

PyObject * repr(PyObject *obj) {
  PyObject *res = NULL;
  res = PyObject_Str(obj);
  fprintf(stdout, "repr says: ");
  PyObject_Print(res, stdout, 0);
  fprintf(stdout, "\n");
  return res;
}
