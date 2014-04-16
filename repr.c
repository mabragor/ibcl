#include "/usr/include/python2.7/Python.h"
#include <stdio.h>

PyObject * repr(PyObject *obj) {
  fprintf(stderr, "Entering repr\n");
  PyObject *res = NULL;
  int rv = 0;

  res = PyObject_Str(obj);
  if (!res) goto error;
  fprintf(stderr, "Got string representation of object\n");
  fprintf(stdout, "repr says: ");
  rv = PyObject_Print(res, stdout, 0);
  if (rv == -1) goto error;

  fprintf(stdout, "\n");
  return res;

 error:
  Py_XDECREF(res);
  return NULL;
}
