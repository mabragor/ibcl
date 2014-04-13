#include "Python.h"
#include <stdio.h>

PyObject * atom(PyObject *obj) {
  PyObject *cls = NULL;
  int res = -1;
  int rv = NULL;

  cls = PyMapping_GetItemString(PyEval_GetGlobals(),
				"Cons");
  if (!cls) goto error;

  res = PyObject_IsInstance(obj, cls);
  if (res == 1) {
  } else if (res == 0) {
  } else {
    goto error;
  };

 error:
  Py_XDECREF(cls);
  Py_XDECREF(rv);

  return NULL;
}
