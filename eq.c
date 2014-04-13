#include "/usr/include/python2.7/Python.h"
#include <stdio.h>

PyObject * eq(PyObject *obj1, PyObject *obj2) {
  PyObject *moddict = NULL;
  PyObject *mod = NULL;
  PyObject *fun = NULL;
  PyObject *res = NULL;

  moddict = PyImport_GetModuleDict();
  if (!moddict) goto error;

  mod = PyMapping_GetItemString(moddict,
  				"toy");
  if (!mod) goto error;
  fprintf(stderr, "Found toy module\n");

  fun = PyObject_GetAttrString(mod,
  			       "eq");
  if (!fun) goto error;
  fprintf(stderr, "Found eq there\n");

  res = PyObject_CallFunctionObjArgs(fun,
				     obj1,
				     obj2,
  				     NULL);
  if (!res) goto error;
  fprintf(stderr, "Called eq\n");

 success:
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  return res;

 error:
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  Py_XDECREF(res);

  return NULL;
}
