#include "/usr/include/python2.7/Python.h"
#include <stdio.h>

PyObject * intern(char *str) {
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
  			       "intern");
  if (!fun) goto error;
  fprintf(stderr, "Found intern there\n");

  res = PyObject_CallFunctionObjArgs(fun,
  				     PyString_FromString(str),
  				     NULL);
  if (!res) goto error;
  fprintf(stderr, "Called intern\n");

 success:
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  fprintf(stderr, "Returning from intern\n");
  return res;

 error:
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  Py_XDECREF(res);

  return NULL;
}
