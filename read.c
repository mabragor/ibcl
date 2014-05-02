#include "/usr/include/python2.7/Python.h"
#include <stdio.h>

PyObject * prog1_read() {
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
  			       "prog1_read");
  if (!fun) goto error;
  fprintf(stderr, "Found read there\n");

  res = PyObject_CallFunctionObjArgs(fun, NULL);
  if (!res) goto error;
  fprintf(stderr, "Called read\n");

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
