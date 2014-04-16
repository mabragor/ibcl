#include "/usr/include/python2.7/Python.h"
#include <stdio.h>

PyObject * car(PyObject *obj) {
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
  			       "car");
  if (!fun) goto error;
  fprintf(stderr, "Found car there\n");

  res = PyObject_CallFunctionObjArgs(fun,
				     obj,
  				     NULL);
  if (!res) goto error;
  fprintf(stderr, "Called car\n");

 success:
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  fprintf(stderr, "Returning from car\n");
  return res;

 error:
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  Py_XDECREF(res);

  return NULL;
}

PyObject * cdr(PyObject *obj) {
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
  			       "cdr");
  if (!fun) goto error;
  fprintf(stderr, "Found car there\n");

  res = PyObject_CallFunctionObjArgs(fun,
				     obj,
  				     NULL);
  if (!res) goto error;
  fprintf(stderr, "Called cdr\n");

 success:
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  fprintf(stderr, "Returning from cdr\n");
  return res;

 error:
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  Py_XDECREF(res);

  return NULL;
}
