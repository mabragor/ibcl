#include "/usr/include/python2.7/Python.h"
#include <stdio.h>

PyObject * length(PyObject *obj) {
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
  			       "lisp_length");
  if (!fun) goto error;
  fprintf(stderr, "Found lisp_length there\n");

  res = PyObject_CallFunctionObjArgs(fun,
  				     obj,
  				     NULL);
  if (!res) goto error;
  fprintf(stderr, "Called lisp_length\n");

 success:
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  fprintf(stderr, "Returning from lisp_length\n");
  return res;

 error:
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  Py_XDECREF(res);

  return NULL;
}

PyObject * lisp_equality (PyObject *num1, PyObject *num2) {
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
  			       "lisp_equality");
  if (!fun) goto error;
  fprintf(stderr, "Found lisp_equality there\n");

  res = PyObject_CallFunctionObjArgs(fun,
  				     num1,
				     num2,
  				     NULL);
  if (!res) goto error;
  fprintf(stderr, "Called lisp_equality\n");

 success:
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  fprintf(stderr, "Returning from lisp_equality\n");
  return res;

 error:
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  Py_XDECREF(res);

  return NULL;
}
