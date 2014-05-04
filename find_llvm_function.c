#include "/usr/include/python2.7/Python.h"
#include <stdio.h>

void * find_llvm_function(PyObject *sym) {
  PyObject *moddict = NULL;
  PyObject *mod = NULL;
  PyObject *fun = NULL;
  PyObject *res = NULL;
  void *res1 = NULL;

  moddict = PyImport_GetModuleDict();
  if (!moddict) goto error;

  mod = PyMapping_GetItemString(moddict,
  				"toy");
  if (!mod) goto error;
  fprintf(stderr, "Found toy module\n");

  fun = PyObject_GetAttrString(mod,
  			       "find_llvm_function");
  if (!fun) goto error;
  fprintf(stderr, "Found find_llvm_function there\n");

  res = PyObject_CallFunctionObjArgs(fun,
				     sym,
  				     NULL);
  if (!res) goto error;
  fprintf(stderr, "Called find_llvm_function\n");

  res1 = PyCObject_AsVoidPtr(res);
  if (!res1) goto error;
  fprintf(stderr, "Converted to void*\n");
  
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  Py_XDECREF(res);
  fprintf(stderr, "Returning from find_llvm_function\n");
  return res1;

 error:
  Py_XDECREF(fun);
  Py_XDECREF(mod);
  Py_XDECREF(res);

  return NULL;
}
