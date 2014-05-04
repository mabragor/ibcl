IBCL
----

Insanely Bootstrappable Common Lisp.

It is an attempt to build ANSI-compliant CL implementation, which
runs on LLVM (and thus is hopefully crossplatform) via incrementally
introducing features, starting with a very small kernel lisp.

Kernel lisp is built using llvmpy bindings, since prorotyping
in Python is much more fun than in C++.

TODO:
  * (done) Basic McCarthy's lisp, written (almost entirely) in Python
  * (done) Simplest lisp interpreter from Steele & Sussman's
    "Art of the interpreter", written in the lisp from previous step
    * LOAD-FILE function
      * (done) at Python level
      * at McCarthy level
      * at S&S level
    * FUNCALL function
      * at McCarthy level
      * at S&S level
  * the most sophisticated interpreter from S&S
  * hooks into construction of LLVM IR from lisp code
  * hot-compilation of this IR
  * ?????
  * PROFIT!

# License

Copyright (c) 2014 Alexander Popolitov (popolit@gmail.com)

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
