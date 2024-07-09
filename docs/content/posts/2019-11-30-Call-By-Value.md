---
title: Call by value, libffi, cffi-libffi and Windows
layout: post
---
## Call by value in sdl2-ttf

The api for sdl2-ttf requires the rendering function to accept the color information as a call by value.
This references a windows implementation of SBCL.
cffi rejects all calls defined with :struct unless cffi-libfii is loaded.
I set the compiler to the ming64/gcc using `$CC`, but cffi-libffi seems to need to execute `pkg-config` to get the header information to compile the test code or glue code.
I need to setup the C compiler enviroment for ming64 which shell can correctly execute the command that cffi-libffi needs not to fail.

## Strategy for the solution

- Write C by reference glue code which does the by value calls. Compile C in asdf, will need `.h` files for sdl_ttf.

- Use the `$CC` variable to setup the mingw64 enivironment. Mounts are the difficult part.

- Find another way to fix cffi-libffi for mingw.

