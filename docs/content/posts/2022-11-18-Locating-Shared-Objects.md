---
layout: post
comments: yes
title: Locating Shared Objects
---
* The location of the shared objects needed for the-invaders are located in a lisp file ~/.the-invaders

* How is this file generated?

For each OS-platform there is function that verifies and writes the file.

* What needs to be defined currently

The current OS-platform is Windows-MSYS64 this is a 64 bit configuration for windows that locates the object-binaries in the MSYS2 environment.

* Example code seqment
```lisp
 (in-package #:object-binary-loader)
 (defparameter os "Windows")
 (defparameter platform "MSYS64")
 (defparameter msys64-base "")
 (defparameter sdl.dll-path "")
 (defparameter sdl_image.dll-path "")
 (defparameter sdl_ttf.dll-path "")
 (defparameter sdl_mixer.dll-path "")
```

The loading code should work this way.
 1. Go through all the external binaries in order and check that the file exists or throw a condition.
 2. Go through all the external binaries in order and cffi load each one.
 3. Return if no errors

* Build a working system

The first stage is to have a working system.
 - build an external library configurator called lib-configurator
   - first version will directly load the required libraries for the-invaders a works-on-my-system-version
   - next goal is to write and read the configuration to a file.

* First Stage development
 - Modify asd to require lib-configurator
 - Modify the-invaders. lispW to call the configure function.
 - Where are the libraries - hard code them
 - Which cffi symbols need to be defined - hard code them
 - Do per item hard code and loading.