---
layout: post
comments: yes
title: Back Again
date: 2022-11-17
---
How to supply the SDL binary shared libraries has always been a question and this is my current strategy to resolve that question.
Leverage the LISP image model to load cffi references to the SDL objects in a wrapper code, that define the local locations of the various external object files.
The wrapper tell cffi where to find these objects and then starts the game.

The location of the individual SDL libraries is a platform specific mechanism,
this implies that each platform should various ways of defining the locations.
In my working example on Windows uses MSYS2 to supply the SDL.dlls.