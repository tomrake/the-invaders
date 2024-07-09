---
layout: post
title: The Delivery of Shared Libraries
---
In [A previous post]({{ site.baseurl }}{% link _posts/2019-11-19-Supply-dlls.md %}) I dicussed a method to stragically load DLL shared code.

The general method is to load all the dynamic libraries up front,
forcing the locally preferred DLL/shared code to load before any
system library.
The only trade-off is that you must call loading routines before you use any other cffi form.
This method has been reported to work on Linux, MacOS and Windows.