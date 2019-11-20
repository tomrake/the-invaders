---
title: DLL name conflicts
layout: post
---
### Avoiding DLL Hell

It seems that SDL 2 dll have the same names as SDL 1.2 components and I wish to use both in this repo.
SDL 2 is now under zlib liscense so I can distribute the binaries without the code.
I plan to have a fat multi-platfom lib directory, so for each platform `lib/linux`, `lib/windows`, `lib/darwin` exist in the rep.
I heavily borrow concepts from Pavel Korolev's Guide [Delivering games written in Common Lisp](https://borodust.org/delivering-common-lisp) to structure my code.

### Load the dll upfront from platform specific paths

Before I use any wrappers which will load the shared object files, I change the load path to my platform specific path first and load each required shared object.



