---
layout: post
title: Getting the dll files
date: 2019-11-14-0001
---
### Where to install dlls for SBCL

The installation of dll shared libraries should be in the load path for SBCL.
My choice was to copy these files to directory containing sbcl.exe as this is always in SBCL search path on windows (see [this referemce] (https://docs.microsoft.com/en-us/windows/win32/dlls/dynamic-link-library-search-order)).

To use the same dll with multiple Common Lisp implementations is a more complex topic, where the most likely solutions is a shared dll directory that must be entered in each implementations path, likely by a shortcut or cmd script.

### Get the source code and dlls

My development system is Windows 10 64bit PC with emacs and slime.
I git cloned https://github.com/tomrake/the-invaders into my local_projects,this resulted in a starting point commit of a21ddaa.
I did a (ql:quickload :the-invaders)
I did a series of web searches for missing dll files and located the 64bit dll files which I added to my dll folder and then restarted the quickload until all missing dll were found for lisp-builder-sdl and lisp-builder-sdl-ttf.

I tried next did a (the-invaders:start) command.
I kept receiving error about not a BMP file type for the sprite sheet images which were PNG type.
The source code for lisp-builder-sdl in the traceback indicated PNG could be read as images.
There are examples and documentation in the lisp-builder-sdl code base.
A days research caused me to add lisp-builder-sdl-image to the packages required by the system.

I next did a round of dll installs for lisp-builder-sdl-image.
Changed the sdl:load-image calls to sdl-image:load-image.
(the-invaders:start) ran a corrent version of play.

