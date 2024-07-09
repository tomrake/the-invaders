---
layout: post
title: The Invaders, The Refactor
published: true
date: 2019-11-14T11:00:00+05:00
---
Jan Tatham ( Sebity on github http://www.sebity.com/ )
created a [Common Lisp clone of the Space Invaders game](https://github.com/sebity/the-invaders) on Github.
This is SDL 1.2 version game and needed a little tweeking to run on SBCL on Windows.
Once I got all the needed dll in my SBCL path, it seemed I could not reproduce the original error.
Because SDL is now at version 2.0 I decided to refactor similar code while still maintaining a working version.

## Thank you, Jan Tatham!
All the original game logic is well covered by Sebity, my adventure starts.