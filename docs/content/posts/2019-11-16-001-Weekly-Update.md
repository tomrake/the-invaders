---
title: Weekly Update #1
layout: post
date: 2019-11-16
---
The week of work on the-invaders.

Reference [the devel repo.](https://github.com/tomrake/the-invaders/tree/fbd12ded83fb358b6e50204dd364f13a90261518)

- Game play on screen element are CLOS based.
- Sheet based object images and animation cells are a small CLOS structures orthangonal to the screen elements. The shots are still rendered with direct SDL code.
- Various backgrounds info screens use direct sdl code.
- My new debug display used direct SDL code.
- The sound options are throught the code.

There are still boundary and motion control issues to tackle but conversion to cl-sdl2 code is the focus for the next week of work.

## BUGS

There is still a bug in the SHIP explosion because the exposion jumps to the location of the replaced SHIP just before it is drawn.

The game is missing the Space Invaders Shields, so this function needs to be created.