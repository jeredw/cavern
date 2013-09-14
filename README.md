Overview
========

CAVERN is an Atari 2600 game in which you must make your way out of a
large cave system before you run out of air.

The cave is seen from a side view. It's very dark, so you can only see
a limited area around you.

As usual, you'll need to contend with gravity. While your character is
very athletic and could probably jump if there were a button for it,
obstacles are big, so it would not help much. Fortunately you can run,
climb and fall arbitrary distances as long as you have enough air.

The amount of air remaining is shown in a meter at the lower right of
the screen. You will encounter more pockets of air to boost your
supply as you descend deeper into the cave.

Controls
========

RUN by pushing the joystick left or right.

CLIMB by bumping into a wall and then pushing the joystick up. When
climbing, you will automatically alight when you reach a ledge.

Note that, while falling, you can move left or right. If you bump a
wall while falling and then press up, you can grab on and begin
climbing. Mastering this is key to progressing through several
trickier parts of the cave.

Warnings
========

There are many places where it is possible to get stuck.

The cave is fairly geologically stable, but cave-ins are possible and
even likely! A cave-in may result in a new passage opening up in a
previously impassable place.

Due to unforseen subtleties of 8-bit quantum physics, you may rarely
perceive yourself teleporting through a wall or floor while attempting
certain acrobatic maneuvers in certain tight spots. Should this
happen, remain calm. This is clearly impossible. It is probably just
something you ate. Anyway, no such freak occurrence is necessary to
escape the cave successfully.

Technical Rambling
==================

This is my first Atari 2600 game and was a lot of fun to program.

I used DASM and Stella to develop and test the game, and found both
worked great. Though I have a 64-bit machine so had to patch DASM to
fix some silly assumptions about 32-bit pointers. I wrote a simple web
page, map.html, to edit the map.

Strangely, I didn't use git during development but have attempted to
reconstruct a partial history of the game source from saves.

After the game was done I tested this it on an Atari 2600 console on
an NTSC TV, a little Sceptre E16. My vertical sync was off by a couple
of lines, which I found through quick trial and error, but otherwise
the game plays fine. I was especially pleased with how the joystick
worked out since I'd only tested with arrow keys and wasn't sure the
controls would feel right. But climbing seems appropriately hard.

The game rendering works by soft scrolling around a big bitmap into a
64 byte framebuffer (half the total RAM!) Its "kernel", which is what
Atari 2600 programmers call the loop that draws TV scanlines, draws
32-bit wide framebuffer rows by selecting mirrored playfield rendering
and then setting PF1, PF2, PF2 and PF1 just in time for each 8 pixels.
The sprite is drawn on each line, rather than being delayed. It might
be possible to get two sprites with a delay. You could add bats or
some kind of evil tacky cave troll that you can shoot with a pixel.

The game logic is pretty dumb and was hacked together quickly.
Collision detection uses a combination of TIA hardware registers and
inspecting the framebuffer because it looked janky to have gravity
bump the lit region up and down vertically for a frame.

I considered compressing the map, but it looked like a wash given the
extra complexity in the unpacking and soft scroll code. Maybe not
though! There are some cheesy compression functions in the Javascript.

TODO
====

While the ROM seems full, a motivated programmer could easily squeeze
another 32 bytes or more out. There's some extra padding in sprite
frames, some places could actually use stack instead of lda/sta'ing
into it, the code could be reordered to skip some jumps, the byte
reverse loop could be a loop instead of repeated.

There are a couple code paths that are not timed right so in some
parts of the world the picture jumps for a few frames. That should
be fixable with some patience.

The sound is awful. Some kind of ambient music might be cool.

An ending screen would be cool. I was thinking twinkling stars.

There should be some kind of reward for exploring more of the cave,
and maybe a way to blast out of dead ends sometimes or go back.
