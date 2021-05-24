# Tetranglix

![Screenshot](http://i.imgur.com/HaHrpoU.png)

Tetranglix, a 512-byte OS, is intended to be a playable and bootable Tetris clone. While the project aims at fitting it 
in 512-bytes, it tries not to compromise with the gameplay, that which is important to the gamer.

## Why?

The project originates from `#osdev-offtopic` (on Freenode, `irc.freenode.net`), where sortiecat provided inspiration to
the co-authors to write a 512-byte Tetris clone. It was released on the 16th of September, 2013, to mark the 1-year
anniversary of the channel.

And, of course, "Why not?"

## Testing It Out

### Building It Yourself

The build system only supports *nix hosts, and as such hasn't been tested elsewhere. After cloning the repository, you can run:

    ./configure && make dog
    make

Note that `./configure` isn't absolutely necessary, and might fail on some hosts. After performing the above operations, 
you'll obtain a floppy disk, `tetranglix.img`, in the build directory.

### Pre-built Images

Pre-built images can be obtained from the Releases section, on the GitHub repository.

### Running

The floppy image works with all major emulators, and any 1.44MiB floppy disk. Gameplay on Bochs might be a bit difficult to handle,
due to the way Bochs handles timing.

As an example, `qemu-system-i386 -fda tetranglix.img` can be used to test it via QEMU.

## Gameplay

Anyone familiar with Tetris wouldn't have much difficulty adapting to Tetranglix. The left and right cursor controls 
work exactly as expected. The up cursor control rotates the current tetramino in the clockwise direction.

Due to a lack of space, scoring was left out. The only challenge in the game is to survive, and the game (unofficially) 
ends when any newly spawned tetramino collides with an already existing one.

## Hacking It

The game was written by, and for hackers. For starters, clone the repository and tinker around. Simple changes can 
include modifying line 86 to decrease or increase the difficulty.

As an open challenge, we invite you all to add scoring support.

*UPDATE*: Thanks to Peter (peterferrie), scoring has now been added, and the game halts on end.

## Authors

While only nortti (JuEeHa) and shikhin have pushed to the repository, XgF (erincandescent) has helped immensely by optimizing 
several parts of the game. sortie was responsible for testing it out regularly, and giving the original inspiration.

All authors hang out in `#offtopia` (Libera.Chat).
