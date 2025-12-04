# Introduction
The game is written purely in single-file, iAPX-88 (Intel-8088) Assembly. It is confirmed to compile on nasm, and might work on others, but not sure. I run it in dosbox, in 25x80 resolution.

---

# Smolness
Check out [Project Document](Project.pdf) for greater detail. <br>
The game is mostly similar to thsoe old Nokia snake games, however, it's much simpler than even that. <br>
This game only has: <br>
    1. One level (no concept of levels, actually) <br>
    2. Hard coded obstacles <br>
    3. Non-growable player character <br>
But that is barely any fun :)

---

# My Additions
Of course, the entire game is my own addition, but.. I want to discuss the things that are in my project, but were not required by the project docs. <br>

## Music:
I added purely port-based speaker support, and hardcoded Ode to Joy, from Beethoven's Finale of the 9th Symphony. The mechanism is quite interesting, and does not even use multithreading. It runs on the same thread as the actual game, and plays one note per timer tick.
## Screens:
No starting/pausing/losing/winning screens were required, but the game looks pretty bland without that, so I did add them anyway. Interesting to note that the main starting screen hooks a timer ISR to IRQ0 in order to display a spinning loader animation. <br>
All screens have blocking keystroke requirements, through INT 16, service 0.
## Some Visual Liberties:
A lot of things that are in the shown-but-not-required by the project doc have been modified to fit my personal taste, like 2 column thick vertical obstacles, and goal block. Along with the design choice to have 2 columns traversal per second timer tick. This circumvents the awkward ratio of 25:80.

---

# Compiling & Running
1. You will need to set-up dosbox with nasm, both of which are readily available on every operating system imaginable (idk about the weird Darwin OSes, but I'm sure if you use them, you'll just make your own stuff). On arch, the script [vimasm](https://github.com/sanecodeguy/vimasm.git) is wonderful, if you are on Neovim.
2. Navigate to this cloned repository's directory, and compile with:
```dos
nasm project.asm -o snake.com
```
3. You can now run this (iff there were no errors before this step) by typing out its name and pressing enter (shocking):
```dos
snake.com
```

---

# Who to Blame for Redundant Code:
Not me.
