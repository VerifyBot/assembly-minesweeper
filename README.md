# 🎱️ Assembly Minesweeper

Minesweeper implementation in **8086 assembly**, running on `DosBox`

<hr>

This project shows many features you can get out of the basic 8086 set.
The `utils/tools.asm` file contains many macros and procedures that can
be used in other projects as they are mostly general.

##  Game Description

*<p>Minesweeper is a single-player puzzle video game.
The objective of the game is to clear a rectangular
board containing hidden "mines" or bombs without
detonating any of them, with help from clues about
the number of neighboring mines in each field.</p>*

### 📦 Showcase
<img src="https://i.imgur.com/EMS7qCY.png" width="500"> <br>
<img src="https://i.imgur.com/S2J2jRP.png" width="500"> <br>
<img src="https://i.imgur.com/qdNsw5N.png" width="500"> <br>
<img src="https://i.imgur.com/Rg2rRnP.png" width="500"> <br>
<img src="https://i.imgur.com/nlhJJRj.png" width="500"> <br>


# 🐉 How To Run

### 🎯 Step 1: Compile

- `tasm /zi mine.asm`

### 🎯 Step 2: Link files and get executeable

- tlink /v mine.obj

### 🎯 Step 3: Run

- mine.exe

# 📂 Files

### 🪁 Main Code

- `mine.asm`  main project file - contains game loop and calls procedures from other files
- `main.asm`  main page code - contains the main page of the game with buttons
- `game.asm`  game page code - contains code for the actual game, when pressing the START button
- `help.asm`  help page code - contains code to display information on how to play the game
- `stats.asm` stats page code - contains code to display statistics on the gameplay - top players, top points, wins,
  loses, top time

### 🪁 Utilities files

- `tools.asm`   contains useful procedures and macros that are used all over the project!
- `bmpUtil.asm` contains procedures and macros that help loading assets .bmp onto the screen
- `random.asm`  contains macros that help generating random numbers - byte size and word size

### 🪁 Assets

- `1.bmp, 2.bmp, ..., 8.bmp` minesweeper cells numbers
- `air.bmp, cell.bmp, bomb.bmp, flag.bmp` more cells textures
- `flgclc.bmp` asset to prettify flags count and stopwatch
- `mainf.bmp, gamef.bmp, helpf.bmp` pages 320x200 assets
- `blood.bmp` funny easteregg
- `n0.bmp, n1.bmp, ..., n9.bmp` numbers to display values in a pretty font - see: *proc PrintNumber bmpUtil.asm*
- `winner.bmp, loser.bmp` popup alerts for losing and winning


