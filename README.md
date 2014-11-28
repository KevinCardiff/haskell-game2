This project is a simple dungeon-crawling game written in Haskell.

You can download the Haskell platform (compiler and libraries) at http://www.haskell.org/platform/

To build and run the project after cloning the repository, open a terminal in the project folder and do:

    $ cabal configure && cabal build
    $ ./dist/build/haskell-game/haskell-game

Use the I, J, K, and L keys to move the player. Ctrl-C quits the game.

### Project Structure

- Main.hs: the main function
- Battle.hs: the implementation of the combat system
- Interaction.hs: handling player input and doing things based on it
- Datatypes.hs: all the datatypes used in the game
- Engine.hs: the core code that does one time step of the game state
- Graphics.hs: printed representations of the game objects, monsters, players, etc.
- Rendering.hs: helper functions for displaying stuff on the screen
- Utils.hs: miscellaneous utility functions that don't belong elsewhere
