# Cabal Stack Awesome Init

## What problem does this solve?
Saves having to go through the tedious `cabal init` and adding a `.ghci file` to show all warnings.

## Instructions
Git clone this project, and run the following command (you'll need to modify the source and destination) in the directory of the new project:

`rsync -av --progress ~/Projects/Haskell/CabalInit/ . --exclude .git  --exclude Readme && git init`

You can use the the `ghcid.sh` script to start a ghcid session. 
