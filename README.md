# ks-library


## Synopsis

Library of common modules for KitchenSnitch (Haskell)


## Description


## Installing


## Configuration and execution


## Building from source

Make sure you have `ghc 7.10.x`, `cabal-install` and `darcs` installed.

Update your cabal list

    $ cabal update

And install some native deps that `cabal` can't do for you

On Ubuntu:

    # apt-get install --reinstall g++ 
    # apt-get install libzip-dev

On Arch Linux:

    # pacman -S libzip

Get the `ks-libary` source code

    $ darcs get http://hub.darcs.net/dino/ks-libary

Update your cabal library and tools, we need a modern version

    $ cabal install Cabal cabal-install

Set up a sandbox for building (if you wish to use a sandbox)

    $ mkdir ~/.cabal/sandbox
    $ cabal sandbox init --sandbox=$HOME/.cabal/sandbox/kitchensnitch

Then install the dependencies

    $ cabal install --enable-tests --only-dep

This will build for quite some time, when it's done, you can build
ks-download:


### Building for development

    $ cabal configure --enable-tests
    $ cabal build
    $ cabal test

And you should be good for development from here.

Later, to install into the sandbox (so other project can use this library):

    $ cabal install

Also, to generate the `tags` file:

    $ hasktags --ctags .


## Contact

### Reporting Bugs

### Authors

Dino Morelli <dino@ui3.info>


## Links
