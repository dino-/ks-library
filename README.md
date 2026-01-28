# ks-library

## !!!!! ATTENTION !!!!!

This project has been permanently moved to Codeberg
([ks-library](https://codeberg.org/dinofp/ks-library)) and is no longer actively
maintained on Github. Do not use the Issues system on Github to report to us.
Don't bother forking or getting source from here as it will not be updated.

Microsoft is not a friend of open-source and we do ourselves a disservice
giving them this impressive power over our work.

Never forget 2020 when Github (a Microsoft product) removed the popular
open-source `youtube-dl` project, sparking enormous controversy. The issue is
not that pushback eventually prompted reinstatement - Github can and will act
like this against us at any time. Expect it and get your work away from
Microsoft, Satya Nadella and their pals in the White House.

## !!!!! ATTENTION !!!!!

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
