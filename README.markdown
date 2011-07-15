MetAl
=====
MetAl is a command-line utilty for calculating metric distances between alternative alignments of the same sequences.

Installation
------------
You need my phyhs library, first. Neither this nor phyhs are yet on hackage, so you'll have to use git. You also need [Haskell-Platform] installed.

[Haskell-Platform]: http://www.haskell-platform.org

    git clone git://github.com/benb/EvoHaskell.git phyhs
    cd phyhs
    cabal update
    cabal install
    cd ..
    git clone git://github.com/benb/MetAl.git metal
    cd metal
    cabal install

Binaries
--------
Binaries for Windows, Linux and MacOSX are available from our [Univeristy of Manchester] website, or from the [Github Downloads] page.

[Univeristy of Manchester]: http://kumiho.smith.man.ac.uk/whelan/software/metal/
[Github Downloads]: https://github.com/benb/MetAl/downloads

Usage
-----
Run `metal -h` for usage. There are four metrics, to be detailed in an upcoming publication.

Problems?
---------
Please contact me at benjamin.blackburne@manchester.ac.uk

License
-------
GPL3.
