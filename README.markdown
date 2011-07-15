MetAl is a command-line utilty for calculating metric distances between
alternative alignments of the same sequences.

## Download

Binaries are available for Windows, Linux and Mac..

### Windows
[metal-win.zip](http://foo/)

### Linux
Modern distros: [metal-linux.tar.gz](http://foo/)

Older distros: [metal-linux-glibc2.5.tar.gz](http://foo/)

### Mac
[metal-mac.tar.gz](http://foo/)

## Usage

Run `metal -h` for usage. There are four metrics, to be detailed in an
upcoming publication.

## Installation from source

MetAl is written in Haskell. 

You need my phyhs library first. Neither this nor phyhs are yet on
Hackage, so you`ll have to use git. You also need
[Haskell-Platform](http://haskell.org/platform) installed.

    git clone git://github.com/benb/EvoHaskell.git phyhs
            cd phyhs
            cabal update
            cabal install
            cd ..
            git clone git://github.com/benb/MetAl.git metal
            cd metal
            cabal install
            

## Problems?

Please contact me at <benjamin.blackburne@manchester.ac.uk>

## License

GPL3.
