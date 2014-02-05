# HydroFerret

spacecats

## Dependencies and development

You should use a recent Cabal release (at least 1.18) so you can use the [cabal
sandbox](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html). It's possible
to use cabal-dev of course, but the new cabal sandbox works with more tools
(such as [hdevtool](https://github.com/bitc/hdevtools), which I recommend.

    cabal sandbox init
    cabal install --only-dependencies
    cabal configure
    cabal build

Then to run:

    ./dist/build/spacecatslol/spacecatslol

Or, to specify a port to listen on:

    PORT=8181 ./dist/build/spacecatslol/spacecatslol
