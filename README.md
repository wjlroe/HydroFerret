# HydroFerret

spacecats

## Dependencies and development

Ensure you've got the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) installed, you will need it to run
the commands in this README.

After installing Haskell Stack, ensure you run `stack setup` to download and
install a `ghc` and so on.

To build this project run:

    stack build

To run the web server:

    stack exec spacecatslol

Or, to specify a port to listen on:

    PORT=8181 stack exec spacecatslol
