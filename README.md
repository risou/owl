# Owl

Owl is a mini blog (or simply logger) built up with Yesod.

Owl means "One way logger ".

## Download

You can get Owl(zip) from this page, or clone this repository.

## Configuration

Edit `config/settings.yml`.  
You should change `approot`, `title` and `port`.

## Start application

    $ cabal configure -fproduction
    $ cabal build
    $ ./dist/build/owl/owl Production


