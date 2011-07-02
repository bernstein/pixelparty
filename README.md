pixelparty
==========
A program to experiment with glsl shaders.

Getting started
---------------

You can play with some shaders from here:
[pixelparty-shader](https://github.com/bernstein/pixelparty-shader)

    $ pixelparty mandelbrot.glsl
    $ pixelparty tunnel.glsl -t picture.jpg

Press 'r' to reload your shader, 's' to make a screenshot to pixelparty.jpg and
ESC to quit.

Installing pixelparty
---------------------

### Linux, etc.

If you have installed [The Haskell Platform](http://hackage.haskell.org/platform/)
then you can configure, build, and install all in the usual way with Cabal
commands.

    $ runhaskell Setup.lhs configure
    $ runhaskell Setup.lhs build
    $ runhaskell Setup.lhs install
