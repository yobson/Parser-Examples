# Parsers

This is 3 projects to demonstrate the different kinds of parsers that are available on hackage

## Interpreter

This is a [Tiny BASIC](https://en.wikipedia.org/wiki/Tiny_BASIC) interpreter

You can run it with:
```
cabal run -- basic <PATH_TO_BASIC_FILE>
```

## Lego robot controller
See [this](https://github.com/yobson/haskell-lego/tree/main/spikectl)

## QOI Viewer

This is a simple image viewer for displaying images in the [qoi format](https://qoiformat.org).
To run it:

```
cabal run -- qoi-viewer <PATH_TO_IMAGE>
```

you can find some test images [here](https://qoiformat.org/qoi_test_images.zip)

It is written to show how to build parsers of binary formats
