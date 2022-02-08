# aeson-tiled

[![Hackage](https://img.shields.io/hackage/v/aeson-tiled.svg)](https://hackage.haskell.org/package/aeson-tiled)

`aeson-tiled` is the spiritual successor to `htiled`. `htiled` uses `hxt` which relies
too heavily on Arrows and is rather hard to work with. Tiled's json export
supports 100% of Tiled's features, so there doesn't seem to be much of a
point to maintaining a large Arrows-based project when writing Aeson instances
for Tiled types is much easier. Hence this project!
