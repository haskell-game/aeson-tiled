# aeson-tiled

[![Hackage](https://img.shields.io/hackage/v/aeson-tiled.svg)](https://hackage.haskell.org/package/aeson-tiled)

Types and instances for [Tiled](https://www.mapeditor.org/) map editor `.tmj` files.

> `aeson-tiled` is the spiritual successor to `htiled`.
> `htiled` uses `hxt` which relies too heavily on Arrows and is rather hard to work with.
> Tiled's json export supports 100% of Tiled's features, so there doesn't seem to be much of a point to maintaining a large Arrows-based project when writing Aeson instances for Tiled types is much easier.
> Hence this project!

The package is geared toward minimal divergence from Tiled type specifications.

Tiled data format is taken at version 1.8.4.
Maps from the previous versions may be loaded assuming there were no relevant breaking changes.

GHC versions are supported from 8.10 and follow Stackage LTS + Stackage Nightly + whatever is available on `ghcup`.

## Module structure

The modules are designed for qualified imports and `OverloadedRecordDot` extension.

- Modules under `Codec.Tiled.*` are concerned with `aeson` representation, one type at a time.
  They provide JSON `Value` translation to concrete types, but not much beyond that.
  Your application most likely should use its own representation.
- Modules under `Data.Tiled.*` are types specific to this package.
  They are more refined and may be utilized directly.
