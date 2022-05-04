module Main where

import Test.Tasty
import Test.Tasty.HUnit

import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import qualified Codec.Tiled.Map.IO as Map
import qualified Codec.Tiled.Tileset.IO as Tileset

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Codec"
  [ mapCodecTests
  , tilesetCodecTests
  ]

mapCodecTests :: TestTree
mapCodecTests = testGroup "Map"
  [ testGroup "Roundtrip"
      [ testCase "Embedded tileset" mapTestRoundtripEmbedded
      , testCase "External tileset" mapTestRoundtripExternal
      ]
  ]

mapTestRoundtripEmbedded :: IO ()
mapTestRoundtripEmbedded = do
  embeddedZstd <- Map.readFile "maps/microgue/embedded-zstd.tmj"
  withSystemTempDirectory "" \path -> do
    let out = path </> "map.tsj"
    Map.writeFile out embeddedZstd
    embeddedZstdOut <- Map.readFile out
    embeddedZstd @?= embeddedZstdOut

mapTestRoundtripExternal :: IO ()
mapTestRoundtripExternal = do
  embeddedZstd <- Map.readFile "maps/microgue/external-zstd.tmj"
  withSystemTempDirectory "" \path -> do
    let out = path </> "map.tsj"
    Map.writeFile out embeddedZstd
    embeddedZstdOut <- Map.readFile out
    embeddedZstd @?= embeddedZstdOut

tilesetCodecTests :: TestTree
tilesetCodecTests = testGroup "Tileset"
  [ testCase "Roundtrip" tilesetCodecRoundtrip
  ]

tilesetCodecRoundtrip :: IO ()
tilesetCodecRoundtrip = do
  tiles <- Tileset.readFile "maps/microgue/tiles.tsj"
  withSystemTempDirectory "" \path -> do
    let out = path </> "tiles.tsj"
    Tileset.writeFile out tiles
    embeddedZstdOut <- Tileset.readFile out
    tiles @?= embeddedZstdOut
