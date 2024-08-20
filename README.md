# rune-cache

`rune-cache` is a RuneScape 2 cache library and CLI application written in
Haskell.

Note that, at the moment, `rune-cache` only supports *read* operations.

## Getting Started

### Usage

Reading a file from the cache:

```haskell
import Control.Monad.Trans.Except
  ( runExceptT )
import Prelude hiding
  ( readFile )
import RuneScape.Cache
  ( readFile, withCache )
import RuneScape.Cache.Archive
  ( ArchiveId (..) )
import RuneScape.Cache.File
  ( FileId (..) )
import RuneScape.Cache.Group.Id
  ( fromWord32 )

main :: IO ()
main = do
  res <- runExceptT . withCache "/path/to/cache" $ \c -> do
    let aId = ArchiveId 2
        groupId = fromWord32 10
        fileId = FileId 1042
    readFile c aId groupId fileId Nothing
  ...
```

## Documentation

Documentation can be found in the [`docs` directory](docs/README.md).

## Building

Building the library:

```
cabal build rune-cache
```

## Acknowledgments

- [OpenRS2](https://github.com/openrs2/openrs2)
- [Jagex-Store-5](https://github.com/guthix/Jagex-Store-5)
- [RuneLite](https://runelite.net)
- [RuneWiki.org](https://runewiki.org/Cache_Layout)
- [Explanation and Simplification of the RuneScape Cache](https://rune-server.org/threads/explanation-and-simplification-of-the-runescape-cache.534604/)
- [Things you may need to know about the RS engine](https://dreambot.org/forums/index.php?/topic/3324-things-you-may-need-to-know-about-the-rs-engine/)
- [rs-cache](https://github.com/jimvdl/rs-cache)
- [rs2-cache](https://github.com/osrs-rs/rs2-cache)
- [rsmod](https://github.com/Tomm0017/rsmod)
- [RuneStats Archive](https://archive.runestats.com/osrs/)

## License

[Apache-2.0](LICENSE)
