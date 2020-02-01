# quiet

Generic deriving of `Read` / `Show` with no record labels.

[![Hackage][hackage-shield]][hackage] [![Travis][travis-shield]][travis]

Often one wants to create a `newtype` which has a convenient field
accessor like `unUserId` below, but that makes the derived `Show`
instance overly verbose.

For example:

```hs
newtype UserId = UserId { unUserId :: String } deriving (Show)
```

Renders as:

```
ghci> show (UserId "simon")
UserId {unUserId = "simon"}
```

With `qshowsPrec` you can have a `Show` instance which doesn't print
the field labels. It will render as if the `unUserId` accessor wasn't
present at all.

```hs
newtype UserId = UserId { unUserId :: String } deriving (Generic)

instance Show UserId where showsPrec = qshowsPrec
```

```
ghci> show (UserId "simon")
UserId "simon"
```

A compatible `Read` instance can also be derived using `qreadPrec` if
necessary.

```hs
instance Read UserId where showsPrec = qreadPrec
```
 [hackage]: http://hackage.haskell.org/package/quiet
 [hackage-shield]: https://img.shields.io/hackage/v/quiet.svg?style=flat

 [travis]: https://travis-ci.org/jacobstanley/quiet
 [travis-shield]: https://travis-ci.org/jacobstanley/quiet.svg?branch=master
