# Chapter 19 - General Remarks

## Concatenating key configurations

In the example with **XMonad**, the following code is shown:

```haskell
main = do
  xmonad def { keys =
    \c -> fromList [
      ((0, xK_F6),
        lowerVolume 4 >> return ()),
      ((0, xK_F7),
        raiseVolume 4 >> return ())
    ] `mappend` keys defaultConfig c
  }
```

The monoid is quite clear from here, but I did not understand the
`def { keys = ... }` part. After looking at the tutorial from XMonad for
configuring: [XMonad Doc Configuring](https://bit.ly/2Il3kAO), I discovered
that this was using **record update syntax**. This allows you to change fields
of a record type, e.g.

```shell
Prelude> data User = User { name :: String, age :: Integer } deriving Show
Prelude> let a = User { name = "John", age = 33 }
Prelude> let b = b { name = "Jane" }
Prelude> a
User { name = "John", age = 33 }
Prelude> b
User { name = "Jane", age = 33 }
```

## 19.3 - lifting over web monad

I don't quite understand how this works. I believe it should be a single fmap.
I'm even more convinced after looking at the **snap** library:

```haskell
getRequest :: MonadSnap m => m Request
getHeader :: HasHeaders a => CI ByteString -> a -> Maybe ByteString
```

`UserAgent` is probably defined as `type UserAgent = ByteString and with
`OverloadedString`, we do have:

```haskell
userAgent' :: Request -> Maybe UserAgent
userAgent' req = getHeader "user-agent" req
```

Which is correct. In order to apply the `userAgent'` function to the
`getRequest`, we only to need lift over one structure, not two as `getRequest`
only has one structure.