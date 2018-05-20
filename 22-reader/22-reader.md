# 22 Reader

## 22.2 Short Exercise: Warming up

[src/warmingup.hs](./src/warmingup.hs)

## 22.5 Excercise: Ask

[src/ask.hs](./src/ask.hs)

## 22.6 Exercise: Reading Comprehension

[src/readingcomp.hs](./src/readingcomp.hs)

## 22.7 Exercise: Reader Monad

[src/readingcomp.hs](./src/readingcomp.h)

## 22.9 You can change what comes below, but not above

Trying to understand the following quote:

> You can swap in a different type or value of `r` for function that you call,
> but not for functions that call you.

Perhaps a better way of saying this would be:

> You can can choose the input for a function you are calling, but cannot
> change the input from within the function.

The input being the context of a `Reader`. This makes sense, consider:

`f :: a -> b`

When we call `f`, we can choose any input value or type to use. However, within
`f`, the value and type are fixed. Immutable. The same applies of course for
`Reader a b` as this is just a `newtype` for `f`.

```haskell
f :: Reader Integer String
f = do
    r <- ask
    -- we cannot change r, but we can do operations on them
    return $ show (r * r)

-- However, we can change the input for f when we call it:
g :: Reader Integer String
g = do
    r <- ask
    -- either via:
    -- return $ runReader f (r + 1)
    -- or:
    withReader (+1) f
```

## 22.11 Chapter exercises

### A warmup stretch

[src/ReaderPractise.hs](./src/ReaderPractise.hs)

### Rewriting Shawty

No idea if this is what was requested, but it works:

[shawty-prime/app/Main.hs](./shawty-prime/app/Main.hs)