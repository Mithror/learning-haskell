# 24 Parser Combination

## 24.3 Parsing practise

[src/LearnParsers.hs](./src/LearnParsers.hs)

## 24.4 Exercise: Unit of Success

`(>>)` is actually `(*>)` from `Applicative` and `Applicative` also has `(<*)`
which has type `Applicative f => f a -> f b -> f a` So the solution here is:
`integer <* eof`

## 24.6 Exercise: Try Try

[src/TryTry.hs](./src/TryTry.hs)

## 24.11 Chapter Exercises

1. [src/SemVer.hs](./src/SemVer.hs)
2. [src/PosInt.hs](./src/PosInt.hs)
3. [src/PosInt.hs](./src/PosInt.hs)
4. [src/PhoneNumber.hs](./src/PhoneNumber.hs)
5. [src/ParseLog.hs](./src/ParseLog.hs)
6. [src/IPAddress.hs](./src/IPAddress.hs)
7. [src/IPAddress.hs](./src/IPAddress.hs)
8. [src/IPAddress.hs](./src/IPAddress.hs)
9. [src/IPAddress.hs](./src/IPAddress.hs)
10. Probably not going to finish this at this time. It's a lot of the same
    but just a lot more.