# 27 Nonstrictness

## 27.5 Exercises: Evaluate

Excercise 1

```haskell
const 1 undefined
(\a -> \b -> a) 1 undefined
(\b -> 1) undefined
1
```

Exercise 2

```haskell
const undefined 1
(\a -> \b -> a) undefined 1
(\b -> undefined) 1
undefined
```

Exercise 3

```haskell
flip const undefined 1
(\f -> \a -> \b -> f b a) const undefined 1
(\a -> \b -> const b a) undefined 1
(\b -> const b undefined) 1
const 1 undefined -- same as Exercise 1 now
```

Exercise 4 follows same principle as Exercise 3 but leads to Exercise 2.

Exercise 5 follows same pattern as before but where 1 is undefined.

Exercise 6

```haskell
foldr const 'z' ['a'..'e']
const 'a' (foldr const 'z' ['b'..'e'])
'a'
```

Exercise 7

```haskell
foldr (flip const) 'z' ['a'..'e']
(flip const) 'a' (foldr (flip const) 'z' ['b'..'e'])
const (foldr (flip const) 'z' ['b'..'e']) 'a'

foldr (flip const) 'z' ['b'..'e']
(flip const) 'b' (foldr (flip const) 'z' ['c'..'e'])
const (foldr (flip const) 'z' ['c'..'e']) 'b'

foldr (flip const) 'z' ['c'..'e']
(flip const) 'c' (foldr (flip const) 'z' ['d','e'])
const (foldr (flip const) 'z' ['d','e']) 'c'

foldr (flip const) 'z' ['d','e']
(flip const) 'd' (foldr (flip const) 'z' ['e'])
const (foldr (flip const) 'z' ['e']) 'd'

foldr (flip const) 'z' ['e']
(flip const) 'e' (foldr (flip const) 'z' [])
const (foldr (flip const) 'z' []) 'e'

foldr (flip const) 'z' []
'z'
```

## 27.14 Chapter Exercises

### What will :sprint output

1. `x = _`
2. `x = "1"`
3. `x = _`
4. `x = 1`
5. `x = _`
6. `x = _`

### Will printing this expression result in bottom

1. No
2. Yes
3. Yes
4. No
5. No
6. No
7. Yes

### Make the expression bottom

There are different ways of doing this. This is one of them:

```haskell
x = undefined
y = "blah"
main = do
    print (snd (x `seq` (x,y)))
```
