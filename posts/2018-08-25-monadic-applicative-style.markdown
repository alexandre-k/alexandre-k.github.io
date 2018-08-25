---
title: Applicative and monadic styles
tags: monad, applicative, haskell, return, do, fmap, apply
---

# Applicative and monatid styles


---
## Abstract

I will cover different ways in Haskell of doing the same thing, with
the monadic and applicative style.

---

# Fmap and Return

Coming from Python, I needed some time to wrap my mind around
`return`. It has nothing to do with `return` in Python.

Let us say we want to compute an age any given integer entered from
the command line. We could use the `fmap` function:

```

displayAge :: Maybe Integer -> IO ()
displayAge age =
  case age of
    Nothing -> putStrLn "You did not enter a valid date"
    Just theYear -> putStrLn $ "In 2020 you will be " ++ show theYear


computeAge :: Integer -> Integer
computeAge year = 2020 - year

main :: IO ()
main = do
  putStrLn "Enter you birth date:"
  year <- getLine
  let computedAge = fmap computeAge (readMay year :: Maybe Integer)
  displayAge computedAge
```

In this example, we use `fmap` to compute a value wrapped up inside a
`Maybe`. It won't compute anything if it is Nothing. Then it would
display "You did not enter a valid date". `fmap` is really a powerful
way in Haskell to easily get around a wrapped value and make any
computation given a value.

The other equivalent way is the do-notation, where instead of using
`fmap` we unwrap a value with the slurp operator, `<-`, and then feed
it to `return`, which is actually a function, not just a keyword to
return a value like in Python. So the following bit is strictly
equivalent to the previous one:

```
displayAge :: Maybe Integer -> IO ()
displayAge age =
  case age of
    Nothing -> putStrLn "You did not enter a valid date"
    Just theYear -> putStrLn $ "In 2020 you will be " ++ show theYear

computeAge :: Integer -> Integer
computeAge year = 2020 - year

main :: IO ()
main = do
  putStrLn "Enter you birth date:"
  year <- getLine
  let computedAge = do
        theYear <- readMay year :: Maybe Integer
        return $ computeAge theYear
  displayAge computedAge
```

The do-notation being used only in the case of Monads. Further
explanations are in [this wonderful
post](https://www.schoolofhaskell.com/school/advanced-haskell/functors-applicative-functors-and-monads).

# Monadic vs applicative


Now if we want to pass several arguments to our computeAge function,
we would use multiple times the slurp operator in our do
block. Nothing fancy here:

```
displayAge :: Maybe Integer -> IO ()
displayAge age =
  case age of
    Nothing -> putStrLn "You did not enter a valid date"
    Just theYear -> putStrLn $ "In 2020 you will be " ++ show theYear

computeAge :: Integer -> Integer -> Integer
computeAge futureYear year = futureYear - year

main :: IO ()
main = do
  putStrLn "Enter you birth date:"
  year <- getLine
  putStrLn "Enter a future year"
  futureYear <- getLine
  let computedAge = do
        theYear <- readMay year :: Maybe Integer
        theFutureYear <- readMay futureYear :: Maybe Integer
        return $ computeAge theFutureYear theYear
  displayAge computedAge
```

As I said, nothing fancy, quite a beginner-friendly way of using
wrapped values.

Now let's say we want to use `fmap`. It basically takes one function
and one `Maybe Integer` to return one `Maybe a`. So to use fmap, we
would need to curry the function or to use apply, the `<*>` operator
in Haskell. Why? The apply operator, `<*>`, apply a function inside a
functor (think of `computeAge` as a curried function) to a value
inside a functor (our `Maybe Integer`).

Then our previous code in the monadic style would become this, in an
applicative style:

```
displayAge :: Maybe Integer -> IO ()
displayAge age =
  case age of
    Nothing -> putStrLn "You did not enter a valid date"
    Just theYear -> putStrLn $ "In 2020 you will be " ++ show theYear

computeAge :: Integer -> Integer -> Integer
computeAge futureYear year = futureYear - year

main :: IO ()
main = do
  putStrLn "Enter you birth date:"
  year <- getLine
  putStrLn "Enter a future year"
  futureYear <- getLine
  let theYear = readMay year :: Maybe Integer
  let theFutureYear = readMay futureYear :: Maybe Integer
  let computedAge = fmap computeAge theFutureYear
                        <*> theYear
  displayAge computedAge

```

Not so much beginner friendly style I would say, although it looks
nice if we replace `fmap` with its infix equivalent, `<$>`:

```
let computedAge = computeAge
                        <$> theFutureYear
                        <*> theYear
```

# To lift or not to lift

Altough we could stop here, let us rewrite our monadic version with `liftM2`:

```
main :: IO ()
main = do
  putStrLn "Enter you birth date:"
  year <- getLine
  putStrLn "Enter a future year"
  futureYear <- getLine
  displayAge $ liftM2 computeAge (readMay futureYear :: Maybe Integer)
                                 (readMay year :: Maybe Integer)
```

This is strictly equivalent to what we did before. I just applied
`liftM2` to replace the do notation. Since I have a `Maybe` type and
the monadic actions depend on the previous one, I use `liftM2` instead
of `liftM`. See [the
wiki](https://wiki.haskell.org/Applicative_functor) for more details.

Still, what is the purpose of lifting something in Haskell? Will see
that in a next post.
