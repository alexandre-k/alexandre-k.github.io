---
title: Basics about functors
---

# Basics about IO Monads, transformers and do syntax


---
## Abstract

I will cover how we interact with IO, parse from it, read into a
custom data type strings.

---

# Getting data from the command line

```
module Main where


data Person = Person { firstName :: String
                     , lastName  :: String
                     , age       :: Int
                     } deriving Show


readPerson :: IO [String]
readPerson =
  words <$> getLine


createPerson :: [String] -> Person
createPerson [a, b, c] = Person a b (read c :: Int)


main :: IO ()
main =
  do
    person <- readPerson
    putStrLn $ show $ createPerson person
```

Pretty basic stuff. We read 3 words from the command line. It is read
as an `IO String`. We parse it to get a list, so `readPerson` returns
a `IO [String]`. We want to convert it into a specific data structure,
a `Person`, which takes his firstname, lastname and age.

Pretty simple. Still, the left arrow assigns a function and runs a
computation. Under the hood the `<-` operator unwraps the value on the
right-hand side from getLine. See [Morning Monday
Haskell](https://mmhaskell.com/monads-3), there is a detailed
explanation.

If you play with the code in GHCI, I would add also that `<-` makes
the action occur, while with just the assignment operator, `=`,
nothing is unwrapped from the monad.

```
Prelude> :load app/Main
[1 of 1] Compiling Main             ( app/Main.hs, interpreted )
Ok, one module loaded.
*Main> person = readPerson
*Main> person
John Doe 24
["John","Doe","24"]
*Main> person <- readPerson
John Doe 24
*Main> person
["John","Doe","24"]
```

When we assign readPerson to person, readPerson is not yet executed,
but is run as we run person, the function is just given a new
name. With `<-`, readPerson executes getLine and then returns an `IO
[String]`.

# Getting safety

Once we got our person from the command line, we still have the possibility of running into an invalid input.

```
*Main> person <- readPerson
John
*Main> person
["John"]
*Main> createPerson person
*** Exception: app/Main.hs:17:1-51: Non-exhaustive patterns in function createPerson
```

Oops... Let's use `Either` to change from the usual Maybe type we see
on the Internet. As written in [Real World
Haskell](http://book.realworldhaskell.org/read/error-handling.html),
we can define our own custom exception data type, although we could
just print a string instead. Before creating a person, we check if the
input is as expected. If not, we return an InvalidInputPerson.

```
module Main where

data InvalidInput = InvalidInputPerson deriving Show

data Person = Person { firstName :: String
                     , lastName  :: String
                     , age       :: Int
                     } deriving Show



readPerson :: IO [String]
readPerson =
  words <$> getLine


createPerson :: [String] -> Either InvalidInput Person
createPerson [a, b, c] = Right (Person a b (read c :: Int))
createPerson _ = Left InvalidInputPerson


main :: IO ()
main =
  do
    person <- readPerson
    case createPerson $ person of
      Left InvalidInputPerson ->
        putStrLn "Please, enter a correct person as:\nfirstname lastname age"
      Right thePerson ->
        putStrLn $ show thePerson
```

Now if we try again to enter wrong input, like just John, we will get
our error message:

```
*Main> main
John
Please, enter a correct person as:
firstname lastname age
*Main> main
John Doe 24
Person {firstName = "John", lastName = "Doe", age = 24}
```

Although it is already a good step forward, let us just note that it
is far from being robust, as we could enter a wrong number which will
make raise an unexpected exception:

```
*Main> main
John Doe 23^?4
Person {firstName = "John", lastName = "Doe", age = *** Exception: Prelude.read: no parse
```

But we won't look for any edge case, as we just want to write a
program to ticker around with Haskell.
