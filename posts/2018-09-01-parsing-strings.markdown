---
title: Parsing strings in clojure
---

# Basics about parsing strings in clojure


---
## Abstract

I will cover how to parse nested data structures contained in a string in clojure.

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
