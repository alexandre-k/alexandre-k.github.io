---
title: Getting data from nested structures
author: Alexandre Krispin
date: Sat 25 Aug 2018 02:35:41 PM JST
tags: lens, Haskell, json, nested, data structure, record
---

# Basics about lenses


---
## Abstract

I will cover how we use lenses to get data from nested data types.

---

# Getting data from nested structures without lenses

Getting data from nested structures can get pretty complicated. In
Python if we had to get data from nested dictionaries, we would do
something like:

```
whateverdic['alex']['address']['street']
```

And to update it, we would do use the assigment operator to assign a
new value. But we don't do it like this in Haskell. How should we do, then?

Let's say we want to define a person having an address. We could do it like this:
```
data Address = Address { street :: String, city :: String } deriving Show
data Person = Person { name :: String, address :: Address } deriving Show
```

Then, we want to show the data. For this, we would have to grab the
data with pattern matching, by writing down the shape of the structure
we want to get the data from, like this:

```
showPerson :: Person -> String
showPerson (Person name (Address street city)) = "Name: " ++ name ++ ", living in " ++ street ++ ", " ++ city
```

To update it, we could use intermediate variables to hold
the point we are at each step we delve into the nested structure, like this:

```
updatePerson :: Person -> String -> String -> Person
updatePerson person newStreet newCity = person { address = Address newStreet newCity}
```

This way could get more cumbersome with a more complex
structure. Another way would be to use the same kind of pattern
matching we used earlier to update the data with new values, like
this:

```
updatePerson2 :: Person -> String -> String -> Person
updatePerson2 (Person name (Address street city)) newStreet newCity = Person name (Address newStreet newCity)
```

But in any cases, this kind of way would not be easy to maintain with the growth of
the data structure used. How do we solve this problem of accessibility of complex structures? We use lenses.

# Get and set nested data with lenses

Ok so in order to make any confusion, we will use the notation `'` after each name. In order to show the flexibility of lenses, we will also add another data type, the `Location'` of a person:

```
data Location' = Location' { _planet' :: String, _country' :: String } deriving Show
data Address' = Address' { _street' :: String, _city' :: String, _location' :: Location' } deriving Show
data Person' = Person' { _name' :: String, _address' :: Address' } deriving Show

```

It's a detail, but you'll notice I added `Location`, it is just to get
a more complex structure.  As you can see, each field has an
underscore. According to the guy behind [Monday Morning
Haskell](https://mmhaskell.com/blog/2017/6/12/taking-a-close-look-at-lenses),
it is a convention.  It is necessary to avoid any collision of names
inside the same namespace. To create lenses for each data type, we use
`makeLenses`:

```
makeLenses ''Location'
makeLenses ''Address'
makeLenses ''Person'
```

Once we've done that, we can use it to get or set data. Let's create
some functions to get specific data:

```
showPerson' :: Person' -> String
showPerson' person = "Name: " ++ person ^. name' ++ ", living in " ++ person ^. address' ^. street' ++ ", " ++ person ^. address' ^. city'
```

We used the operator `^.` to abbreviate view. It is the same thing, so
we can use `view` instead. In the following examples I used it to get
the city, country and planet from our person data structure:

```
getCity' :: Person' -> String
getCity' person = view (address' . city') person

getCountry' :: Person' -> String
getCountry' person = view (address' . location' . country' ) person

getPlanet' :: Person' -> String
getPlanet' person = view (address' . location' . planet') person
```

Quite simple, huh? I like the composition style of the functions, it
makes it easy to follow where we get the data from.

We got some data, but now let's say we want to update it. How do we do
it? We use the set function.

```
updatePerson' :: Person' -> String -> Person'
updatePerson' person newCity = set (address' . city') newCity person
```

Here I used the set function, indicated what should be updated, passed
the newCity argument, and then indicate the `Person'` argument where
we update the data structure. Notice a new object will be returned
since everything is immutable.

Ok, but we want to have a way to update several values in one
function, how to do that? Easy. with the operator `&`, which is the
inverse of `$`:

```
updatePerson2' :: Person' -> String -> String -> Person'
updatePerson2' person newPlanet newCountry =
  person
  & set (address' . location' . planet') newPlanet
  & set (address' . location' . country') newCountry
```

Here we set a new planet and a new country to our person by using set
again, but do it in a row thanks to `&`.

You can update data not only with set but also with over to pass a
function, you can update it in a transversal way, update a `Maybe`
data kind, etc. If you want to know more this, the best resource I
found is an article on Medium, [Haskell Lens Operator
Onboarding](https://medium.com/urbint-engineering/haskell-lens-operator-onboarding-a235481e8fac),
written by Russel Matney. He goes into many details, and his article
is really great for more details on how to use lenses. Once you read
it, you can read [the
tutorial](http://hackage.haskell.org/package/lens-tutorial-1.0.3/docs/Control-Lens-Tutorial.html)
on Hackage if you want yet more detailed information on how things
work.


# Whole source code


The whole source code for the small examples above is here:

```
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens

data Location' = Location' { _planet' :: String, _country' :: String } deriving Show
data Address' = Address' { _street' :: String, _city' :: String, _location' :: Location' } deriving Show
data Person' = Person' { _name' :: String, _address' :: Address' } deriving Show

makeLenses ''Location'
makeLenses ''Address'
makeLenses ''Person'

showPerson' :: Person' -> String
showPerson' person = "Name: " ++ person ^. name' ++ ", living in " ++ person ^. address' ^. street' ++ ", " ++ person ^. address' ^. city'

getCity' :: Person' -> String
getCity' person = view (address' . city') person

getCountry' :: Person' -> String
getCountry' person = view (address' . location' . country' ) person

getPlanet' :: Person' -> String
getPlanet' person = view (address' . location' . planet') person

updatePerson' :: Person' -> String -> Person'
updatePerson' person newCity = set (address' . city') newCity person

updatePerson2' :: Person' -> String -> String -> Person'
updatePerson2' person newPlanet newCountry =
  person
  & set (address' . location' . planet') newPlanet
  & set (address' . location' . country') newCountry

data Address = Address { street :: String, city :: String } deriving Show
data Person = Person { name :: String, address :: Address } deriving Show

showPerson :: Person -> String
showPerson (Person name (Address street city)) = "Name: " ++ name ++ ", living in " ++ street ++ ", " ++ city

updatePerson :: Person -> String -> String -> Person
updatePerson person newStreet newCity = person { address = Address newStreet newCity}
  where
    oldName = name person

updatePerson2 :: Person -> String -> String -> Person
updatePerson2 (Person name (Address street city)) newStreet newCity = Person name (Address newStreet newCity)

getCity :: Person -> String
getCity person = city pAddress
  where
    pAddress = address person


main = do
  putStrLn $ "Person without lens: " ++ showPerson person
  putStrLn $ "Person' with lens: " ++ showPerson' person'
  putStrLn $ "City without lens: " ++ getCity person
  putStrLn $ "City with lens: " ++ getCity' person'
  putStrLn $ "Country with lens: " ++ getCountry' person'
  putStrLn $ "Update without lens: " ++ showPerson newPerson
  putStrLn $ "Update2 without lens: " ++ showPerson newPerson2
  putStrLn $ "Update' with lens: " ++ showPerson' newPerson'
  putStrLn $ "Update2' with lens: " ++ getCountry' newPerson2' ++ " on " ++ getPlanet' newPerson2'
  -- putStrLn $ show $ updatePerson $ person "somewhere else" "Nagoya"
  -- putStrLn $ "New person without lens: " ++ show $
  -- putStrLn $ "Person with lens: " ++ showPerson' newPerson

  -- putStrLn $
  -- putStrLn $ "Nested city with lens: " ++ getCity' newPerson
  -- putStrLn $ "Nested street with lens: " ++ getStreet' newPerson

  where
    person = Person "Alex" (Address "Tateishi" "Tokyo")
    person' = Person' "Alex'" (Address' "Tateishi'" "Tokyo'" (Location' "Earth" "Japan"))
    newPerson = updatePerson person "somewhere" "Osaka"
    newPerson2 = updatePerson2 person "somewhere else" "Nagoya"
    newPerson' = updatePerson' person' "Fukuoka"
    newPerson2' = updatePerson2' person' "Jupiter" "China"
```
