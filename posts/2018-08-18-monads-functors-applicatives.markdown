---
title: Basics about functors
---


---
## Abstract

I will cover simple use cases as a memo of what I have learned about
pretty basic stuff related to functors (monads, applicatives).

Nothing crazy, just a kind of tutorial to wrap my mind around
fundamentals.

The whole with functional programming being composition of function, I
will divide this post in sections based on this idea.

---

# Composing functions returning values

Let's say we have 2 functions, one doubling a value, the other one
tripling it. How would we easily compose it?

```
Prelude> double x = x * 2
Prelude> triple x = x * 3
Prelude> double $ triple 2
12
```
Easy peasy.

But why not just use the dot operator?
```
Prelude> (double . triple) 2
12
```
Same shit, just we have to use parentheses to avoid an error.

# Composing functions with wrapped values

Now let's say we got a casual function, and we got a wrapped value. How do we compose that?

We reuse the same double and triple functions, and this time we feed it a `Just 5`.
```
Prelude> double (Just 5)

<interactive>:34:1: error:
    • Non type-variable argument in the constraint: Num (Maybe a)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a. (Num a, Num (Maybe a)) => Maybe a
```

Houston, we've got a problem! Maybe values being actually instances of
functors, we have to use `fmap`, or `<$>`, the infix operator for
`fmap`.

```
Prelude> fmap double (Just 5)
Just 10
```

Or with the infix operator:
```
Prelude> double <$> (Just 5)
Just 10
```
Same shit.

The point here is to remember that `fmap` is used when we want to
apply a casual function and we want to feed it a wrapped value
(functors under the hood).

Like byte strings in Python displayed as letters, it is under the hood
just bytes, and displayed as letters to appear more
user-friendly. Here, we are dealing with functors, even though it is
displayed as a friendly `Just 10`.

Note that fmap would fail with primitive values, since it can only be
used with wrapped values (that is, functors):

```
Prelude> (triple . double) <$> (Just 4)
Just 24
Prelude> (triple . double) <$> 4

<interactive>:69:1: error:
    • Ambiguous type variable ‘f0’ arising from a use of ‘print’
      prevents the constraint ‘(Show (f0 Integer))’ from being solved.
      Probable fix: use a type annotation to specify what ‘f0’ should be.
      These potential instances exist:
        instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
        instance (Show a, Show b) => Show (a, b) -- Defined in ‘GHC.Show’
        instance (Show a, Show b, Show c) => Show (a, b, c)
          -- Defined in ‘GHC.Show’
        ...plus 13 others
        ...plus two instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In a stmt of an interactive GHCi command: print it

```

# Composing functions with wrapped values on both sides

Now with wrapped values only, we got two wrapped values, we want to add it:
```
Prelude> pure (+) <*> Just 5 <*> Just 5
Just 10
```

We used the applicative to add 2 functors. This is basically the
example from the online book [Learn you a
Haskell](http://learnyouahaskell.com/functors-applicative-functors-and-monoids).

But the point is, it works for many things like lists, which are
functors too under the hood. So let's say we want to double each
element of a list, we can apply it sequentially:

```
Prelude> pure double <*> [1,2,3]
[2,4,6]
```
Nice!

We can see in
[Hackage](http://hackage.haskell.org/package/base-4.11.0.0/docs/Prelude.html#g:10)
what are the types implementing a Functor already available in the
Prelude. There are many: lists, Maybe, IO, Sum, Product, ZipList,
Last, Max, etc.

:warning: I still need more experience to get a better grasp of real
world uses of applicatives to write something non trivial and useful.

# Composing functions returning wrapped values

I have a double function returning a Maybe value.
```
stack exec -- ghci
GHCi, version 8.4.3: http://www.haskell.org/ghc/  :? for help
Prelude> double x = Just ( x * 2 )
Prelude> double 2
Just 4
```
`double 2` returns `Just 4`. Let's say I also got a triple function.
```
Prelude> triple x = Just ( 3 * x )
Prelude> triple 5
Just 15
```

`triple 5` gives `Just 15`, nothing crazy. But now I want to triple
and then double a value. For that, we use `=<<`, the bind operator.

```
Prelude> double =<< triple 2
Just 12

```
Or in the oppposite direction, same shit:
```
Prelude> triple 2 >>= double
Just 12
```

We could have used fmap for that, but it would have given us something
we don't want:

```
Prelude> double <$> triple 2
Just (Just 12)
```
After callback hells, we got a "Maybe hell".

So basically, we applied an operator to functions which return wrapped
values, not just a primitive type like integer, char or
whatever. [This bind operator was a monad, something used for
functions returning wrapped values, nothing else, nothing
more.](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html#monads)
