# Functors and Family

This document will go over the `Functor` type class and some of its associated
type classes:

* `Bifunctor`
* `Contravariant`
* `Profunctor`

This reference is recommended for anyone who understands typeclasses. Knowledge
of `Functor` is not required, as each section hopefully serves to prepare you
for each subsequent section.

## Functor

The `Functor` typeclass is roughly defined as:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Colloquially speaking, a `Functor` is a typeclass for all types that *contain*
data. For example, `Maybe` is a `Functor`:

```haskell
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f (Just x) = Just $ f x
  fmap _ Nothing = Nothing
```

`Maybe a` is a type that (possibly) contains data of type `a`. Looking at the
definition above, `fmap` allows us to convert the contained data into another
type. For example,

```haskell
fmap (+ 1) (Just 2) == Just 3
fmap (+ 1) Nothing  == Nothing

fmap show (Just 2) == Just "2"
fmap show Nothing  == Nothing
```

Step back for a moment and think about what this means. We can run a function
on data while keeping the structure containing that data intact; in this case,
we can run a given function on a piece of data if the data exists, or don't do
anything if it doesn't! Take a look at these two ways of manipulating `Maybe`
values:

```haskell
-- without Functor
myFunc :: Maybe Int -> Maybe (String, [String], String)
myFunc maybeX =
  let res1 = case maybeX of
        Just res -> Just $ show res
        Nothing -> Nothing
      res2 = case res1 of
        Just res -> Just [res]
        Nothing -> Nothing
      res3 = case maybeX of
        Just res -> Just $ show $ res + 1
        Nothing -> Nothing
  in (res1, res2, res3)

-- with Functor
-- `<$>` is an alias for fmap
myFunc :: Maybe Int -> Maybe ([String], String)
myFunc maybeX =
  let res1 = show <$> maybeX
      res2 = (:[]) <$> res1
      res3 = show . (+ 1) <$> maybeX
  in (res1, res2, res3)
```

Another example of this "converting data without changing the structure" idea
that might come to mind is the List type:

```haskell
instance Functor [a] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap = map
```

Yes: `fmap` is just `map` for lists! This should make intuitive sense; `map`
converts everything in a list to another type, which is exactly what `fmap`
does: convert types *within* a structure.

One more example: `IO`. This makes for an interesting example, because the
internals of `IO` are rather mysterious, and we don't really think about how
`IO` is defined. However, when reading `IO String`, we understand this to mean
that this is some `IO` action (e.g. goes to the filesystem, reads an
environment variable, etc.) that eventually returns a `String`. With the
`Functor` instance for `IO`, we now have the tool to convert data inside of an
`IO` -- all without needing to know how `IO` stores that `String`!

```haskell
-- without Functor
userEnv <- getEnv "USER"
let user = capitalize userEnv

-- with Functor
user <- capitalize <$> getEnv "USER"
```

Now with `Functor`, we don't have to think of a good name for the intermediate
value `userEnv`, and we don't have to make a mental note about what is stored
in `userEnv`.

Note the similarity between using the `<$>` alias and the `$` operator:

```haskell
-- without Functor
time <- getSystemTime
let hour = show . toInt . getHour $ time
putStrLn $ "It is roughly " ++ hour ++ ":00"

-- with Functor
hour <- show . toInt . getHour <$> getSystemTime
putStrLn $ "It is roughly " ++ hour ++ ":00"
```

## Bifunctor

A `Bifunctor` is just like a `Functor`, except the type contains two pieces of
data.

```haskell
class Bifunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
  first :: (a -> c) -> f a b -> f c b
  second :: (b -> d) -> f a b -> f a d

  -- defaults
  first f = bimap f id
  second g = bimap id g
```

To make this a little more understandable, let's do an example with the
2-tuple:

```haskell
instance Bifunctor (,) where
  bimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
  bimap f g (x, y) = (f x, g y)

  -- explicitly defining for clarity; note how this is equivalent to the
  -- default implementations
  first f (x, y) = (f x, y)
  second g (x, y) = (x, g y)
```

As you can see, `bimap` takes in two functions, each function mapping each
piece of data in the tuple.

```haskell
bimap (+ 1) head (3, "asdf") == (4, 'a')

first (+ 1) (3, "asdf") == (4, "asdf")
second head (3, "asdf") == (3, 'a')
```

`Either` is also a `Bifunctor`. `Either` is a bit more complicated than the
tuple, because the two types of data don't appear at the same time. What
`Bifunctor` means for `Either` is that it lets us run functions depending on
whether it's a `Left` or a `Right`. This effectively lets us use `first` to
change the value in a `Left` and do nothing if it's a `Right`, and vice-versa
for `second` (note the similarities with `Functor` and `Maybe`).

```haskell
instance Bifunctor Either where
  bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right y) = Right $ g y

first show (Left 3) == Left "3"
first show (Right 2) == Right 2

second isOdd (Left "foo") == Left "foo"
second isOdd (Right 3) == Right True

-- without Bifunctor
showRight x = case x of
  Left a -> Left a
  Right b -> Right $ show b

-- with Bifunctor
showRight = second show
```

## Conduit

Before moving on to the more complex typeclasses, I'd like to introduce a type
that will be useful for understanding the future sections: `Conduit`. Note: the
`Conduit` type described here is a simplified version of the
[actual type](http://hackage.haskell.org/package/conduit-1.3.0.3/docs/Data-Conduit.html#t:ConduitT).

`Conduit i o` represents a sort-of pipe that can consume a stream of input
values of type `i` and produce a stream of output values of type `o`. For
example, you could write a function `readFileLinesC :: Conduit () String` that
would open a file and stream it line-by-line.

Some more examples of Conduit functions might be:

```haskell
yieldC :: [a] -> Conduit () a
putStrLnC :: Conduit String ()
mapC :: (a -> b) -> Conduit a b
filterC :: (a -> Bool) -> Conduit a a
plusOneC :: Num a => Conduit a a
```

And then we can connect our `Conduit`s together with
`fuse :: Conduit a b -> Conduit b c -> Conduit a c`. With the alias `.|`, we
can make readable pipelines:

```haskell
yieldC [1..10] .| filterC isOdd .| mapC (("Hello: " ++) . show) .| putStrLnC
-- prints:
--   Hello: 1
--   Hello: 3
--   Hello: 5
--   Hello: 7
--   Hello: 9
```

With this framework, we can think of a `Functor` `f a` as being a
`Conduit () a`. `[a]` is a type that produces (quite literally) a list of `a`s.
`Maybe a` is a type that produces one or none of `a`, and `IO a` is a type that
does some `IO` action and produces an `a`.

## Contravariant

We introduced the `Functor` typeclass above; in mathematics, there are actually
different types of functors than the one we defined above. In fact, Haskell's
`Functor` typeclass is known as a *covariant functor* in mathematics. The
`Contravariant` typeclass represents another kind of mathematical functors,
*contravariant functors*.

Instead of starting out with the Haskell definition, let's simply define
`Contravariant a` as `Conduit a r` for some defined type `r`. Remember how
`Functor` can be though of as `Conduit () a`, where a `Functor` `f a`
represents something that produces `a` values. Conversely, a `Contravariant`
`f a` represents something that consumes `a` values, and possibly returns `r`.

```haskell
data Predicate a = Predicate { getPredicate :: a -> Bool }
```

This example shows a `Predicate` type, where `Predicate String` represents some
function that takes an `String` and returns a `Bool`. Similarly,
`Predicate Int` represents some function that takes an `Int` and returns a
`Bool`. For example:

```haskell
isOdd :: Predicate Int
isOdd = Predicate ((== 1) . (`mod` 2))

startsWith :: a -> Predicate [a]
startsWith x = Predicate ((== x) . head)

getPredicate isOdd 1 == True
getPredicate isOdd 2 == False
getPredicate (startsWith 'a') "asdf" == True
getPredicate (startsWith 1) [2,3,4] == False
```

But now, what if you want to modify a value before running the `Predicate` on
it? Enter `contramap`:

```haskell
isOddNumber :: Predicate String
isOddNumber = contramap read isOdd

getPredicate isOddNumber "1" == True
getPredicate isOddNumber "2" == False
```

The type of `contramap` might be confusing at first, but try to look at the
similarities between `contramap` and (the flipped form of) function composition
`(.)`:

```haskell
class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

flip (.) :: (b -> a) -> (a -> c) -> (b -> c)
```

`contramap` looks similar to `flip (.)`, if it were specialized to the type
`(-> c)`. You can think of `contramap` as being function composition for
`Contravariant`s. This is more explicit when implementing `contramap` for
concrete types:

```haskell
instance Contravariant Predicate where
  contramap :: (b -> a) -> Predicate a -> Predicate b
  contramap g (Predicate f) = Predicate (f . g)
```

Think a bit on that example. `Predicate a` means "this function takes in `a`
values". `Predicate b` means "this function takes in `b` values". To connect
the two, we provide a function `b -> a` that tells `Predicate b` how to take in
a `b`, convert it into an `a`, and then pass that into the original
`Predicate a`.

Thinking more on the similarities with the `Conduit` representation:

```haskell
-- Conduit
mapC read           :: Conduit String Int
isOddC              :: Conduit Int Bool
mapC read .| isOddC :: Conduit String Bool

-- Predicate
read                   :: String -> Int
isOdd                  :: Predicate Int
read `contramap` isOdd :: Predicate String
```

You start to see a pattern like `type Predicate a = Conduit a Bool`, where `a`
represents something to be consumed as input rather than an output.

Another example might be:

```haskell
data Handler a = Handler { runHandler :: a -> IO () }

handleErrors :: Exception e => Handler e
handleErrors = Handler print

handleFile :: FilePath -> Handler String
handleFile = Handler . writeFile
```

Again, the type variable `a` represents something that will be consumed with
this data type rather than something stored in the data type.

## Profunctor

Lastly, a `Profunctor` is simply a `Functor` with two type variables, the
first being contravariant and the second being covariant. Hint: you've already
seen a `Profunctor`: `Conduit`! `Conduit a b` represents a type that consumes
an `a` (contravariant) and produces a `b` (covariant).

```haskell
class Profunctor f where
  dimap :: (c -> a) -> (b -> d) -> f a b -> f c d

instance Profunctor Conduit where
  dimap :: (c -> a) -> (b -> d) -> Conduit a b -> Conduit c d
  dimap f g conduit = mapC f .| conduit .| mapC g
```

Notice how `dimap` treats the first function as a preprocessor (modifying the
input before running the `Conduit`) and the second function as a postprocesser
(modifying the output after running the `Conduit`). So at the end of running
`dimap`, you get the following data flow:

* Give `c` as input
* Modify with `f` to get an `a`
* Call the conduit to get a `b`
* Modify with `g` to get a `d`

Here, the first and last bullet represent the type of the overall `Conduit`:
`Conduit c d`. The second and third bullet represent the original `Conduit`,
before `dimap`: `Conduit a b`.

Another easy example is `(->)`, the arrow type. A function of type `(->) a b`
(i.e. `a -> b`) is something that consumes an `a` and produces a `b`.

```haskell
instance Profunctor (->) where
  dimap :: (c -> a) -> (b -> d) -> (a -> b) -> (c -> d)
  dimap f g func = g . func . f
```

Here, the data flow is a bit more obvious, since it's literally function
composition.

## Conclusion

Hopefully this makes `Functor`s and its related typeclasses a little more
understandable. It may be completely over your head at this point, but after
thinking about it more and using `Functor` and `fmap` more, you'll start to
understand this better and better.

I got a lot of inspiration from this
[School of Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/profunctors)
article. It does a really good job of explaining all of this as well, so take
a look at that if you want to learn more.

This [article](https://www.fpcomplete.com/blog/2016/11/covariance-contravariance)
also explains the same concepts in much the same format.
