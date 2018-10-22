# MTL Style Monads

This document gives an introduction for what MTL style monads offer and some
motivation for what problems it solves. "MTL" comes from the
[`mtl`](http://hackage.haskell.org/package/mtl) Haskell package: the Monad
Transformer Library.

## Two separate problems

Let's say you have two people working on separate libraries. One person is
writing a library to allow programs to pass some configuration or environment
to every function:

```haskell
-- | Passes a value of type `r` to every function. The final result of the
-- computation is of type `a`.
newtype Reader r a = Reader { ... }

-- | Returns the stored value passed to every function.
ask :: Reader r r

-- | Runs a `Reader` with the given value.
runReader :: r -> Reader r a -> IO a
```

The other person is writing a library to allow programs to keep track of log
messages:

```haskell
-- | Allows tracking messages. The final result of the computation is of type
-- `a`.
newtype Writer a = Writer { ... }

-- | Add the given message to the log.
tell :: String -> Writer ()

-- | Runs a `Writer`, returning the consolidated messages.
runWriter :: Writer a -> IO (a, [String])
```

These two separate packages should not care about the other package. The
`Writer` package shouldn't need to think about the case of people passing
configuration around the program, and the `Reader` package shouldn't need to
think about tracking messages.

In this current state, how might you write a program that needs to pass around
configuration and also log? You can't just interleave the functions like:

```haskell
do
  str <- ask
  tell $ "Environment contains: " ++ str
  return True
```

Because `ask` is of type `Reader` and `tell` is of type `Writer`, and every
action in a `do` block needs to be the same type. We could define our own
`ReaderWriter` type that re-implements `ask` and `tell`, but that would re-do
work that's already been done, and it doesn't scale. What happens if we want to
add error handling? Reading an environment, writing a log, and error handling
are all orthogonal problems; they each solve a different problem and they
shouldn't care about all of the other possible effects you might want in your
program.

## Type classes and monad transformers

That last example might give some indication about the next step. We would
ideally like to `ask` or `tell` within an arbitrary type. This hints at using
type classes as a solution:

```haskell
class MonadReader r m where
  ask :: m r

class MonadWriter m where
  tell :: String -> m ()
```

Now, we could implement these type classes for each program we write, but we
still have the same problem as before: we don't want to re-implement these
effects every time. What we can do instead is create data types that know how
to do these actions within the context of an arbitrary type, not just within
`IO`, as the concrete examples above give.

```haskell
newtype ReaderT r m a = ReaderT { ... }

runReaderT :: r -> ReaderT r m a -> m a

newtype WriterT m a = WriterT { ... }

runWriterT :: WriterT m a -> m (a, [String])
```

Now, instead of only knowing how to read/log in the `IO` monad, we can now
read/log within any given monad!

We can implement the above type classes just once for these new types:

```haskell
instance Monad m => MonadReader r (ReaderT r m) where
  ask :: ReaderT r m r

instance Monad m => MonadWriter (WriterT m) where
  tell :: String -> WriterT m ()
```

And then we can use these instances in our program *for free*:

```haskell
data Config = Config { ... }

type MyApp = ReaderT Config (WriterT IO)

runMyApp :: Config -> MyApp a -> IO (a, [String])
runMyApp config app =
  runWriterT                -- WriterT IO a -> IO (a, [String])
    $ runReaderT config     -- ReaderT Config (WriterT IO) a -> WriterT IO a
    $ app                   -- ReaderT Config (WriterT IO) a

foo :: MyApp String
foo = do
  config <- ask
  ...

bar :: MyApp ()
bar = tell ...
```

`MyApp` automatically gets `MonadReader Config` because
`ReaderT Config (WriterT IO)` matches the `MonadReader r (ReaderT r m)`
instance (setting `Config` for `r`). `MyApp` also automatically gets
`MonadWriter` because `MonadWriter` also has the following instance:

```haskell
instance MonadWriter m => MonadWriter (ReaderT r m) where
  tell = lift . tell
```

(`lift` is something like `WriterT m a -> ReaderT r m a`. You can think of it
as magic for now, or check out
[MonadTrans](https://www.stackage.org/haddock/lts-12.14/transformers-0.5.5.0/Control-Monad-Trans-Class.html#t:MonadTrans)
for more details).

These types are called "monad transformers" because they transform a given
monad by providing it another "effect". `ReaderT r m a` transforms the monad
`m` by implementing `MonadReader r` to provide the `ask` function.
`WriterT m a` transforms the monad `m` by implementing `MonadWriter` to provide
the `tell` function. You can then "stack" effects upon each other by wrapping
subsequent monad transformers within each other with the `m` type parameter.

## Design practices for monad stacks

Let's say you're building a program using the `MyApp` type above. You want to
pass configuration to every function and be able to consolidate outputs from
the functions. What you could do is use the `MyApp` type explicitly everywhere:

```haskell
getUser :: MyApp String
getUser = do
  config <- ask
  return $ userFromConfig config

plusOne :: Int -> MyApp Int
plusOne x = do
  tell $ "Incremented: " ++ show x
  return $ x + 1

getUserId :: MyApp Int
getUserId = do
  name <- getUser
  plusOne $ length name
```

But it would be better if you instead listed the exact type instances you need.
Or, as I like to talk about it, list the exact *capabilities* a function
requires in order to run:

```haskell
getUser :: MonadReader Config m => m String

plusOne :: MonadWriter m => Int -> m Int

getUserId :: (MonadReader Config m, MonadWriter m) => m Int
```

This provides the following benefits:

* Easier to test
    * Imagine testing `plusOne` above with `MyApp` specified. You would need to
      create a dummy `Config` in order to run `runMyApp config $ plusOne x`,
      even though the function doesn't even use the config.
    * It's easier to mock out effects; `MonadLaunchNukes MyApp` might do some
      production logic, whereas `MonadLaunchNukes TestApp` can do a fake action
      and return a suitable response.
* Easier to refactor
    * You can skim your code and see how much of your code base actually uses
      the configuration, or needs to log
    * If you refactor out the `tell` line in `plusOne`, you can easily see that
      `plusOne` doesn't need `MonadWriter m` anymore and can be pure now,
      whereas before, you don't know if you required effects within `MyApp`
      other than `MonadWriter`.
* Easier to extend
    * Let's say you later want to make this code a library for others and allow
      them to run your super special `plusOne` code. With the explicit `MyApp`
      type, you force them to use `runMyApp` to run `plusOne`, when in fact,
      `plusOne` only requires the monadic context to be able to run `tell`. If
      you just use a constraint, anyone can drop in `plusOne` whenever they
      have a monad that can run `tell`.

One of the best articles I've read that talks about this is the
[Three Layer Haskell Cake](http://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html).

## Additional reading

Here are some more resources I've found that are helpful:

* [`ReaderT`](https://www.stackage.org/haddock/lts-12.14/mtl-2.2.2/Control-Monad-Reader.html) on Stackage
* [`WriterT`](https://www.stackage.org/haddock/lts-12.14/mtl-2.2.2/Control-Monad-Writer-Strict.html) on Stackage
* https://ocharles.org.uk/posts/2016-01-26-transformers-free-monads-mtl-laws.html
* https://robots.thoughtbot.com/refactoring-to-a-monad-transformer-stack
