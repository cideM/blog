---
title: Understanding unliftio
date: '2019-05-30'
publish: true
---

In this post I will go over the source code for [unliftio-core](https://www.stackage.org/package/unliftio-core), more specifically the `MonadUnliftIO` typeclass, found in `Control.Monad.IO.Unlift`. The typeclass is used to power the actual [unliftio](https://www.stackage.org/package/unliftio) library, which is the one you'd use in your applications.

The typeclass itself isn't too big nor too complicated but it took me a while to wrap my head around what's really happening under the hood. To improve my own understanding, I wrote this blog post and I hope that it can make for an interesting or even helpful read.

I'll borrow the examples from the `unliftio` documentation.

## `unliftio` in a Nutshell

You have the following function -- which gives you an `IO ()` -- but you need to run it inside a function returning `ReaderT env IO ()`

```haskell
foo :: String -> IO ()
foo = print
```

The most straight forward and idiomatic way to translate from `IO a` to `m a` (the `m` here being `ReaderT env`) is `liftIO`.

```haskell
bar :: String -> ReaderT env IO ()
bar = liftIO . print
```

But now you have a function where `IO` occurs in negative position[^1] (see this [post on co- and contravariance](https://www.fpcomplete.com/blog/2016/11/covariance-contravariance)).

```haskell
foo :: (String ->  IO ()) -> IO ()`
                -- ^^^^^ negative position
foo func = func "test"
```

Again, you would like to use a function with `ReaderT env IO ()` instead of a plain `IO ()`. Unfortunately, `liftIO` won't help us here, due to the negative position of the first `IO`. This is where `unliftIO` shines!

```haskell
foo2 :: (String -> ReaderT String IO ()) -> ReaderT String IO ()
foo2 func =
  askUnliftIO >>=
    \u -> liftIO $ foo (unliftIO u . func)
                -- ^^^ note that we're still calling
                -- the original `foo`. We didn't have to
                -- reimplement anything

-- Or alternatively
foo2' func = withRunInIO $ \runInIO -> foo (runInIO . func)
```

Both versions of `foo2` work. `unliftIO` temporarily translates back from `m a` to `IO a` so that we can utilize our `ReaderT` but inside a function expecting plain `IO ()`. In that sense the name "unlift" is quite fitting since it's the opposite of lift.

The `MonadUnliftIO` typeclass has two methods: `askUnliftIO` and `withRunInIO`, either of which suffices for a minimal implementation. We'll go over both, so let's start with `askUnliftIO`.

_*I need to mention the fact that the lib gives us something, either runInIo or the unlifted thing, that we then apply to our own function. That's really important*_

## `askUnliftIO`

`askUnliftIO` is the first typeclass method we'll look at. It has the type signature `m (UnliftIO m)`. Here's a file that you can load into `ghci` to follow along:

_*Mention the newtype wrapper here*_

```haskell
#!/usr/bin/env stack
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
    stack
    script
    --resolver lts-13.16
    --package transformers
-}
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader (ReaderT (..))

test :: (String -> IO ()) -> IO ()
test f = f "test"

test2 :: (String -> ReaderT env IO ()) -> ReaderT env IO ()
test2 f =
  askUnliftIO
   >>= \wrappedUnliftedThing ->
    let unwrapped = unwrap wrappedUnliftedThing
     in liftIO $ test (unwrapped . f)
  where
    unwrap :: UnliftIO m -> m a -> IO a
    unwrap = unliftIO

myPrint :: String -> ReaderT String IO ()
myPrint string = ReaderT $ \env -> print $ env ++ " " ++ string

main :: IO ()
main = runReaderT (test2 myPrint) "Environment"

instance MonadUnliftIO IO where
  askUnliftIO = return (UnliftIO id)

instance MonadUnliftIO m => MonadUnliftIO (ReaderT r m) where
  askUnliftIO =
    ReaderT $ \env ->
      askUnliftIO >>= \unliftedIO
       -> liftIO $
          return (UnliftIO (unliftIO unliftedIO . flip runReaderT env))

newtype UnliftIO m = UnliftIO
  { unliftIO :: forall a. m a -> IO a
  }

class MonadIO m =>
      MonadUnliftIO m
  where
  askUnliftIO :: m (UnliftIO m)
```

Load it with `stack ghci file.hs` and run `main` to see the resulting string "Environment test", consisting of our `env` and the hardcoded string in `test`. Note that we're not importing `unliftio`, instead I simply copy & pasted the minimal, relevant bits from the source code.

_* Mention instances here and then use instance names for headings*_

### Instances

We have two instances for `MonadUnliftIO` in this file, one for `ReaderT` and one for `IO`. The `IO` instance is interesting in the sense that it's almost just the identity function. Unfortunately though there's this `UnliftIO` newtype wrapper, which doesn't provide any useful functionality and exists solely because

> We need to new datatype (instead of simply using a forall) due to lack of support in GHC for impredicative types.

You can pretty much ignore the `UnliftIO` newtype and just treat this instance as if it was just `return id`, because unlifting plain `IO` doesn't require anything special.

Then there's the `ReaderT` instance, which is a bit more involved.

```haskell
instance MonadUnliftIO m => MonadUnliftIO (ReaderT r m) where
  askUnliftIO =
    ReaderT $ \env ->
      askUnliftIO >>= \unliftedIO
   -- ^^^^^^ This is the askUnliftIO from the IO instance
       -> liftIO $ return (UnliftIO (unliftIO unliftedIO . flip runReaderT env))
```

[^1] `IO` also occurs in positive position.
