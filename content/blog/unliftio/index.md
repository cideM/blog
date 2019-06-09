---
title: Understanding unliftio
date: '2019-05-30'
publish: true
---

In this post I will go over the source code for [unliftio-core](https://www.stackage.org/package/unliftio-core), more specifically the `MonadUnliftIO` typeclass, found in `Control.Monad.IO.Unlift`. The typeclass is used to power the actual [unliftio](https://www.stackage.org/package/unliftio) library, which is the one you'd use in your applications.

The typeclass itself isn't too big nor too complicated but it took me a while to wrap my head around what's really happening under the hood. To improve my own understanding, I wrote this blog post and I hope that it can make for an interesting or even helpful read.

I'll borrow the examples from the `unliftio` documentation.

## `unliftio` in a Nutshell

You have the following function -- which gives you an `IO ()` -- but you actually need `ReaderT env IO ()`.

```haskell
foo :: String -> IO ()
foo = print
```

The most straight forward and idiomatic way to translate from `IO a` to `m a` (the `m` here being `ReaderT env`) is `liftIO`.

```haskell
bar :: String -> ReaderT env IO ()
bar = liftIO . foo
```

But now you have a function where `IO` occurs in negative position[^1] (see this [post on co- and contravariance](https://www.fpcomplete.com/blog/2016/11/covariance-contravariance)).

```haskell
foo :: (String ->  IO ()) -> IO ()`
                -- ^^^^^ negative position
foo func = func "test"
```

Again, you would like to use a function with `ReaderT env IO ()`, instead of plain `IO ()`. Unfortunately, `liftIO` won't help us here, due to the negative position of the first `IO`. This is where `unliftIO` shines!

```haskell
foo2 :: (String -> ReaderT String IO ()) -> ReaderT String IO ()
foo2 func =
  askUnliftIO >>=
    \u -> liftIO $ foo (unliftIO u . func)
                -- ^^^ note that we're still calling
                -- the original `foo`. We didn't have to
                -- reimplement anything

-- Or alternatively and more concisely
foo2' func = withRunInIO $ \runInIO -> foo (runInIO . func)
```

Both versions of `foo2` work. `unliftIO` temporarily translates back from `m a` to `IO a` so that we can utilize our `ReaderT` but inside a function expecting plain `IO ()`. In that sense the name "unlift" is quite fitting since it's the opposite of lifting.

What made me scratch my head a little was where `runInIO` and `u` came from, and so that's what this post is exploring.

The `MonadUnliftIO` typeclass has two methods: `askUnliftIO` and `withRunInIO`, either of which suffices for a minimal implementation. We'll go over both, so let's start with `askUnliftIO`.

## `askUnliftIO`

`askUnliftIO` is the first typeclass method we'll look at. It has the type signature `m (UnliftIO m)`. The `UnliftIO` newtype, which is shown below, exists because (quoting the docs)

> We need to new datatype (instead of simply using a forall) due to lack of support in GHC for impredicative types.

```haskell
newtype UnliftIO m = UnliftIO
  { unliftIO :: forall a. m a -> IO a
  }
```

You can pretty much ignore the `UnliftIO` newtype for the purposes of understanding how the library works.

To make it easier to follow along, I created [a gist](https://gist.github.com/cideM/aa69df23cf8cb50295ed629f2432d6a6) with a Haskell script that you can simply save somewhere and then load into `ghci`. Note that I'm not importing `unliftio`, instead I simply copy & pasted the minimal, relevant bits from the source code.

_Once post is done, annotate the source code so it's a stand alone tutorial_

We have two instances for `MonadUnliftIO` in this file, one for `ReaderT` and one for `IO`. The `IO` instance isn't very interesting, since it's more or less just the identity function.

Then there's the `ReaderT` instance, which is a bit more involved.

```haskell
instance MonadUnliftIO m => MonadUnliftIO (ReaderT r m) where
  askUnliftIO =
    ReaderT $ \env ->
      askUnliftIO >>= \newtypeU ->
        let unlift = unliftIO newtypeU
         in liftIO $ return (UnliftIO (unlift . flip runReaderT env))
```

The function signature of `askUnliftIO` is `m (UnliftIO m)`. Specialized to our `ReaderT` example here it's `askUnliftIO :: ReaderT r m (UnliftIO (ReaderT r m))`. Since we need to stay in the `ReaderT` monad, the function body starts by creating a new `ReaderT`. More precisely, and also how to docs formulate it, we need to preserve the monadic context, which is `ReaderT r m`.

In the function passed to the `ReaderT` constructor, we use `askUnliftIO` yet again, but this time it's the `askUnliftIO` of the monad inside `ReaderT` (the `m` in `ReaderT r m`). That's why there is a type class constraint for that `m`, mandating that `m` also implements `MonadUnliftIO`. This kind of unwrapping our monad layers by invoking the instance for the next layer is a pretty important concept which can be seen in many libraries.

Since we're using monadic bind `>>=`, the inner most function will get the `UnliftIO m` part from `m (UnliftIO m)`.

Once unwrapped, this gives us the actual function to unlift something from `m a` to `IO a` -- here it's called `unlift`. It may seem a bit magical but it's actually pretty mechanical. Somewhere in our monad layers is an `IO`.

The last line composes two functions together: run our `ReaderT` and pass the result to `unlift`. In our case with `ReaderT env IO` that `unlift` is just the identity function. This means this function takes a monad (`ReaderT`) and outputs an `IO`, which is exactly the signature (`m a -> IO a` ) `UnliftIO` needs. We then package that composed function up in `UnliftIO`, and return a lifted version of it. That last part is necessary because `(UnliftIO ...)` has the signature `UnliftIO (ReaderT r m)`, `return` makes it `IO (UnliftIO (ReaderT r m))` and `liftIO` turns it into `m (UnliftIO (ReaderT r m))`[^2].

One thing that helps tremendously is to just add type annotations everywhere and let GHC check if our assumptions are correct. If you're ever stuck and can't figure out the type signature of something, just replace it with a wildcard[^3] and see what GHC suggests. Here's the above code snippet but with types sprinkled all over it. Note that this snippet _requires the `{-# LANGUAGE InstanceSigs #-}` pragma_!

```haskell
instance MonadUnliftIO m => MonadUnliftIO (ReaderT r m) where
  askUnliftIO :: ReaderT r m (UnliftIO (ReaderT r m))
  askUnliftIO =
    ReaderT $ \env ->
      (askUnliftIO :: m (UnliftIO m)) >>= \(u :: UnliftIO m) ->
        let unlift = (unliftIO u :: m a -> IO a)
            newUnlift =
              (unlift . flip runReaderT env :: ReaderT r m a1 -> IO a1)
            returned =
              return (UnliftIO newUnlift) :: IO (UnliftIO (ReaderT r m))
            returnedLifted = liftIO returned :: m (UnliftIO (ReaderT r m))
         in returnedLifted
```

**TL;DR**: `askUnliftIO` unwraps the layers of monads by recursively invoking the next `askUnliftIO`. Each of those instance methods will also run its monadic context (here `runReaderT`) and pipe the result into the next `unlift`. Eventually we reach `IO`, at which point `askUnliftIO` can just be the identity function and we're done.

## `withRunInIO`

withRunInIO :: ((forall a. m a -> IO a) -> IO b) -> m b

[^1] `IO` also occurs in positive position.

[^2] Technically the `liftIO` isn't necessary for this specific use case. Since we're in the `IO` monad anyway, `liftIO` is superfluous here.

[^3] `foo :: _; foo s = s ++ s` gives `Found type wildcard ‘_’ standing for ‘[a] -> [a]’` which we can then use in the signature.
