---
title: Understanding unliftio
date: '2019-05-30'
publish: false
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

What made me scratch my head a little was where `runInIO` and `u` came from, and so that's what this post is exploring.

The `MonadUnliftIO` typeclass has two methods: `askUnliftIO` and `withRunInIO`, either of which suffices for a minimal implementation. We'll go over both, so let's start with `askUnliftIO`.

## `askUnliftIO`

`askUnliftIO` is the first typeclass method we'll look at. It has the type signature `m (UnliftIO m)`. That newtype wrapper exists because (quoting the docs)

> We need to new datatype (instead of simply using a forall) due to lack of support in GHC for impredicative types.

You can pretty much ignore the `UnliftIO` newtype for the purposes of understand how the library works.

To make it easier to follow along, I created [a gist](https://gist.github.com/cideM/aa69df23cf8cb50295ed629f2432d6a6) with a Haskell script that you can simply save somewhere and then load into `ghci`. Note that I'm not importing `unliftio`, instead I simply copy & pasted the minimal, relevant bits from the source code.

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

The function signature of `askUnliftIO` is `m (UnliftIO m)`. We therefore need to return the same monad that we're defining the instance for. Or more precisely, and also how to docs formulate it, we need to preserve the monadic context. That's why the first thing we do is return a new `ReaderT`.

In the function passed to the `ReaderT` constructor, we use `askUnliftIO` yet again, but this time it's the `askUnliftIO` of the monad inside `ReaderT` (the `m` in `ReaderT r m`). That's why there is a type class constraint for that `m`. This kind of unwrapping our monad transfomer stack by invoking the instance for the next layer is a pretty important concept which can be seen in many libraries.

Since we're using monadic bind `>>=`, the inner most function will get the `UnliftIO m` part from `m (UnliftIO m)` (quick refresher, think of `>>=` as passing the content of the first monad to the function on the right hand side).

Once unwrapped, this gives us the actual function to unlift something from `m a` to `IO a` -- here it's called `unlift`. It may seem a bit magical but it's actually pretty mechanical. Somewhere in our monad transformer stack is an `IO`. Once that is reached, `askUnliftIO` is just the identity function. Until we've gotten to that layer, we just invoke the `askUnliftIO` instance of the next layer, effectivly temporarily unwrapping the transformer stack. Why temporarily? Well in our case right here we ultimately return `ReaderT` again, so that the transformer stack isn't changed. We only really need to get to the inside of the stack (to the `IO`) temporarily to run it in plain `IO`.

The last line composes two functions together: run our `ReaderT` and pass the result to `unlift`. In our case with `ReaderT env IO` that `unlift` is just the identity function. That means this function takes a monad (`ReaderT`) and outputs an `IO`, which is exactly the kind of `m a -> IO a` that `UnliftIO` needs. We then package that composed function up in `UnliftIO`, and return a lifted version of this. That last part is necessary because the `(UnliftIO ...)` part has the signature `UnliftIO (ReaderT r m)`, `return` makes it `IO (UnliftIO (ReaderT r m))` and `liftIO` turns that into `m (UnliftIO (ReaderT r m))`[^2].

[^1] `IO` also occurs in positive position.
[^2] Technically the `liftIO` isn't necessary for this specific use case I've created. Since we're in the `IO` monad anyway, `liftIO` is superfluous here.
_That part is a little fuzzy_
