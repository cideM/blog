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
foo2 :: (String -> ReaderT String IO ())
     -> ReaderT String IO ()
foo2 func =
  askUnliftIO >>=
    \u -> liftIO $ foo (unliftIO u . func)
                -- ^^^ note that we're still calling
                -- the original `foo`. We didn't have to
                -- reimplement anything

-- Or alternatively and more concisely
foo2' func =
  withRunInIO $ \runInIO -> foo (runInIO . func)
```

Both versions of `foo2` work. `unliftIO` temporarily translates back from `m a` to `IO a` so that we can utilize our `ReaderT` but inside a function expecting plain `IO ()`. In that sense the name "unlift" is quite fitting since it's the opposite of lifting.

What made me scratch my head a little was where `runInIO` and `u` came from, and so that's what this post is exploring.

The `MonadUnliftIO` typeclass has two methods: `askUnliftIO` and `withRunInIO`, either of which suffices for a minimal implementation. We'll go over both, so let's start with `askUnliftIO`.

## `askUnliftIO :: m (UnliftIO m)`

This is the first of the two typeclass methods we'll look at. It returns a newtype wrapper (shown below), which, according to the documentation, exists because

> We need to new datatype (instead of simply using a forall) due to lack of support in GHC for impredicative types.

```haskell
newtype UnliftIO m = UnliftIO
  { unliftIO :: forall a. m a -> IO a
  }
```

You can pretty much ignore the newtype for the purposes of understanding how the library works, since it's purely an implementation detail.

To make it easier to follow along, I created [a gist](https://gist.github.com/cideM/aa69df23cf8cb50295ed629f2432d6a6) with a Haskell script that you can simply save somewhere and then load into `ghci`. Note that I'm not importing `unliftio`, instead I simply copy & pasted the minimal, relevant bits from the source code.

We have two instances for `MonadUnliftIO` in this file, one for `ReaderT` and one for `IO`. The latter isn't very interesting, since it's more or less just the identity function. We'll therefore focus on the former:

```haskell
instance MonadUnliftIO m =>
  MonadUnliftIO (ReaderT r m) where
  askUnliftIO =
    ReaderT $ \env ->
      askUnliftIO >>= \newtypeU ->
        let unlift = unliftIO newtypeU
         in liftIO $ return
           (UnliftIO (unlift . flip runReaderT env))
```

The function signature of `askUnliftIO` specialized to `ReaderT` is `ReaderT r m (UnliftIO (ReaderT r m))`. Since we need to stay in the `ReaderT` monad, the function body starts by creating a new `ReaderT`. More precisely, and also how to docs formulate it, we're preserving the monadic context.

In the function passed to the `ReaderT` constructor, we use `askUnliftIO` yet again, but this time it's the instance of the monad inside our reader (the `m` in `ReaderT r m`). That's why there is a type class constraint, mandating that `m` also implements `MonadUnliftIO`. This kind of unwrapping our monad layers by invoking the instance for the next layer is a pretty important concept which can be seen in many libraries.

Since we're using monadic bind `>>=`, the inner most function will get the `UnliftIO m` part from `m (UnliftIO m)`.

Once unwrapped, this gives us the actual function to unlift something from `m a` to `IO a` -- here it's called `unlift`. It may seem a bit magical but it's actually pretty mechanical. Somewhere in our monad layers is an `IO`.

The last line composes two functions together: run our `ReaderT` and pass the result to `unlift`. In our case with `ReaderT env IO` that `unlift` is just the identity function. This means this function takes a monad (`ReaderT`) and outputs an `IO`, which is exactly the signature (`m a -> IO a` ) `UnliftIO` needs. We then package that composed function up in `UnliftIO`, and return a lifted version of it. That last part is necessary because `(UnliftIO ...)` has the signature `UnliftIO (ReaderT r m)`, `return` makes it `IO (UnliftIO (ReaderT r m))` and `liftIO` turns it into `m (UnliftIO (ReaderT r m))`[^2].

One thing that helps tremendously is to just add type annotations everywhere and let GHC check if our assumptions are correct. If you're ever stuck and can't figure out the type signature of something, just replace it with a wildcard[^3] and see what GHC suggests. Here's the above code snippet but with types sprinkled all over it. Note that this snippet _requires the `{-# LANGUAGE InstanceSigs #-}` pragma_!

```haskell
instance MonadUnliftIO m =>
  MonadUnliftIO (ReaderT r m) where
  askUnliftIO :: ReaderT r m (UnliftIO (ReaderT r m))
  askUnliftIO =
    ReaderT $ \env ->
    -- askUnliftIO :: m (UnliftIO m)
      (askUnliftIO >>= \(u :: UnliftIO m) ->
        let unlift = (unliftIO u :: m a -> IO a)
            newUnlift =
                     -- flip runReaderT env
                     -- :: ReaderT r m a1
                     -- -> m a1
              (unlift . flip runReaderT env)
         -- returned :: IO (UnliftIO (ReaderT r m))
            returned =
              return (UnliftIO newUnlift)
         -- returnedLifted :: m (UnliftIO (ReaderT r m))
            returnedLifted =
              liftIO returned
         in returnedLifted
```

**TL;DR**: `askUnliftIO` unwraps the layers of monads by recursively invoking the next `askUnliftIO`. Each of those instance methods will also run its monadic context (here `runReaderT`) and pipe the result into the next `unlift`. Eventually we reach `IO`, at which point `askUnliftIO` can just be the identity function and we're done.

## `withRunInIO`

The `withRunInIO` function removes a bit of the plumbing that `askUnliftIO` requires and let's you write code that is even more concise than `askUnliftIO` (as can be seen from the example at the end of the first chapter).

Its function signature is `((forall a. m a -> IO a) -> IO b) -> m b`, but for simplicity's sake I won't include the `forall` part in future references to the signature. As you can see, it takes only a single argument, which is a callback.

Just as with `askUnliftIO`, the `IO` instance is pretty boring, so we'll jump straight to our familiar `ReaderT`.

The basic layout is quite similar as for `askUnliftIO`: We're returning a `ReaderT`, and inside the function passed to the constructor we're calling `withRunInIO`. This uses the `withRunInIO` instance from whatever monad we're using in the reader. Hence the type class constraint for the monad used in the reader. But there is one important difference: `withRunInIO` takes an argument, which is called `inner` in the code snippets.

```haskell
withRunInIO inner =
  ReaderT $ \env ->
    withRunInIO $ \runInIO ->
      inner (runInIO . flip runReaderT env)

foo2' func = withRunInIO $
  \runInIO -> foo (runInIO . func)
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ inner
```

I copied the snippet from the first chapter, so it's easier to see how `foo2'` is passing a callback to `withRunInIO`, which is what we refer to as `inner`. That function also receives a callback (here called `runInIO`), which does exactly the same as the `unlift` function we pulled out of the `UnliftIO` newtype in the last chapter.

The basic mechanism is exactly the same: peel off the monad layers, delegating to the `UnliftIO` instance of each layer, until we eventually reach `IO`. `withRunInIO` is just a bit more concise.

Here's the reader instance with type annotations:

```haskell
instance MonadUnliftIO m =>
  MonadUnliftIO (ReaderT r m) where
  withRunInIO
    :: ((forall a. ReaderT r m a -> IO a) -> IO b)
    -> ReaderT r m b
  withRunInIO inner =
    ReaderT $ \env ->
      -- withRunInIO
      -- :: ((forall a. m a -> IO a) -> IO b)
      -- -> m b
      withRunInIO $ \runInIO ->
          -- runInIO          |   flip runReaderT env
          -- :: forall a. m a |   :: ReaderT r m a
          -- -> IO a          |   -> m a
          inner (runInIO      .   flip runReaderT env)
```

## What Now?

The goal of this post was to understand how `unliftio-core` works on a basic level. Consider it an intellectual exercise for non-experts (like me) to understand how other people write their Haskell libraries. For information on how this library treats async exceptions or handling the state monad in relation to cleanup handlers, please see the official documentation.

The `unliftio` library re-exports a lot of modules, none of which I looked at in this post. That's because once you understand the concept, the implementation of most (all?) of these re-exported modules is fairly mechanical. See for example this random snippet from `UnliftIO.Process`

```haskell
withCreateProcess ::
     MonadUnliftIO m
  => CreateProcess
  -> (Maybe Handle
      -> Maybe Handle
      -> Maybe Handle
      -> ProcessHandle
      -> m a)
  -> m a
withCreateProcess c action =
  withRunInIO
    (\u ->
       P.withCreateProcess
         c
         (\stdin_h stdout_h stderr_h proc_h ->
            u (action stdin_h stdout_h stderr_h proc_h)))
```

Hopefully it's easy to see the familiar pattern of passing a callback to `withRunInIO`, where you then have access to an unlift function, thanks to the type class constraint.

[^1] `IO` also occurs in positive position.

[^2] Technically the `liftIO` isn't necessary for this specific use case. Since we're in the `IO` monad anyway, `liftIO` is superfluous here.

[^3] `foo :: _; foo s = s ++ s` gives `Found type wildcard ‘_’ standing for ‘[a] -> [a]’` which we can then use in the signature.
