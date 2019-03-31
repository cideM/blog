---
title: Applicative Instance for Compose
date: "2019-03-27"
publish: true
---

## The Problem

When I went through _Haskell From First Principle (HFFP)_ the first time, I struggled with the `Compose` applicative instance, which is part of an exercise in chapter 25. I recently revisited some chapters from the book and I finally understand how it works. In this post I'll go through some hints and visual cues that have helped me sort it all out.

Here's the code that I couldn't make sense of initially:

```haskell
instance (Applicative fa, Applicative fb) =>
  Applicative (Compose fa fb) where
  (<*>) ::
       Compose fa fb (a -> b)
    -> Compose fa fb a
    -> Compose fa fb b
  Compose x <*> Compose y =
    Compose ((<*>) <$> x <*> y)
```
_Functor types are named fa, fb, and so on. If you see an f, it's a function, both on the type and the data level. I refer to fa and fb as functors since the applicative type class requires those things to be functors._

As is often the case with Haskell, the code is really concise. In a single line we're dealing with three operators and one of them is the very operator we're defining for `Compose` (`<*>`). While that kind of code is a joy to read and write if you're fluent in all things functor and applicative, it's a mouthful if you're trying to improve your understanding of these topics.

Creating instances for "simple" functors (without additional nesting) is arguably a lot easier since in `fmap f fa` the `f` is a plain function. It's not hidden inside a data structure. That lets you focus entirely on the functor `fa`. Ultimately you just apply `lift` often enough so that `f` is applied to the value inside the functor `fa`.

With the applicative instance here it's fundamentally the same problem, just that the function is also wrapped inside something else. So let's just tackle this problem one layer at a time, starting with things we're familiar with.

## Something Concrete

The code below shows a function `(+) 2` wrapped in a `Maybe`. We apply it to a value (wrapped in a `Maybe`) by using `<*>`. Simple enough.

```haskell
> let a = Just ((+) 2)
> let b = Just 5
> a <*> b

       Just ((+) 2)     Just 5    Just 7
<*> :: fa   (a -> b) -> fa   a -> fa   b
```

Let's up the ante a bit and wrap both the function and the value in another `Maybe`.

```haskell
> let a = Just (Just ((+) 2))
> let b = Just (Just 5)
> a <*> b
```

This does not work since we can't just add another layer and expect the original `<*>` to work. After all, its type signature expects function and value to be inside a single functor, not nested in another.

So what's the #1 solution for manipulating nested stuff in Haskell? `lift` all the things!

```haskell
> let a = Just (Just ((+) 2))
> let b = Just (Just 5)
> liftA2 (<*>) a b
```

We lift `<*>` over both values. I like to think of it like doing exactly the same thing as before, just one level deeper.

## Something Abstract

How does `liftA2` help us make sense of the instance code though?

```haskell
instance (Applicative fa, Applicative fb) =>
  Applicative (Compose fa fb) where
  (<*>) ::
       Compose fa fb (a -> b)
    -> Compose fa fb a
    -> Compose fa fb b
  Compose x <*> Compose y =
    Compose ((<*>) <$> x <*> y)
```

The first part `(<*>) <$> x` written without infix notation and `fmap` instead of the operator is `fmap (<*>) x`. What does that mean in terms of the type signature? We map the function `<*>` over `x`, which has the type `fa fb (a -> b)`. We therefore apply `<*>` to the `fb (a -> b)` part. Check out the commented code below, which hopefully makes things clearer.

```haskell
instance (Applicative fa, Applicative fb) =>
  Applicative (Compose fa fb) where
    (<*>) ::
          Compose fa fb (a -> b)
                 --  ^^^^^^^^^^^
                 -- This is the first argument to <*>
       -> Compose fa fb a
       -> Compose fa fb b
    Compose x <*> Compose y =
        -- fa' :: fa (fb a -> fb b)
        --            ^^^^ The 2nd argument to <*>
        let fa' = fmap (<*>) x
        in ???
```

The `<*>` only needs its 2nd argument now, which is a functor with a value inside it. And we have something like that **inside** our `y` (`y` is `fa fb b` and therefore the missing argument to `<*>` is the `fb b` part inside the `fa`). How can we apply a function inside a functor to a value inside a functor? `<*>`! And that's how we arrive at the 2nd part:

```haskell
instance (Applicative fa, Applicative fb) =>
  Applicative (Compose fa fb) where
    (<*>) ::
          Compose fa fb (a -> b)
       -> Compose fa fb a
       -> Compose fa fb b
    Compose x <*> Compose y =
        let fa' = fmap (<*>) x
        in fa' <*> y
```

Quick recap:

- `fmap (<*>) x`: Partially apply `<*>`
- `fa' <*> y`: Fully apply the `<*>` inside `fa'`

## Back to `liftA2`

The implementation of the applicative instance using `<*>` and a mix of infix operators requires some mental gymnastics. On [hackage](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Functor.Compose.html#line-112) however the instance uses `liftA2`, which does a much better job of communicating the essence of what's going on. We're lifting `<*>` through the additional level of nesting.

```haskell
   fb (a -> b) ->    fb a ->    fb b
fa fb (a -> b) -> fa fb a -> fa fb b
```

If you align the type signatures of `<*>` (top) and `Compose x <*> Compose y` (bottom), you can see the similarities. It's the exact same operation one level deeper for both arguments, hence the use of `liftA2`.
