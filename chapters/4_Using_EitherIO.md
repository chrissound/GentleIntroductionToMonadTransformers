### Using `EitherIO`

Now that our `EitherIO` type is a real monad, we'll try to put it to work! If we change the type signature of our `getToken` function, we'll run into problems quickly, though.

{{{ghci abcxyz
:load src/Main.hs
:set -XOverloadedStrings
:{
getToken :: EitherIO LoginError Text
getToken = do
  T.putStrLn "Enter email address: "
  input <- T.getLine
  return (getDomain input)
:}
}}}

We get three type errors from this function alone, now!

Converting `Either e a` to `EitherIO e a` isn't terribly difficult. We have two functions to help us with that.

```haskell
return   :: Either e a -> IO (Either e a)
EitherIO :: IO (Either e a) -> EitherIO e a
```

With both of those, we can take the `getDomain` function call and fit it in the new `getToken` function, like so:

```haskell
EitherIO (return (getDomain input))
```

Remember this line, because we're going to put it into the function soon enough. But first, let's find out how to convert the two `IO a` values to an `EitherIO e a`. Again, we will have use of the `EitherIO` function. For the rest, it's useful to know your functors. If you do, you'll realise that

```haskell
fmap Right :: IO a -> IO (Either e a)
```

so our `IO` actions would both be

```haskell
EitherIO (fmap Right (T.putStrLn "email"))
EitherIO (fmap Right (T.getLine))
```

With these three lines, the `getToken` function is now written as

```haskell
getToken :: EitherIO LoginError Text
getToken = do
  EitherIO (fmap Right (T.putStrLn "Enter email address:"))
  input <- EitherIO (fmap Right T.getLine)
  EitherIO (return (getDomain input))
```

But this looks even *more* horrible than it was before! Relax. We'll take a detour to clean this up slightly.
