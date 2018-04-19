### We Can Make Our Own Monads

We keep coming across the `IO (Either e a)` type, so maybe there is something
special about that. What happens if we make a Haskell data type out of that
combination?

```haskell
{{fileSection src/Main.hs EitherIO}}
```

What did we get just by doing this? Let's see:

{{{ghci
:l src/Main.hs
:type EitherIO
:type runEitherIO
}}}

So already we have a way to go between our own type and the combination
we used previously! That's *gotta* be useful somehow.

