### Implementing Instances for Common Typeclasses

This section might be a little difficult if you're new to the language and haven't had a lot of exposure to the internals of how common typeclasses work. You don't *need* to understand this section to continue reading the article, but I strongly suggest you put on your to-do list to learn enough to understand this section. It touches on many of the core components of what makes Haskell Haskell and not just another functional language.

If you want to read more about this kind of thing, [The Typeclassopedia][2] by Brent Yorgey is a comprehensive reference of the most common typeclasses used in Haskell code. Learn You a Haskell has a popular [introduction to the three typeclasses we use here][3]. Additionally, Adit provides us with [a humourous picture guide to the same three typeclasses][4].

But before we do anything else, let's make `EitherIO` a functor, an applicative and a monad, starting with the functor, of course.

```haskell
{{fileSection src/Main.hs EitherIOFunctor}}
```

This may look a little silly initially, but it does make sense. First, we
"unwrap" the `EitherIO` type to expose the raw `IO (Either e a)` value. Then
we `fmap` over the inner `a`, by combining two `fmap`s. Then we wrap the
new value up in `EitherIO` again, and return the wrapped value. If you are
a more experienced Haskell user, you might prefer the following, equivalent,
definition instead.

```haskell
instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO
```

In a sense, that definition makes it more clear that you are just unwrapping,
running a function on the inner value, and then wrapping it together again.

The two other instances are more of the same, really. Creating them is a
mostly mechanical process of following the types and unwrapping and wrapping
our custom type. I challenge the reader to come up with these instances on
their own before looking below how I did it, because trying to figure these
things out *will* improve your Haskell abilities in the long run.

In any case, explaining them gets boring, so I'll just show you the instances
as an experienced Haskell user might write them.

```haskell
{{fileSection src/Main.hs EitherIOApMon}}
```

If your definitions look nothing like these, don't worry. As long as your
definitions give the correct results, they are just as good as mine. There
are many ways to write these definitions, and none is better than the other
as long as all are correct.
