A GitChapter Introduction to Monad Transformers 
===========================================

or, Values as Exceptions
------------------------

A big thank you to Christoffer Stjernlöf <https://two-wrongs.com/> who provided his work under (Creative Commons Share Alike license), as well as the other contributers, this article was originally posted at <https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md>.

This project is written with <https://github.com/chrissound/GitChapter>, this means you are able to clone down the project from <https://github.com/chrissound/GentleIntroductionToMonadTransformers>, and hack along from each section where you see the following:

{{ gitCommitOffset }}

The rewrite using GitChapter is partially completed. Hoping to finish this eventually! 


### Either Left or Right

Before we break into the mysterious world of monad transformers, I want to
start with reviewing a much simpler concept, namely the `Either` data type.
If you aren't familiar with the `Either` type, you should probably not jump
straight into monad transformers – they do require you to be somewhat
comfortable with the most common monads.

With that out of the way:

Pretend we have a function that extracts the domain from an email address.
Actually [checking this properly is a rather complex topic][1] which I will
avoid, and instead I will assume an email address can only contain one `@`
symbol, and everything after it is the domain.

I'm going to work with `Text` values rather than `String`s. This
means if you don't have the `text` library, you can either work with
`String`s instead, or `cabal install text`. If you have the Haskell platform,
you have the `text` library.

We need to import `Data.Text` *and* set the `OverloadedStrings` pragma. The
latter lets us write string literals (such as `"Hello, world!"`) and have
them become `Text` values automatically.

```
λ> :module +Data.Text
λ> :set -XOverloadedStrings
```

Now, figuring out how many `@` symbols there are in an email address is
fairly simple. We can see that

{{{ghci eitherLeftOrRight
:set -XOverloadedStrings
:module +Data.Text

splitOn "@" ""

splitOn "@" "test"

splitOn "@" "test@example.com"

splitOn "@" "test@example@com"
}}}

So if the split gives us just two elements back, we know the address
contains just one `@` symbol, and we also as a bonus know that the second
element of the list is the domain we wanted. We can put this in a file.

```haskell
{{fileSection src/Main.hs main}}
```

This draws on our previous discoveries and is pretty self-explainatory. The
function returns `Right domain` if the address is valid, otherwise
`Left InvalidEmail`, a custom error type we use to make handling the errors
easier later on. (Why this is called `LoginError` will be apparent soon.)

This function behaves as we expect it to.

{{{ghci eitherLeftOrRight
:load src/Main.hs
getDomain "test@example.com"
getDomain "invalid.email@example@com"
}}}

To deal with the result of this function immediately, we have a couple of
alternatives. The basic tool to deal with `Either` values is pattern matching, in other words,

```haskell
{{fileSection src/Main.hs printResult'}}
```

Testing in the interpreter shows us that

{{{ghci eitherLeftOrRight
printResult' (getDomain "test@example.com")
printResult' (getDomain "test#example.com")
}}}

Another way of dealing with `Either` values is by using the `either` function.
`either` has the type signature

{{{ghci
:t either
}}}

In other words, it "unpacks" the `Either` value and applies one of the two
functions to get a `c` value back. In this program, we have an
`Either LoginError Text` and we want just a `Text` back, which tells us what
to print. So we can view the signature of `either` as

```haskell
either :: (LoginError -> Text) -> (Text -> Text) -> (Either LoginError Text -> Text)
```


and writing `printResult` with the help of `either` yields a pretty neat
function.

```haskell
{{fileSection src/Main.hs printResult}}
```

This function works the same way as the previous one, except with the
pattern matching hidden inside the call to `either`.


<a name="side-effects"/>


[1]: http://davidcel.is/blog/2012/09/06/stop-validating-email-addresses-with-regex/
