### Introducing Side-Effects

{{ gitCommitOffset }}

Now we'll use the domain as some sort of "user token" – a value the user
uses to prove they have authenticated. This means we need to ask the user
for their email address and return the associated token.

```haskell
{{fileSection src/Main.hs getToken}}
```

So when `getToken` runs, it'll get an email address from the user and
return the domain of the email address.

```
λ> getToken
Enter email address:
test@example.com
Right "example.com"
```

and, importantly,

```
λ> getToken
Enter email address:
not.an.email.address
Left InvalidEmail
```

Now, let's complete this with an authentication system. We'll have two users
who both have terrible passwords:

```haskell
{{fileSection src/Main.hs users}}
```

With an authentication system, we can also run into two new kinds of errors,
so let's change our `LoginError` data type to reflect that.

```haskell
{{fileSection src/Main.hs LoginError}}
```

We also need to write the actual authentication function. Here we go...

```haskell
{{fileSection src/Main.hs userLogin}}
```

checks that the email was processed without problems, finds the
user in the collection of users, and if the passwords match, it returns
the token to show the user is of authenticated.

If anything goes wrong, such as the passwords not matching, there not
being a user with the entered domain, or the `getToken` function failing to
process, then a `Left` value will be returned.

This function is *not* something we want to deal with. It's big, it's bulky,
it has several layers of nesting... it's not the Haskell we know and love.

Sure, it's possible to rewrite it using function calls to `either` and
`maybe`, but that wouldn't help very much. The real reason the code is this
ugly is that we're trying to mix both `Either` and `IO`, and they don't seem
to blend well.

The core of the problem is that the `IO` monad is designed for dealing with
`IO` actions, and it's terrible at handling errors. On the other hand, the
`Either` monad is great at handling errors, but it can't do `IO`. So let's
explore what happens if you imagine a monad that is designed to *both*
handle errors *and* `IO` actions.

Too good to be true? Read on and find out.

