# checked-io

A package providing a new `IOE` type that specifies exactly what exceptions a given `IO` action may throw.

Table of contents:

* [Quickstart](#quickstart)
* [Overview](#overview)
* [Motivation](#motivation)
* [A future in base?](#a-future-in-base)
* [Alternative approaches](#alternative-approaches)
* [Acknowledgements](#acknowledgements)

## Quickstart

1. Add GitHub repo to your snapshot

    ```
    # Stack
    extra-deps:
    - github: brandonchinn178/checked-io
      commit: 358e765363b9281c3c1f981757abc8bd99308074
      subdir:
        - packages/checked-io/

    -- Cabal
    source-repository-package
      type: git
      location: git://github.com/brandonchinn178/checked-io.git
      subdir: packages/checked-io/
    ```

    Not on Hackage right now to avoid taking up a namespace for a project that's still an RFC

2. Add `checked-io` as a dependency

    Recommended: Use `CheckedIO` as the new `Prelude`:

    ```
    # package.yaml
    library:
        dependencies:
          - name: base
            mixin:
              - hiding (Prelude)
          - name: checked-io
            mixin:
              - hiding (CheckedIO)
              - (CheckedIO as Prelude)
        when:
          - condition: false
            other-modules: Paths_my_library

    -- my-library.cabal
    library
      build-depends:
          base
        , checked-io
      mixins:
          base hiding (Prelude)
        , checked-io hiding (CheckedIO)
        , checked-io (CheckedIO as Prelude)
    ```

3. Use new `CheckedIO` interface:

    ```hs
    -- Alternatively, use `uncheckUIO` to force
    -- everything in `main` to handle exceptions
    main :: Main
    main = uncheckIOE $ do
      -- Get an environment variable in IO
      user <- getEnvIO "USER"

      -- Forgetting to handle the exception here would
      -- be a compilation error, since `getEnv` throws
      -- `GetEnvVarError`, but `main` throws
      -- `SomeSyncException`
      try (getEnv "SOME_VAR") >>= \case
        Left e -> print e
        Right var -> putStrLn var

      -- Alternatively, use the UIO version if you're going
      -- to be inspecting the result anyway
      getEnvUIO "SOME_VAR" >>= \case
        Left e -> print e
        Right var -> putStrLn var

      -- Or, to just propagate, use `liftE` to lift the
      -- exception to `SomeSyncException`
      var <- liftE $ getEnv "SOME_VAR"
      putStrLn var
    ```

## Overview

The primary type in this library is `IOE e a`, which is an action that:
1. Interacts with the real world
2. Can throw an exception of type `e`
3. And, if successful, returns a value of type `a`

For convenience, the library also provides two aliases:
* `IO a` is an alias for `IOE SomeSyncException a`; i.e. an `IOE` action that can throw any synchronous exception
* `UIO a` is an alias for `IOE Void a`; i.e. an `IOE` action that _cannot_ throw any synchronous exception

> In this section, we will only be concerned about synchronous exceptions. See the "Motivation" section for more details about why we're ignoring asynchronous and imprecise exceptions.

This library also provides two useful type classes:

* `MonadRunIOE e m`: the equivalent of today's `MonadIO`, running an `IOE e a` action as `m a`
* `MonadRunAsIOE e m`: the equivalent of today's `MonadUnliftIO`, running a `m a` action as `IOE e a`

> The type classes are named this way to avoid the "lifting" and "unlifting" terminology, which is mostly a historical artifact. At the core, all these type classes do is provide a mechanism for embedding an `IOE` action into some monad `m`, or running some monad `m` as an `IOE` action.
>
> The type classes also include the exception as a parameter (instead of forcing `m` to be of kind `* -> * -> *`) because one might want a transformer stack that can only throw `MyException`, which should be able to implement `MonadRunIOE MyException MyMonad`.

`throw` is a useful starting point:

```hs
throw :: e -> IOE e a
```

This signature says that `throw` throws an exception of type `e` as `IOE e a`. Notice that you can't throw a different exception than what `IOE` says it throws. The actual signature looks like this:

```hs
throw :: MonadRunIOE e m => e -> m a
```

This just says that `throw` throws an exception in any monad `m` that can throw exceptions of type `e`.

Looking at `try`, you'll notice that it actually returns an `IOE` that can throw _any_ exception type, not just the one being handled:

```hs
try :: (...) => IOE e1 a -> IOE e2 (Either e1 a)
```

Because `try` catches any exception of type `e1`, and the input `IOE` can _only_ throw exceptions of type `e1`, the resulting `IOE` can be typed as _any_ `e2` (including `UIO`) because we know that `try` won't throw any exceptions.

### Interop with unchecked IO

The long term goal of `checked-io` is to provide a re-implementation of the entire `base` library with properly typed IO. This goal is still in progress, but even if it's accomplished, every other library on Hackage will still be using the unchecked IO, not the new checked IO.

To reference the old unchecked `IO` type, use the new `UnsafeIO` alias. In fact, the `Main` type referenced in the Quickstart is just an alias for `UnsafeIO ()`.

To bring unchecked IO into this new framework, you'll probably want the `checkIOWith` function, if you _know_ that a given function will _only_ throw exceptions of a certain type:

```hs
-- implemented with unchecked IO
makeDatabaseQuery :: String -> UnsafeIO [Value]

-- convert unchecked IO into checked IO
makeDatabaseQueryIOE :: String -> IOE DatabaseException [Value]
makeDatabaseQueryIOE q = checkIOWith check $ makeDatabaseQuery q
  where
    check e =
      case fromException e of
        Just dbExc -> dbExc
        Nothing -> error $ "makeDatabaseQuery threw an unexpected exception: " ++ show e
```

Alternatively, just use `checkIO` to allow handling _any_ exception that might be thrown:

```hs
makeDatabaseQueryIO :: String -> IO DatabaseException [Value]
makeDatabaseQueryIO q = checkIO $ makeDatabaseQuery q
```

### Haddocks

This project isn't on Hackage yet because it's still an in-progress RFC. But the haddocks are available at: https://brandonchinn178.github.io/checked-io.

## Motivation

Haskell exceptions are complicated, made moreso by the fact that they're completely invisible in the type system. In some cases, this is unavoidable (unless you want to handle a `Maybe` every time you divide two numbers), but in other cases, it's not in the type system simply for historical reasons. Today, `IO` represents both "an action that interacts with the real world" and "an action that can throw and catch exceptions", but those two things aren't necessarily tied together.

For example, C code cannot throw exceptions, so any FFI call is an `IO` action that is guaranteed to not throw exceptions. If you're writing bindings to a C library, wouldn't it be _great_ if you could have a public API that says "this is an `IO` action that has side effects _but is guaranteed to not throw any exceptions_"? Or perhaps "this calls a C library and potentially raises a `MyException`, _but no other type of exception_"?

Now, there are three types of exceptions. This library only encodes one of those types in the type system: synchronous exceptions. The other two types, asynchronous and imprecise exceptions, can occur at any time, even in pure code. So it doesn't make sense to represent them specially in an `IO` context. It will still be possible to handle them (e.g. to handle _absolutely every_ exception and log it before terminating), but all normal exception handling will ignore async and imprecise exceptions.

Useful resources:
* https://markkarpov.com/tutorial/exceptions.html
* https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell/
* https://wiki.haskell.org/Error_vs._Exception

## A future in base?

Obviously, trying to keep up with all the changes to `base` would be an immense effort. Plus, it would be nice if `base` typed its functions exactly, to avoid the partial branches that `checked-io` requires, to handle _the possibility_ of a given `IO` action throwing some other exception.

Moving these changes into `base` would also allow libraries to start typing their APIs more strictly, and provide a more type-safe interface. Notice that `checked-io` is flexible about how users want to deal with exceptions; see the "Compatibility with `checked-io`" section for more information.

Here's one possible migration plan for migrating `base` to using checked exceptions:

1. Non-breaking changes
    1. Add `IOE` type
        * Add `IOE` data type and alias `type IO = IOE SomeSyncException`
        * Add `MonadRunIOE` and `MonadRunIO`, alias `MonadIO = MonadRunIO`
    1. Rename existing functions + add alias to original name, e.g.
        * `tryIO = <old try implementation>; try = tryIO`
        * `getEnvIO = <old getEnv implementation>; getEnv = getEnvIO`
    1. Add functions with more precise types, e.g.
        * `tryIOE :: IOE e a -> IOE e' (Either e a)`
        * `getEnvIOE :: String -> IOE GetEnvError String`
        * `getEnvUIO :: String -> UIO (Either GetEnvError String)`
    1. (Optional) Add general functions
        * `MonadRunAsIOE` + `MonadCatchIO`
        * `tryM :: (MonadCatchIO e ioe, MonadRunIOE e' ioe') => ioe a -> ioe' (Either e a)`

1. Breaking change: switch unqualified functions to qualified, e.g.
    * `try = tryIOE`
    * `getEnv = getEnvIOE`

1. Breaking change (optional): deprecate/remove aliases (e.g. `MonadIO`)

Discussion for this can be found at this issue: TODO.

## Alternative approaches

### Summary

| Name of library/approach | `ExceptT`-like transformer <sup>1</sup> | New `IO` monad <sup>2</sup> | Propagates errors | Composes errors <sup>5</sup> | Compatible with `checked-io` |
|---|:-:|:-:|:-:|:-:|:-:|
| `checked-io` |:x: | :white_check_mark: | :white_check_mark: | :x: | :white_check_mark: |
| [`explicit-exception`](https://hackage.haskell.org/package/explicit-exception) | :white_check_mark: | :x: | :white_check_mark: | :x: | :white_check_mark: |
| [`mtl`](https://hackage.haskell.org/package/mtl) | :white_check_mark: | :x: | :white_check_mark: | :x: | :white_check_mark: |
| [`control-monad-exception`](https://hackage.haskell.org/package/control-monad-exception) | :white_check_mark: | :x: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| [`unexceptionalio`](https://hackage.haskell.org/package/unexceptional) | :x: | :white_check_mark: | :x: <sup>3</sup> | :x: | :white_check_mark: |
| [`plucky`](https://hackage.haskell.org/package/plucky) | :x: | :x: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| ["Lightweight Checked Exceptions in Haskell"](https://well-typed.com/blog/2015/07/checked-exceptions/) |:x: | :x: | :white_check_mark: | :white_check_mark: | :warning: <sup>6</sup> |
| [`exceptions-checked`](https://hackage.haskell.org/package/exceptions-checked-0.0.1/candidate) | :x: | :x: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| [`eio`](https://hackage.haskell.org/package/eio) | :x: | :white_check_mark: | :white_check_mark: |  :white_check_mark: <sup>4</sup> | :white_check_mark: |

1. Approaches that propagate exceptions via an `ExceptT`-like transformer has a performance cost, since every use of `>>=` requires checking for exceptions. Throwing exceptions in a normal IO-based system, the runtime system will automatically jump to the last registered handler
2. Any of the approaches that don't provide a new `IO` monad will also allow other hidden exceptions to be thrown in `IO`
3. `unexceptionalio` returns `UIO (Either e a)` everywhere, forcing you to manually propagate exceptions. Due to Haskell's declarative paradigm, if you have a series of operations that can fail, there's no way to exit early without a plugin like [`early`](https://hackage.haskell.org/package/early). But even using `early` runs into the same issues as the `ExceptT` approach.
4. `eio` requires `QualifiedDo` to compose errors.
5. Haskell doesn't currently have a good story for composing exceptions, and `checked-io` does nothing to change that. Rather, `checked-io` simply aims to make errors more visible at the type level, and later efforts can experiment with composing errors. If one is firmly concerned about composing errors, `checked-io` does nothing to change the status quo, and one could keep doing everything in `IO` as before.
6. See the "Lightweight Checked Exceptions" section for more information

### Compatibility with `checked-io`

All of these approaches would still be possible (and even improved) if `checked-io` were in `base`:

* `explicit-exception`, `mtl`, and `control-monad-exception` would all be able to use `UIO` as the base of the stack to force only exceptions specified in the `ExceptT` type to be thrown (Fixing issue #2 above, although still with the performance issue in #1).
* `unexceptionalio` would be redundant, with `UIO` provided by `checked-io`
* "Lightweight Checked Exceptions" would still be technically possible, using `checked-io`'s new `IO` monad, but is conceptually at odds with `checked-io`. See next section for details.
* `control-monad-exception`, `exceptions-checked`, `eio`, and `plucky` are all primarily concerned with error composition, which is still possible to do with `checked-io`'s `IOE` (wrapped in a newtype to get automatic integration with any `MonadRunIOE` function):

    ```hs
    {-- plucky --}

    data EitherE e1 e2 = LeftE e1 | RightE e2
      deriving (Show)
    instance (Exception e1, Exception e2) => Exception (EitherE e1 e2) where
      displayException = \case
        LeftE e1 -> displayException e1
        RightE e2 -> displayException e2
      fromException e =
        (LeftE <$> fromException e) <|> (RightE <$> fromException e)

    newtype IOE' e a = IOE' {unIOE' :: IOE e a}

    instance ProjectError e' e => MonadRunIOE e (IOE' e' a) where
      runIOE = IOE' . mapExceptionM putError

    catchOne ::
      Exception e =>
      IOE' (EitherE e e') a ->
      (e -> IOE' e' a) ->
      IOE' e' a
    catchOne (IOE' m) f = IOE' $ m `catch` go
      where
        go = \case
          LeftE e -> unIOE' (f e)
          RightE e -> throw e

    f :: ProjectError GetEnvError e => IOE' e String
    f = getEnv "USER"

    g :: ProjectError MyException e => String -> IOE' e ()

    -- inferred as:
    --   (ProjectError GetEnvError e, ProjectError MyException e) =>
    --   IOE' e ()
    f >>= g
    ```

    ```hs
    {-- control-monad-exception + exceptions-checked --}

    newtype IOE' e a = IOE' {unIOE' :: IOE SomeSyncException a}

    instance Throws e e' => MonadRunIOE e (IOE' e' a) where
      runIOE = IOE' . liftE

    catchOne ::
      Exception e =>
      IOE' (Caught e e') a ->
      (e -> IOE' e' a) ->
      IOE e' a
    catchOne (IOE' m) f = IOE' $ m `catch` unIOE' . f . go
      where
        go (SomeSyncException e) = fromJust $ cast e

    f :: Throws GetEnvError e => IOE' e String
    f = getEnv "USER"

    g :: Throws MyException e => String -> IOE' e ()

    -- inferred as:
    --   (Throws GetEnvError e, Throws MyException e) =>
    --   IOE' e ()
    f >>= g
    ```

    ```hs
    {-- eio --}

    newtype IOE' es a = IOE' {unIOE' :: IOE SomeSyncException a}

    instance MonadRunIOE e (IOE' '[e] a) where
      runIOE = IOE' . liftE

    catchOne ::
      Exception e =>
      IOE' e1 a ->
      (e -> IOE' e2 a) ->
      IOE (Delete e (e1 <> e2)) a
    catchOne (IOE' m) f = IOE' $ m `catch` unIOE' . f . go
      where
        go (SomeSyncException e) = fromJust $ cast e

    f :: IOE' [GetEnvError] String
    f = getEnv "USER"

    g :: String -> IOE' [MyException] ()

    -- inferred as: IOE' [GetEnvError, MyException] ()
    f EIO.>>= g
    ```

    Note that all of these approaches are still possible with `checked-io`, but the advantage of `checked-io` is that it is backwards compatible with the current state of the Haskell ecosystem, while none of these approaches can provide a backwards-compatible `IO`-analogous type. I think implementing `checked-io` first is the best course of action, and we could take the second step of making one of these error composition approaches first class later.

### The "Lightweight Checked Exceptions" approach

"Lightweight Checked Exceptions" has a fundamentally different approach that's at odds with `checked-io`, where the constraint is _both_ the propagation mechanism and the composition mechanism, as opposed to the other approaches like `plucky`, where the constraint is only the composition mechanism and propagates via the monad.

So if the community prefers this approach over the `checked-io` approach, adding `checked-io` to `base` would be a step in the wrong direction (as opposed to the other approaches, where `checked-io` in `base` is either orthogonal or an intermediate step). That being said, it is technically compatible with `checked-io`, if one wanted to use this approach with `checked-io` already in base:

```hs
{-- option 1, doing the same thing as the blog post
    except with checked-io's IO --}

-- implemented same as blog post
catchOne ::
  Exception e =>
  (Throws e => IO a) ->
  (e -> IO a) ->
  IO a

f :: Throws GetEnvError => IO String
f = getEnvIO "USER"

g :: Throws MyException => String -> IO ()

-- inferred as: (Throws GetEnvError, Throws MyException) => IO ()
f >>= g
```
```hs
{-- option 2, with a newtype wrapper enforcing
    the use of 'Throws' --}

newtype CheckedIO a = CheckedIO (IO a)

unCheck :: CheckedIO a -> IO a
unCheck (CheckedIO m) = m

instance Throws e => MonadRunIOE e CheckedIO where
  runIOE = CheckedIO . liftE

-- implemented same as blog post
catchOne ::
  Exception e =>
  (Throws e => CheckedIO a) ->
  (e -> CheckedIO a) ->
  CheckedIO a

f :: Throws GetEnvError => CheckedIO String
f = getEnv "USER"

g :: Throws MyException => String -> CheckedIO ()

-- inferred as: (Throws GetEnvError, Throws MyException) => CheckedIO ()
f >>= g
```

But analyzing the overall approach, I see the following issues with it:

1. There doesn't seem to be a way to catch all exceptions thrown, you have to manually handle each exception:
    ```hs
    f :: (Throws E1, Throws E2, Throws E3) => IO ()

    f' :: Throws SomeException => IO ()
    f' = handleE @E1 . handleE @E2 . handleE @E3 $ f
      where
        handleE :: forall e. Throws SomeException => (Throws e => IO a) -> IO a
        handleE = handleChecked (throwChecked . SomeException)
    ```

    Or create a type class with type level lists?

    ```hs
    class LiftAll es where
      type ThrowsAll es :: Constraint
      liftAll :: Throws SomeException => (ThrowsAll es => IO a) -> IO a

    instance LiftAll '[] where
      type ThrowsAll '[] = ()
      liftAll = id

    instance LiftAll (e ': es) where
      type ThrowsAll (e ': es) = (Throws e, ThrowsAll es)
      liftAll = liftAll . handleChecked @e (throwChecked . SomeException)

    catchAll ::
      forall es a.
      (ThrowsAll es => IO a) ->
      (SomeException -> IO a) ->
      IO a
    catchAll m f = liftAll @es m `catchChecked` f
    ```
2. It's not tied to any monad, so `Throws Exception1 => Int` is valid, but seems meaningless
    * Unless it should mean imprecise exceptions, but if it does, we should tag every partial function as such. But in that context, one would be forced to propagate a `Throws DivideByZero` constraint everywhere they use division.
3. The blog post indicates it requires FlexibleContexts (?), while `checked-io` doesn't require any extensions
4. `throw :: (Exception e, Throws e) => e -> IO a` fails `-Wredundant-constraints`, which doesn't affect downstream usage, but it would be unfortunate for such a fundamental function to have any warnings at all
5. Having a typeclass with effectively no instances seems brittle
    * Having lawless classes is already controversial, not sure why we'd want to add instance-less classes also
    * Technically, it does have one instance, but its sole purpose is just to be coerced and unwrapped. Who knows if a future version of GHC might optimize out phantom constraints, allowing someone to bypass this?

## Acknowledgements

This work has very much been a standing-on-the-shoulders-of-giants effort. Many thanks to `unliftio` and `safe-exceptions` for initiating conversation on sync vs async exceptions. Thanks to Mark Karpov, Matt Parsons, Michael Snoyman, and many others for their blog posts around better exceptions. And, of course, thanks to GHC developers for providing the foundation of this language upon which we can continue to build.
