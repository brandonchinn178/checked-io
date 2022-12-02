# checked-io

A package providing a new `IOE` type that specifies exactly what exceptions a given `IO` action may throw.

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

Moving these changes into `base` would also allow libraries to start typing their APIs more strictly, and provide a more type-safe interface. Notice that `checked-io` is flexible about how users want to deal with exceptions; library authors are still able to use whatever exception mechanism they want in `IOE`, e.g. Matt Parsons's suggestion of using [type classes to compose error types](https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html).

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

## Prior work

Other libraries that also attempt to improve exceptions in Haskell:

* `explicit-exception` + `mtl` both provide an `ExceptT` transformer, where `>>=` will short-circuit on `Left`. The problem here is that if the base monad is `IO`, you still have the possibility of some other exception type being thrown in `IO`. Plus, constantly checking for `Left` in each `>>=` has a performance cost.

* `unexceptionalio` provides an equivalent of `UIO`, but using `UIO` everywhere forces you to explicitly handle `Either` everywhere. If you have a series of operations that can fail, there's no way to exit early, due to Haskell's declarative paradigm (without a plugin like [`early`](https://hackage.haskell.org/package/early)).

`checked-io` improves upon these libraries by still using the same runtime exception system under the hood, which allows for early exit and better performance (due to the fact that when a runtime exception occurs, the RTS automatically jumps to the last callstack that registered a catch handler).

## Acknowledgements

This work has very much been a standing-on-the-shoulders-of-giants effort. Many thanks to `unliftio` and `safe-exceptions` for initiating conversation on sync vs async exceptions. Thanks to Mark Karpov, Matt Parsons, Michael Snoyman, and many others for their blog posts around better exceptions. And, of course, thanks to GHC developers for providing the foundation of this language upon which we can continue to build.
