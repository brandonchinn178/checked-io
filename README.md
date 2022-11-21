# checked-io

A package providing `IOE` and `UIO` types that capture `IO` actions that do and do not throw exceptions, respectively.

## Motivation

Haskell exceptions are complicated, made moreso by the fact that they're completely invisible in the type system. In some cases, this is unavoidable (unless you want to handle a `Maybe` every time you divide two numbers), but not every case. Today, `IO` represents both "an action that interacts with the real world" and "an action that can throw and catch exceptions", but those two things aren't necessarily tied together.

For example, C code cannot throw exceptions, so any FFI call is an `IO` action that is guaranteed to not throw exceptions. If you're writing bindings to a C library, wouldn't it be _great_ if you could have a public API that says "this is an `IO` action that has side effects _but is guaranteed to not throw any exceptions_"? Or perhaps "this calls a C library and potentially raises a `MyException`, _but no other type of exception_"?

Now, there are three types of exceptions. This library only encodes one of those types in the type system: synchronous exceptions. The other two types, asynchronous and imprecise exceptions, can occur at any time, even in pure code. So it doesn't make sense to represent them specially in an `IO` context. It will still be possible to handle them (e.g. to handle _absolutely every_ exception and log it before terminating), but all normal exception handling will ignore async and imprecise exceptions.

Useful resources:
* https://markkarpov.com/tutorial/exceptions.html
* https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell/
* https://wiki.haskell.org/Error_vs._Exception
