# Introduction

This is one (very biased) attempt to make a prelude module with the following
goals:

1. significant reduction of the number of imported modules and names across a
   diverse selection of code,
2. assurance that the names exported from this prelude do not conflict, and
3. support for easy extension.

The first two goals help application programmers focus more on the application
than worrying about where to import something from. The third goal allows
application programmers to push useful, general-purpose functionality into the
package as needed.

# Usage

```
{-# LANGUAGE NoImplicitPrelude #-}

module MyModule where

import Antemodulum
```

Since this package uses `classy-prelude` (and thus `mono-traversable`) to cover
significant shared portions of functionality with type classes, you may find it
useful to use overloaded strings along with a default string type:

```
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module MyModule where

import Antemodulum

default (TextS)
```

Note that `TextS` is a type synonym for `Data.Text.Text`, the strict `Text`
variant from the `text` package.

## Strings

Dealing with strings in Haskell can be difficult. This package does not try to
solve that problem, but it does try to help.

* Use `TextS`, `TextL`, `ByteStringS`, `ByteStringL` in your code to be clear
  about whether your type is strict or lazy. These type synonyms are exported
  from `Antemodulum`.
* Use the `mono-traversable` type classes for many of the functions that are
  common among the string types.
* If you need to use specialized functions, import one of the included modules
  qualified, e.g. `import qualified Antemodulum.Text.Strict as TS`. Note that
  you can still use the above unqualified type synonyms (e.g. `TextS` and not
  `TS.TextS`) after the qualified import.

# Organization

This package uses the "trick" of multiple imports under a shared name to
simplify export declarations and ensure the names from each of the dependencies
do not conflict.

See [`Antemodulum`](./src/Antemodulum.hs) for details on the module
organization.
