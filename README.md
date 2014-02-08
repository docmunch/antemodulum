# Introduction

This is one (very biased) attempt to make a prelude module with the following
goals:

1. significant reduction in the number of immediate package dependencies and
   imported modules and names for general-purpose application code,
2. assurance that the names exported from this prelude do not conflict, and
3. easy extension.

The first two goals help application programmers focus more on the application
than worrying about where to import something from. The third goal allows
application programmers to push useful, general-purpose functionality into the
package as needed.

There is naturally a balance to be found with this package between the two
conflicting goals:

1. general-purpose re-exports that are useful in a variety of application
   domains and
2. overloading the function namespace with useless operations.

# Usage

```
{-# LANGUAGE NoImplicitPrelude #-}

module MyModule where

import Antemodulum
```

## Package Dependencies

To reduce the need to update an application's `.cabal` file with a core set of
package dependencies, you can depend on `antemodulum` for all of its
dependendencies and avoid adding those same dependencies to your own application
by not importing the same modules directly.

For example, suppose you want to use `Data.Text` functionality (see the next
section). If you import `Data.Text` directly, you will need to add `text` to
your `.cabal`. Instead, import `Antemodulum.Text.Strict` which simply re-exports
`Data.Text`.

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

# Organization

This package uses the "trick" of multiple imports under a shared name to
simplify export declarations and ensure the names from each of the dependencies
do not conflict.

See [`Antemodulum`](./src/Antemodulum.hs) for details on the module
organization.

## Extension

The organization of `antemodulum` into themed modules makes it easy to extend
with related functionality. Here are a few ways it can be extended:

1. *Adding another package dependency.* If the package is related, say to the
   `System` top-level module, it can be added and re-exported at the appropriate
   place, e.g. `Antemodulum.System`.
2. *Adding a function.* If the function is general-purpose but related, say to
   numbers, it can added to the appropriate module, e.g. `Antemodulum.Numeric`.
