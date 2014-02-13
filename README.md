# Introduction

A prelude module with the following goals:

1. significant reduction in the number of immediate package dependencies and
   imported modules and names for general-purpose application code,
2. assurance that the names exported from this prelude do not conflict, and
3. easy extension.

The first two goals help application programmers focus more on the application
than worrying about where to import something from. The third goal allows
application programmers to push useful, general-purpose functionality into the
package as needed.

There is a balance to be found between:

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

* Use `Text`, `TextL`, `ByteString`, `ByteStringL` in your code to be clear
  about whether your type is strict (no suffix) or lazy (an `L` suffix) and to
  avoid having to use qualified names . These types are exported from
  `Antemodulum`.
* Use the `mono-traversable` type classes for many of the functions that are
  common among the string types.
* If you need to use specialized functions, import one of the included modules
  qualified, e.g. `import qualified Antemodulum.Text.Strict as TS`. Note that
  you can still use the above unqualified types (e.g. `Text` instead of
  `TS.Text`) after the qualified import. In case you later remove the qualified
  import, you won't have to revert the type names.

Since this package uses `classy-prelude` (and thus `mono-traversable`) to cover
significant shared portions of functionality with type classes, you may find it
useful to use overloaded strings along with a default string type:

```
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module MyModule where

import Antemodulum

default (Text)
```

# Organization

## Module Hierarchy

The modules in this package are hierarchical only for categorizing the domain of
the included exports and code. A submodule is not exported from its parent
module. One benefit of this is that the modules can themselves be used
independently in the other modules of this package. A common example of this is
importing one or more of the `Text` modules into other modules that define
`Text`-related functionality.

## Types of Modules

In this package, there are currently three types of modules that differ on how
they should be imported into user code.

### No-Conflict Modules

No-conflict modules are exported directly from `Antemodulum`. This means that
all names exported from these modules are immediately available to the user by
importing `Antemodulum` and that -- if the package builds successfully -- there
will be no conflicting names among these modules. The vast majority of modules
in this package are no-conflict modules.

See [`Antemodulum`](./src/Antemodulum.hs) for the no-conflict modules, which are
exported without any export list.

### Partial-Conflict Modules

There are some modules that are not exported from `Antemodulum` but may conflict
with each other. A user can probably safely import one of these with
qualification, though importing more than one will definitely cause problems.

These are the partial-conflict modules:

* `Antemodulum.Monad.Lazy`
* `Antemodulum.Monad.Strict`

### Qualified Modules

The last type of module is one that will probably cause problems in the imported
namespace. For that reason, a user should either import them qualified or with
an import list.

To reduce the number of these modules, we use packages like `classy-prelude`
which include a useful type classes that cover much of the functionality of
these modules. We also add our type classes when it is useful. See
[`Antemodulum.Text`](./src/Antemodulum/Text.hs) for an example. If there are
more functions that can be supported in a similar manner, please report them.

To avoid having to qualify the types, we do export them or type synonyms from
`Antemodulum`. Ideally, a user only needs to qualify the functions.

The qualified modules (with their unqualified types/type synonyms and/or
constructors) are:

* `Antemodulum.ByteString.Strict` (`ByteString`)
* `Antemodulum.ByteString.Lazy` (`ByteStringL`)
* `Antemodulum.ByteString.Char8` (`ByteStringC`)
* `Antemodulum.Text.Strict` (`Text`)
* `Antemodulum.Text.Lazy` (`TextL`)
* `Antemodulum.List.NonEmpty` (`NonEmpty((:|))`)
* `Antemodulum.Strict` (`EitherS`, `MaybeS`)

## Extension

The organization of `antemodulum` into themed modules makes it easy to extend
with related functionality. Here are a few ways it can be extended:

1. *Adding another package dependency.* If the package is related, say to the
   `System` top-level module, it can be added and re-exported at the appropriate
   place, e.g. `Antemodulum.System`.
2. *Adding a function.* If the function is general-purpose but related, say to
   numbers, it can added to the appropriate module, e.g. `Antemodulum.Numeric`.
