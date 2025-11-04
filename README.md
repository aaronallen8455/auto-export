# auto-export :outbox_tray:

This is a GHC plugin that edits the file being compiled by adding a target
top-level definition to the explicit export list of the module.

### Usage

This plugin is intended to be used with GHCi or adjacent utilities such as
`ghcid` and `ghciwatch` as a developement tool, not as a package dependency.
Here is an example command for starting a REPL for a stack project with the
`auto-export` plugin enabled (you may need to add `auto-export` to your
`extra-deps` first):

```
stack repl my-project --package auto-export --ghci-options='-fplugin AutoExport'
```

likewise for a cabal project (you may need to run `cabal update` first):

```
cabal repl my-project --build-depends auto-export --repl-options='-fplugin AutoExport'
```

With the plugin enabled, mark the expression to be exported by placing
`!EXPORT` on a line above it then compile the module. If the module has an
explicit export list, the definition will be automatically added to it.
Compilation is aborted after the code is updated.

For example, given this program:

```haskell
module M () where

data Colors = Red | Green | Blue
  deriving (Enum, Bounded)

allColors :: [Colors]
allColors = [minBound .. maxBound]
```

Insert the export targets:

```haskell
module M () where

!EXPORT
data Colors = Red | Green | Blue
  deriving (Enum, Bounded)

!EXPORT
allColors :: [Colors]
allColors = [minBound .. maxBound]
```

Compiling the module will result in the source code being updated to:

```haskell
module M (Colors(..), allColors) where

data Colors = Red | Green | Blue
  deriving (Enum, Bounded)

allColors :: [Colors]
allColors = [minBound .. maxBound]
```

Data declarations and type classes are always exported in full using `(..)`.

The plugin only supports certain GHC versions with the intent of supporting the
four latest release series, i.e. `9.6.*` thru `9.12.*`. The cabal file
indicates the currently supported versions.
