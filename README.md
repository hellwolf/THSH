> [!NOTE]
>
> This project is still work in progress. It has a working proof-of-concept (PoC), that was demonstrated at the
> [MuniHac2024](https://munihac.de/2024.html) event.

THSH
====

**THSH** (**T**emplate **H**askell **SH**ell) is a "noDSL" approach to mixing shell scripting with Haskell programs
using Template Haskell.

# Key Features

## Gradual "Haskellization"

The key goal of the project is to enable a gradual approach to adding strongly-typed Haskell code to enhance your shell
scripting experience.

It is gradual because you can wrap any shell script into a Template Haskell quasi-quotation with `thsh`:

```sh
#!/usr/bin/env thsh

__main__ = [|thsh\
echo "Hello Haskell!"
|]
```

## Pipe To Haskell Functions

> [!WARNING]
>
> The current PoC uses the PyF project's internal code to achieve parsing, which presents difficulty in choosing a
> better Haskell code delimiters other than "« ... »", for now. In the next version, a more palatable syntax should be
> achieved, e.g. "!{ ... }".

You can pipe the entire outputs from a process to a Haskell function:

```sh
#!/usr/bin/env thsh

import Data.Char (toLower)

__main__ = [thsh|\
curl -s https://www.haskell.org/ | «fn (ContentFn $
  \content -> "Number of occurrences of the word 'haskell' on haskell.org is "
    <> show (length (filter ((== "haskell"). fmap toLower) . words $ content))
    <> "\n"
  )
»
|]
```

===>

```
Number of occurrences of the word 'haskell' on haskell.org is 30
```

You can also process each line independently:

```sh
#!/usr/bin/env thsh

sumSales = LineReadFn
           (\ (_ :: String, price, quantity) s -> let s' = s + price * quantity
                                                  in (s', Nothing))
           (\ s -> Just (show s ++ "\n"))
           (0.0 :: Float)

__main__ = [thsh|\
echo -n "Sum of the sales: "; {
# Read (product :: String, price :: Float, quanity :: Float)
«fn sumSales» <<EOF
("apple",  1.2, 100.2)
("orange", 2.0, 34.2)
("pear", 1.2, 62.3)
EOF
} | tail -n1
|]
```

===>

```
Sum of the sales: 263.4
```

> [!NOTE]
>
> More pre-defined function patter will be developed.

## Composable

The `thsh` quoted code can compose with each other:

```haskell
#!/usr/bin/env thsh

s0 :: Script
s0 = [thsh| bc |]

__main__ = [thsh|\
for i in `seq 0 10`;do
  expr="2 ^ $i"
  echo -n "$expr = "
  echo $expr | «sh s0»
done
|]
|]
```

-->

```
= 2
2 ^ 2 = 4
2 ^ 3 = 8
2 ^ 4 = 16
2 ^ 5 = 32
2 ^ 6 = 64
2 ^ 7 = 128
2 ^ 8 = 256
2 ^ 9 = 512
2 ^ 10 = 1024
```

## Cabal Single-Script Metadata

By default, the "thsh" loader uses the ["cabal
run"](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-run) under the hood. That means that you can use
cabal specific metadata sections for both package-level and project-level:

```
{- cabal:
build-depends: diagrams
-}
{- project:
optimization: 2
-}
```

## More Examples

You can find more examples available under the "examples" folder.

# Comparing To eDSL Solutions

We should note that there have been multiple projects allowing the mixing of Haskell code with shell scripting. All of
them require their users to learn an eDSL of their own.

Here is an incomplete list of these projects:

1. [hell](https://github.com/chrisdone/hell) : Haskell-based shell scripting language.
2. [turtle](https://hackage.haskell.org/package/turtle) : turtle is a reimplementation of the Unix command line
   environment in Haskell so that you can use Haskell as both a shell and a scripting language.
3. [shelly](https://hackage.haskell.org/package/shelly) : shell-like (systems) programming in Haskell.
4. [shh](https://hackage.haskell.org/package/shh) : Simple shell scripting from Haskell.
5. [HSH](https://hackage.haskell.org/package/HSH) : Library to mix shell scripting with Haskell programs.

By now, it should be evident to you that requiring minimum learning of a new eDSL, aka "noDSL" to be tongue-in-cheek,
sets this project apart to offer a viable alternative to Haskell enthusiasts.

# TODOs For The Beta (0.0.1.0) Release

**Known Bugs and Limitations**

- [x] Test with GHC 9.2, 9.4, 9.6, 9.8.
- [x] User pragma fields should be rearranged.
- [x] stdin doesn't work interactively.
- [ ] withSystemTempDirectory is not water-proof.
- [ ] Better quoting syntax, e.g. "!{ ... }": to replace the `PyF` parser or work with the upstream to reuse.

**Features**

- [x] THSH script loader, which uses "cabal run" single-script mode.
- [x] `FnFunction` for text package.
- [x] IO variants for `FnFunction` instances.
- [ ] Pass process args to the `__main__` script.
- [ ] `LineReadFn` for SoP types.
- [ ] Support `stack` in the loader.
- [ ] Support `rungc` in the loader.
- [ ] Support alternative base through a "--alternative-base" option.

**Maintainability**

- [ ] CI system.
- [ ] More test cases.

**DevX**

- [x] Curate a live demo.
- [x] Publish to hackage.
- [ ] A "how does it work" section in README.
