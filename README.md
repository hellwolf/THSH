> [!NOTE]
>
> This project is a work in progress, and a proof-of-concept (PoC) is planned for the
> [MuniHac2024](https://munihac.de/2024.html) event.

THSH
====

**THSH** (**T**emplate **H**askell **SH**ell) is a "noDSL" approach to mixing shell scripting with Haskell programs
using Template Haskell.

# Key Features

## Gradual Approach

The key goal of the project is to enable a gradual approach to adding strongly-typed Haskell code to enhance your shell
scripting experience.

It is gradual because you can wrap any shell script into a Template Haskell quasi-quotation with `thsh`:

```sh
[|thsh\
echo "Hello Haskell!"
|]
```

## Pipe To Haskell Functions

> [!WARNING]
>
> The current PoC uses the PyF project's internal code to achieve parsing logic, which presents difficulty in choosing a better
> Haskell code delimiters other than "« ... »", for now. In the next version, a more palatable syntax should be
> achieved, e.g. "!{ ... }".

You can pipe the entire outputs from a process to a Haskell function:

```sh
[thsh|\
curl -s https://example.org/ | «fn (ContentFn (\content -> "Number of occurrence of the word 'example' is "
    <> show (length (filter ((== "examples"). fmap toLower) . words $ content))
    <> "\n"
))»
|]
```

--> "Number of occurrences of the word 'example' is 1"

You can also process each line independently:

```sh
[thsh|\
«fn lsum» <<EOF
("apple",  1.2, 100.2)
("orange", 2.0, 34.2)
EOF
|] where lsum = LineReadFn
                (\ (_ :: String, price, quantity) s -> let s' = s + price * quantity
                                                       in (s', Just (show s')))
                (0.0 :: Float)
```

--> "Sum of the sales: 188.64"

> [!NOTE]
>
> More pre-defined function patter will be developed.

## Composable

The `thsh` quoted code can compose with each other:

```haskell
s0 :: Script
s0 = [thsh| head -n1 | bc |]

test :: IO ExitCode
test = let s0 = [thsh| head -n1 | bc |]
       in runFuncletWithStdHandles [thsh|\
for i in `seq 0 10`;do
  expr="2 ^ $i"
  echo -n "$expr = "
  echo $expr | «sh s0»
done
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

# Comparing To eDSL Solutions

We should note that there have been multiple projects allowing the mixing of Haskell code with shell scripting. All of them
require their users to learn an eDSL of their own.

Here is an incomplete list of these projects:

1. [turtle](https://hackage.haskell.org/package/turtle) : turtle is a reimplementation of the Unix command line
   environment in Haskell so that you can use Haskell as both a shell and a scripting language.
2. [shelly](https://hackage.haskell.org/package/shelly) : shell-like (systems) programming in Haskell
3. [shh](https://hackage.haskell.org/package/shh) : Simple shell scripting from Haskell
4. [HSH](https://hackage.haskell.org/package/HSH) : Library to mix shell scripting with Haskell programs.

By now, it should be evident to you that requiring minimum learning of a new eDSL, aka "noDSL" to be tongue-in-cheek, sets this project apart to offer a viable alternative to Haskell enthusiasts.

# (TODOs)

**Features**

- More `FnFunction` types.
- THSH script loader, which uses either cabalrun or ghci.
- Better quoting syntax, e.g. "!{ ... }": to replace the `PyF` parser or work with the upstream to reuse.

**Maintainability**

- CI system.
- More test cases.

**DevX**

- Make the project compatible GHC 9.6+.
- Publish to hackage.
- Curate a live demo.
