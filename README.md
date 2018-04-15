# stargazer

Structurally typed arguments and zero extra rubbish

```haskell
-- Stargazer's, very small, API

-- An option can take a single string argument or nothing at all.
string  :: Schema String
nothing :: Schema ()

-- A sum type is built from a list of possible options.
oneOf :: [(String, Schema a)] -> Schema a

-- A product type is built from a list of possible options,
-- composed applicatively.
allOf :: AllOf a -> Schema a

-- The fields of a product type can be singletons or lists.
once  :: String -> Schema a -> AllOf a
many  :: String -> Schema a -> AllOf [a]


-- Using the Stargazer API

-- We're going to parse command line options to set up this ADT
-- which describes how to build a set of Haskell packages.

-- The top level of the ADT is `Install`
data Install = Install { tool_    :: Tool
                       , packages :: [Package]
                       }
  deriving Show

data Tool = Stack Target
          | Cabal Target Build
  deriving Show

data Package = Package String String
  deriving Show

data Target = X86 | X64
  deriving Show

data Build = OldBuild | NewBuild
  deriving Show

-- Writing a schema to parse into our ADT

-- An install must have exactly one `--tool` and can have any
-- number of `--package`s
install :: Schema Install
install = allOf (Install <$> once "tool" tool
                         <*> many "package" package)

tool :: Schema Tool
tool = oneOf [ ("stack", allOf (Stack <$> once "target" target))
             , ("cabal", allOf (Cabal <$> once "target" target
                                      <*> once "build"  build))
             ]

target :: Schema Target
target = oneOf [ ("x86", X86 <$ nothing)
               , ("x64", X64 <$ nothing)
               ]

build :: Schema Build
build = oneOf [ ("old-build", OldBuild <$ nothing)
              , ("new-build", NewBuild <$ nothing)
              ]

package :: Schema Package
package = allOf (Package <$> once "name" string
                         <*> once "version" string)
```

This is what happens when you use it.

```
% ./example
error: Expected --tool

% ./example --tool
error: Expected one of --stack, --cabal

% ./example --tool --cabal
error: Expected --target

% ./example --tool --cabal --target
error: Expected one of --x86, --x64

% ./example --tool --cabal --target --x86
error: Expected --build

% ./example --tool --cabal --target --x86 --build
error: Expected one of --old-build, --new-build

% ./example --tool --cabal --target --x86 --build --new-build
Install {tool_ = Cabal X86 NewBuild, packages = []}

% ./example --tool --cabal --target --x86 --build --new-build \
            --package
error: Expected --name

% ./example --tool --cabal --target --x86 --build --new-build \
            --package --name aeson
error: Expected --version

% ./example --tool --cabal --target --x86 --build --new-build --package \
            --name aeson --version 0.6.0.0
Install {tool_ = Cabal X86 NewBuild, packages = [Package "aeson" "0.6.0.0"]}

% ./example --tool --cabal --target --x86 --build --new-build \
            --package --name aeson --version 0.6.0.0 0.4.0.0
error: Didn't expect to see 0.4.0.0 after 0.6.0.0

% ./example --tool --cabal --target --x86 --build --new-build \
            --package --name aeson --version 0.6.0.0 \
	    --package --name lens --version 10.0.0.0
Install {tool_ = Cabal X86 NewBuild,
         packages = [Package "lens" "10.0.0.0" ,Package "aeson" "0.6.0.0"]}
```

## To do

All the goodies:

* useful help

* short and long versions of arguments

* completion

# jsonargs

## Rationale

We're used to using command line options when running programs.  For
example, suppose I have a program that can run on a file consumed from
standard input, read from the filesystem, or downloaded from a URL.
The default is to read from standard input.  It can also process the
file forwards or backwards.  The default is forwards.

We might have configuration options that look like this.

```
dothing [--stdin]
        [--file=<filename>]
	[--url=<url> [--socks-proxy=<proxy>]]
        [--direction=<forwards|backwards>]
```

Nice tools exist to set up these options.  They can make `--stdin`,
`--file` and `--url` mutually exclusive.  They can ensure that
`--socks-proxy` is only specified when `--url` is specified.  They can
set up the default to be `--stdin` and `--direction=forwards`.

### The output of command line parsing

What do we eventually want to read into our program?  Probably we read
it into a nested structure of ADTs, something like

```haskell
data Opts = Source Source
          | Direction Direction

data Source = Stdin
            | File String
            | Url String (Maybe String)

data Direction = Forwards | Backwards
```

So this really is an example of parsing in the purest sense: taking a
linear sequence of tokens and turning them into a nested structure.
The more complicated the nested structure the more complicated our
parsing rules will have to be.

### The input of command line parsing

What do we put into our program?  It's not actually command line
arguments.  It's my intention as a user.  As a user I already mentally
group together the URL with the proxy address.  Why force me to
flatten them alongside the direction before reconstituting the nested
structure again inside the program?


## Two questions

1. Is it possible and sane to do this?  And if so,

2. Is it desirable to do this?

## What's missing

* completion

* short and long versions of arguments

* useful help

## Counterpoints

This is the only one I've found.

> https://stackoverflow.com/questions/26299755/json-for-command-line-arguments

I don't find it particularly convincing.
