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
