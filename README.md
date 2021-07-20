# What is this?

A general algebraic [pretty-printer](https://en.wikipedia.org/wiki/Prettyprint#Programming_code_formatting) for [Lean 4](https://github.com/leanprover/lean4) using ideas from Jean-Philippe Bernardy's [Prettiest](https://github.com/jyp/prettiest). Bernardy also acknowledges work by Podkopaev, Boulytchev, Azero, and Swierstra in the addendum to [his paper](https://jyp.github.io/pdf/Prettiest.pdf). Broadly, the idea with this style of printer is that the library can save the user a lot of guesswork by doing some measuring/calculating to figure out the most optimal way to render a document, according to three very general principles:

```
1. Visibility: The printer will fit its output within the specified render width*
2. Legibility: The printer will respect the user's layout choices (groups)
3. Frugality: The printer will use as few lines as possible while respecting 1 and 2.

* The printer won't break up individual strings given by the user, so if you pass an 80 character string and specify a render width of 10, principle 1 will be violated (though only in that one spot)
```

Bernardy's insight was that while a naive implementation runs in exponential time, clever (but not complicated) pruning of the space of possible layouts makes things more or less linear (see the paper for details/graphs about this).

--- 

This implementation adds a third (optional) group choice for laying out segments of human-language text, and changes the implementation of the measuring and rendering so that the two phases are completely separate, which allows the final output to be written directly to an output stream rather than building up the whole output string in memory, then writing the string somewhere. 

Much of the Lean community (myself included) has yet to really explore the performance characteristics of Lean 4 and the language primitives, but the compiled version of this already performs very well compared to a very similar Rust implementation and Bernardy's Haskell version (which has more implementation differences, so a little bit apples to oranges).

## How do I use it?

Full examples of the printer's use can be found in the `Tests` module. Conceptually, the thing you want to print is built up by combining `Doc` elements which you then render to either a string with `Doc.renderString <width : Nat>`, or to an output stream with `Doc.renderStream <width : Nat> <stream>`.

There are three infix operators:
```
x <> y -- concatenate x and y
x <+> y -- concatenate x and y, with a space between
x <n> y -- vertically concatenate x and y
```

As well as the group operator, some helper functions around group, and some miscellaneous founctions like `hang`. Group is going to do most of the heavy lifting.

The main ways in which this (and Prettiest) differs from something like a Wadler/Leijen style printer is that the `Group` node does more of the work for you (you can still force a node to render as a certain orientation if you want), and for docs > 1 line in height, concatenation is "tetris-like". For existing programs that depend on "classic" concatenation, you can still achieve the same results by changing the manner in which the doc is built up in your program.

## Future work

+ Adding an annotation system.

+ Figuring out what API/integration to offer for external code or document formatters.

+ Figuring out the best way to make use of buffered streams/writers in Lean 4.

+ While work on verification is sort of blocked until well-founded recursion is implemented and the `partial` functions can be removed, it would be interesting to have a verified pretty-printer. It seems like the relevant correctness properties are:

1. That all of the Text nodes in a Doc tree are present in the output
2. That the rendered text appears in the correct order 

As long as you can show that all of the intended text made it into the output string, and did so in the right order, it seems like everything else is just a matter of taste, but if anyone knows more about this hit me up. 

## Examples

The full set of examples can be seen in the `Tests` module. The real strength of this printer is the versatility and ease of use of the `Group` node, and the way it automatically adjusts the rendering to fit the desired render width (users can still exert manual control over group rendering when they want to). For example, function for rendering generic S-expressions is extremely simple...:

```
partial def Sexpr.pretty : Sexpr -> Doc
| atom s => Doc.Text s
| list es => 
  let inner := Doc.group (es.map pretty) " "
  "(" <> inner <> ")"
```

But it will produce nice outputs automatically for different render widths, while leaving users the option of forcing a horizontal/vertical orientation to accomodate things like code formatters.

```
-- render width := 80 

((abcde ((a b c d) (a b c d) (a b c d) (a b c d)))
 (abcdefgh ((a b c d) (a b c d) (a b c d) (a b c d))))


-- render width := 40 

((abcde ((a b c d)
         (a b c d)
         (a b c d)
         (a b c d)))
 (abcdefgh ((a b c d)
            (a b c d)
            (a b c d)
            (a b c d))))


-- render width := 15

((abcde
  ((a b c d)
   (a b c d)
   (a b c d)
   (a b c d)))
 (abcdefgh
  ((a b c d)
   (a b c d)
   (a b c d)
   (a b c d))))
```

Recreating a portion of the `git --help` output, showing the ability to render a CLI in a manner suited to a user's detected terminal width.

```
-- render width := 80

usage: git [--version] [--help] [-C <path>] [-c <name>=<value>]
           [--exec-path[=<path>]] [--html-path] [--man-path] [--info-path]
           [--p|--paginate|-P|--no-pager] [--no-replace-objects] [--bare]
       <command>
       [<args>]

'git help -a' and 'git help -g' list available subcommands and some
concept guides. See 'git help <command>' or 'git help <concept>' to read
about a specific subcommand or concept. See 'git help git' for an overview
of the system.

-- render width := 40

usage:
    git [--version] [--help]
        [-C <path>] [-c <name>=<value>]
        [--exec-path[=<path>]] [--html-path]
        [--man-path] [--info-path]
        [--p|--paginate|-P|--no-pager]
        [--no-replace-objects] [--bare]
    <command>
    [<args>]

'git help -a' and 'git help -g' list
available subcommands and some concept
guides. See 'git help <command>' or 'git
help <concept>' to read about a specific
subcommand or concept. See 'git help
git' for an overview of the system.


-- render width := 20

usage:
    git [--version]
        [--help]
        [-C <path>]
        [-c <name>=<value>]
        [--exec-path[=<path>]]
        [--html-path]
        [--man-path]
        [--info-path]
        [--p|--paginate|-P|--no-pager]
        [--no-replace-objects]
        [--bare]
    <command>
    [<args>]

'git help
-a' and
'git help
-g' list
available
subcommands
and some
concept
guides. See
'git help
<command>'
or 'git
help
<concept>'
to read
about a
specific
subcommand
or concept.
See 'git
help git'
for an
overview of
the
system.
```

## License

Users are free to copy/distribute/modify this software according to the terms of either the Apache 2.0 license (to match Lean 4), or the GPLv3 (to match Prettiest). Most people will choose Apache 2.0, but licensees who would like to incorporate this in a larger GPL codebase will want to choose the GPLv3 option. Contributions will be accepted under the same dual-license arrangement. 
