import Printiest.Doc

open Doc

inductive Sexpr
| atom : String -> Sexpr
| list : Array Sexpr -> Sexpr

instance : Inhabited Sexpr := ⟨Sexpr.atom "default"⟩
instance : Coe String Sexpr := ⟨Sexpr.atom⟩

partial def Sexpr.pretty : Sexpr -> Doc
| atom s => Doc.Text s
| list es => 
  let inner := Doc.group (es.map pretty) " "
  "(" <> inner <> ")"

open Sexpr

def abcd : Sexpr := list #["a", "b", "c", "d"]
def abcd4 : Sexpr := list #[abcd, abcd, abcd, abcd]

def anSexpr : Sexpr := list #[
  list #["abcde", abcd4],
  list #["abcdefgh", abcd4]
]

-- The canonical "Prettiest" Sexpr demo
#eval IO.Prim.getStdout >>= anSexpr.pretty.renderStream 15
#eval IO.Prim.getStdout >>= anSexpr.pretty.renderStream 40
#eval IO.Prim.getStdout >>= anSexpr.pretty.renderStream 80

def bigSexpr0 := list #[anSexpr, anSexpr, anSexpr, anSexpr]
def bigSexpr1 := list #[bigSexpr0, bigSexpr0, bigSexpr0, bigSexpr0]
-- Don't bother trying to print these larger ones interactively (use leanpkg build bin)
def bigSexpr2 := list #[bigSexpr1, bigSexpr1, bigSexpr1, bigSexpr1]
def bigSexpr3 := list #[bigSexpr2, bigSexpr2, bigSexpr2, bigSexpr2]
def bigSexpr4 := list #[bigSexpr3, bigSexpr3, bigSexpr3, bigSexpr3]
def bigSexpr5 := list #[bigSexpr4, bigSexpr4, bigSexpr4, bigSexpr4]
def bigSexpr6 := list #[bigSexpr5, bigSexpr5]
def bigSexprTest (w : Nat) := bigSexpr6.pretty.renderString w

#eval IO.print (bigSexpr1.pretty.renderString 40)

-- Demo of the groupText thing.
def lorem := "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
#eval IO.Prim.getStdout >>= (groupTextFromString lorem).renderStream 20
#eval IO.Prim.getStdout >>= (groupTextFromString lorem).renderStream 40
#eval IO.Prim.getStdout >>= (groupTextFromString lorem).renderStream 80

namespace test1
def L : Doc := "points:"
def R := Doc.group #["1. first point", "2. second point", "3. third point"] " "

/-
The two hang operators; "alwaysHang" and "hang", where the latter will only
do hanging indentation if the given width can't fit everything on one line.
-/
#eval IO.Prim.getStdout >>= (hang 4 L R).renderStream 80
#eval IO.Prim.getStdout >>= (hang 4 L R).renderStream 20
#eval IO.Prim.getStdout >>= (alwaysHang 4 L R).renderStream 80
#eval IO.Prim.getStdout >>= (alwaysHang 4 L R).renderStream 20
end test1

/-
When there are linebreaks introduced by Flush/vConcat, concatenation
becomes "tetris-style", which is nice for code formatting.
-/
def tetrisDemo1 : Doc := ("xxxxxxxx" <n> "xxxx") <> ("########" <n> "####") <> ("........" <n> "....")
#eval IO.print (tetrisDemo1.renderString 80)

def tetrisDemo2 : Doc := ("abc" <n> "d") <> ("ghi" <n> "jkl") <> ("mno" <n> "pqr")
#eval IO.print (tetrisDemo2.renderString 80)

/-
If you want "normal" in-line concatenation, just don't use the newline operator (or otherwise insert a Flush).
-/
def concatDemo : Doc := ("abc" <> "def") <+> ("ghi" <> "jkl") <+> ("mno" <> "pqr")
#eval IO.print (concatDemo.renderString 80)

def gitOptions : List String :=
[
    "[--version]",
    "[--help]",
    "[-C <path>]",
    "[-c <name>=<value>]",
    "[--exec-path[=<path>]]",
    "[--html-path]",
    "[--man-path]",
    "[--info-path]",
    "[--p|--paginate|-P|--no-pager]",
    "[--no-replace-objects]",
    "[--bare]"
]

def helpText := "'git help -a' and 'git help -g' list available subcommands and some concept guides. See 'git help <command>' or 'git help <concept>' to read about a specific subcommand or concept. See 'git help git' for an overview of the system."
def gitCli := 
  let grp := group #["git" <+> groupText gitOptions, "<command>", "[<args>]"] " "
  --(hang 4 "usage:" ("git" <+> groupText gitOptions)) 
  (hang 4 "usage:" grp)
  <n> Nil 
  <n> groupTextFromString helpText

/-
Recreates a portion of the `git --help` output showing the ability
to render a CLI to best fit a given terminal width (ideally this would be
read from the user's terminal and then printed accordingly).
-/
#eval IO.Prim.getStdout >>= gitCli.renderStream 80
#eval IO.Prim.getStdout >>= gitCli.renderStream 40
#eval IO.Prim.getStdout >>= gitCli.renderStream 20

-- Demo of the helper function for creating a separated and surrounded list of items.
#eval IO.Prim.getStdout >>= (Doc.encloseSep "{" (gitOptions.map (fun x => Doc.Text x)).toArray "," "}" 4).renderStream 40

-- Force a group to be horizontal
def restrict_h := Doc.group (kind := GroupKind.horizontalCode) #["A", "B", "C"] ", "
#eval IO.Prim.getStdout >>= restrict_h.renderStream 1

-- Force a group to be vertical
def restrict_v := Doc.group (kind := GroupKind.vertical) #["A", "B", "C"] " "
#eval IO.Prim.getStdout >>= restrict_v.renderStream 40

