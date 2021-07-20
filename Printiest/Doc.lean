import Std.Data.DList
import Printiest.Util
import Printiest.Measure

open Std (DList)
open Spaces Side

/-
GroupKind controls how Groups are rendered. Generally the choice is going to be whether
you're formatting code or formatting text. If you don't want to choose, you don't have to,
but this will have some impact performance since we potentially have 3 branches instead of 2 at every
`Group`. Most of the time, one of the choices will dominate the other (have strictly more attractive
dimensions) meaning the alternative won't be considered, but there will be outliers.

The main issue is that the horizontal layout scheme originally implemented in `Prettiest` doesn't
lay out blocks of human-language text well, because it wants all the leading elements to be 
non-zero height, and allows the LAST one to be non-zero, then does a fold right, so we can have 
things that hang/indent nicely, like:
```
a b c d e f
      g h i
      j k l m n o
          p q r s t u
```

But for text, we want to have a block, for which the FIRST element can be non-zero height, but
the rest need to be zero-height, and we want to fold LEFT, so we get things like:
```
Lorem ipsum dolor sit amet, consectetur
adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna
aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris
nisi ut aliquip ex ea commodo consequat.
```

Here, if we began with a "first" element of 
```
Lorem ipsum dolor sit amet, consectetur
adipiscing elit, 
```
and a second element of `sed`, we want 'sed' to be "tetris'd" into the
rest of the block if we began by wanting the whole thing to be grouped.
For that to happen, we have to allow non-zero height elements on the LHS.

G(G(G(G("", Lorem), ipsum) dolor) sit)...

We allow the first element to be non-zero so we can nicely tetris together blocks of text
without forcing them to be new paragraphs.

GroupKind is supposed to be a bitfield
1 |-> HorizontalCode
2 |-> HorizontalText
4 |-> Vertical

normal is horizontalCode + Vertical
text is horizontalText + Vertical
-/
abbrev GroupKind := UInt8
def GroupKind.horizontalCode : GroupKind := 1
def GroupKind.horizontalText : GroupKind := 2
def GroupKind.vertical : GroupKind := 4
def GroupKind.normal : GroupKind := GroupKind.horizontalCode.lor GroupKind.vertical
def GroupKind.text : GroupKind := GroupKind.horizontalText.lor GroupKind.vertical
def GroupKind.all : GroupKind := (GroupKind.horizontalText.lor GroupKind.vertical).lor GroupKind.horizontalCode

inductive Doc
| Nil : Doc
| Text (s: String) : Doc
| Concat (l: Doc) (r: Doc) : Doc
| Flush (d: Doc) : Doc
| Group (ds: Array (Nat × Doc)) (sep: String) (kind: GroupKind) : Doc

instance : Inhabited Doc := ⟨Doc.Nil⟩
instance : Coe String Doc := ⟨Doc.Text⟩ 

def Doc.vConcat (l r : Doc) : Doc := Doc.Concat (Flush l) r

infixl:65 " <> "  => Doc.Concat
-- Concatenate d1 and d2, putting a single space between them.
infixl:65 " <+> " => fun d1 d2 => d1 <> Doc.Text " " <> d2
/- 
Concatenate d1 and d2 vertically:
d1
d2
-/
infixl:65 " <n> " => fun d1 d2 => Doc.vConcat d1 d2

/-
The default group style is `normal`. Generally if you're grouping a block of text
you want to use groupText to make it look nice, which is different enough that 
it has a slightly different name.

You can limit it to `horizontal` or `vertical` if you KNOW you want to restrict
the rendering to a certain look, or if you're concerned about performance for 
either extremely large or degenerate Docs.
-/
def Doc.group 
  (docs : Array Doc) 
  (sep : String) 
  (kind : GroupKind := GroupKind.normal) : Doc := 
  Group (docs.map $ fun x => (0, x)) sep kind

def Doc.groupIndent 
  (docs : Array (Nat × Doc)) 
  (sep : String) 
  (kind : GroupKind := GroupKind.normal) : Doc := 
  Group docs sep kind

/-
For creating blocks of text.
-/
def Doc.groupText (l: List String) : Doc :=
  match l with
  | [] => Doc.Nil
  | hd :: tl => 
  let base := Doc.Text hd
  tl.foldl (fun sink next => Doc.Group #[(0, sink), (0, Doc.Text next)] " " GroupKind.text) base

/--
For creating blocks of text split on a character identified by `p`.
-/
def Doc.groupTextFromString (s : String) : Doc := Doc.groupText $ s.split (fun c => c = ' ')

def blank (n : Nat): String :=
  let rec aux (n : Nat) (acc : String) : String :=
    match n with
    | 0 => acc
    | (n+1) => aux n (acc ++ " ")
  aux n ""

def Doc.alwaysHang (indent : Nat) (upper : Doc) (lower : Doc) : Doc := 
  Doc.groupIndent #[(0, upper), (indent, lower)] " " (kind := GroupKind.vertical)

/-
Given `self` and `ys`, this will print bothon the same line
iff they fit, otherwise it will print `xs`, then hang `ys`.
WARNING: This will probably not look good if the RHS is a group.
```text
  EITHER
  self ys1 ys2
  OR
  self
      ys1
      ys2
```
-/
def Doc.hang (indent : Nat) (upper : Doc) (lower : Doc) (sep : String := " ") : Doc := 
  Doc.groupIndent #[(0, upper), (indent, lower)] sep

/-
Make IE
```
[
  x,
  y,
  z
]
```
-/
def Doc.encloseSep (l : Doc) (docs: Array Doc) (sep: String) (r : Doc) (indent : Nat) : Doc :=
  match docs.get? docs.size.pred with
  | none => Doc.Concat l r
  | some last => 
    let sub := (docs.toSubarray 0 docs.size.pred).toArray
    let middle := sub.map (fun d => Doc.Concat d sep)
    let inner := Doc.group (middle.push last) ""
    Doc.groupIndent #[(0, l), (indent, inner), (0, r)] ""

/-
Calcualte the sizes/shapes of different possible outcomes.

Ideally this would measure strings based on unicode graphemes instead
of length, but for now we're working with what we have.
-/
partial def Doc.measure (w : Nat) : Doc -> Array Measure
| Nil => #[Inhabited.default]
| Text t => #[Measure.text t]
| Concat l r => 
  let ls := l.measure w
  let rs := r.measure w
  pareto w $ flatCartesian Measure.concat ls rs

| Flush d => pareto w $ (d.measure w).map Measure.flush
| Group ds sep kind => 
  let inner := ds.map (fun (indent, d) => (indent, d.measure w))
  let sep := Measure.text sep
  let horizontal := if kind.land GroupKind.horizontalCode != 0 then measureH inner w sep else #[]
  let text := if kind.land GroupKind.horizontalText != 0 then measureT inner w sep else #[]
  let vertical := if kind.land GroupKind.vertical != 0 then measureV inner w else #[]
  pareto w $ horizontal ++ text ++ vertical

partial def Doc.renderAux
  {m : Type -> Type} 
  [inst1: Monad m] 
  [Inhabited (m (RenderState σ))] 
  [HasWrite σ m]
  (w : Nat) 
  : Doc -> StateT (RenderState σ) m Unit
| Nil => return
| Text txt => do
  let blank := match (<- get).spaces with
  | Hot n => blank n
  | _ => ""
  HasWrite.tell blank
  HasWrite.tell txt
  modify $ fun s' => { s' with spaces := Cold (txt.length + s'.spaces.toNat)}
 
| Concat l r => do
  fun s => l.renderAux w { s with side := Left }
  fun s => r.renderAux w { s with side := Right }
  modify $ fun s => { s with spaces := s.spaces.toCold }

| Flush d => do
  let s0 <- get
  d.renderAux w
  HasWrite.tell "\n"
  match s0.side with
  | Left => modify $ fun s' => { s' with spaces := s0.spaces.toHot }
  | Right => modify fun s' => { s' with spaces := Hot 0 }

| Group ds sep kind => do
  let s0 <- get
  let mut diffs_total := 0
  let mut iterations : Nat := 0

  let last <- modifyGet $ fun s => 
    match s.choices with
    | [] => (Choice.V 0, s0)
    | hd :: tl => (hd, { s with choices := tl })

  match last with
  | Choice.H lw =>
      for (_, d) in ds do
        let ind_num := if iterations + 1 = ds.size then s0.spaces.toNat + diffs_total else s0.spaces.toNat
        let ind := if (<- get).spaces.isHot then Hot ind_num else Cold ind_num
        fun s => d.renderAux w { s with side := s0.side, spaces := ind }
        if iterations + 1 != ds.size then 
            diffs_total := (diffs_total + sep.length + (<- get).spaces.toNat) - ind.toNat
            HasWrite.tell sep
        iterations := iterations + 1
      modify $ fun s => { s with spaces := Cold (lw + s0.spaces.toNat) }

  | Choice.V lw => 
        for (local_ind, d) in ds do
          let ind_num := s0.spaces.toNat + local_ind
          let ind := if iterations = 0 && (<- get).spaces.isCold then Cold ind_num else Hot ind_num
          fun s => d.renderAux w { s with side := s0.side, spaces := ind }
          if iterations + 1 != ds.size then HasWrite.tell "\n"
          iterations := iterations + 1
        modify $ fun s => { s with spaces := Cold (lw + s0.spaces.toNat) }

  | Choice.T =>
        for (_, d) in ds do
          let ind_num := s0.spaces.toNat
          let ind := if iterations = 0 && (<- get).spaces.isHot then Hot ind_num else Cold ind_num
          fun s => d.renderAux w { s with side := s0.side, spaces := ind }
          if iterations + 1 != ds.size then HasWrite.tell sep
          iterations := iterations + 1
        modify $ fun state => { state with spaces := s0.spaces.toCold }

/-
Render a Doc with the given width directly to a stream.
I'm not sure how Lean handles writing to a stream internally; I know in Rust there are
performance issues without buffered writers
-/
def Doc.renderStream (doc : Doc) (width: Nat) (stream: IO.FS.Stream) : IO Unit := do 
let sils := doc.measure width
let hd := sils.get! 0
let choices := hd.choices.toList
let _ <- doc.renderAux width (RenderState.new stream choices)

/-
Render a Doc with the given width into a string accumulator.
-/
def Doc.renderString (doc : Doc) (width: Nat) : String := do 
let sils := doc.measure width
let hd := sils.get! 0
let choices := hd.choices.toList
let (_, rs) <- doc.renderAux width (RenderState.new "" choices)
rs.sink
