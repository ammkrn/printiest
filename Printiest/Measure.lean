import Std.Data.DList
import Printiest.Util

open Std (DList)

/-
Note: all of the comparisons (equality, LE, etc) use ONLY the
measure's dimensions; we're not concerned with the choices.
-/
structure Measure where
(height : Nat)
(maxWidth : Nat)
(lastWidth : Nat)
(choices : DList Choice)

instance Measure.Inhabited : Inhabited Measure where
  default := ⟨0, 0, 0, DList.empty⟩

instance : Ord Measure where
  compare a b :=
  match Ord.compare a.height b.height with
  | Ordering.lt => Ordering.lt
  | Ordering.gt => Ordering.gt
  | Ordering.eq => 
    match Ord.compare a.maxWidth b.maxWidth with
    | Ordering.lt => Ordering.lt
    | Ordering.gt => Ordering.gt
    | Ordering.eq => Ord.compare a.lastWidth b.lastWidth

instance : LE Measure :=
  ⟨fun m1 m2 => 
  match Ord.compare m1 m2 with
  | Ordering.lt => True
  | Ordering.eq => True
  | _ => False⟩

instance : LT Measure := ⟨fun m1 m2 => Ord.compare m1 m2 = Ordering.lt⟩

instance Measure.decLe (m1 m2 : Measure) : Decidable (LE.le m1 m2) := 
  match h:Ord.compare m1 m2 with
  | Ordering.lt => by
    simp only [LE.le, h] 
    exact (isTrue trivial)
  | Ordering.gt => by
    simp only [LE.le, h] 
    exact (isFalse id)
  | Ordering.eq => by
    simp only [LE.le, h]
    exact (isTrue trivial)

def Measure.lt (s1 s2 : Measure) : Bool :=
match Ord.compare s1 s2 with
| Ordering.lt => true
| _ => false

def Measure.dominates (s t : Measure) : Bool := 
  s.height <= t.height
  && s.maxWidth <= t.maxWidth
  && s.lastWidth <= t.lastWidth

def Measure.toString (m : Measure) : String := s!"⟨{m.height}, {m.maxWidth}, {m.lastWidth}, {m.choices.toList}⟩"
instance : ToString Measure := ⟨Measure.toString⟩

def Measure.beq (m1 m2 : Measure) : Bool := 
  match Ord.compare m1 m2 with
  | Ordering.eq => true
  | _ => false

instance : BEq Measure := ⟨Measure.beq⟩

def Measure.text (s : String) : Measure := ⟨0, s.length, s.length, DList.empty⟩

def Measure.concat (l r : Measure) : Measure := 
  ⟨l.height + r.height, 
  l.maxWidth.max (l.lastWidth + r.maxWidth),
  l.lastWidth + r.lastWidth,
  l.choices ++ r.choices⟩

def Measure.flush (m : Measure) : Measure := ⟨m.height.succ, m.maxWidth, 0, m.choices⟩

def Measure.fits (m: Measure) (width : Nat) : Bool := m.maxWidth <= width

def Measure.cons (m : Measure) (c : Choice) : Measure := { m with choices := m.choices.cons c }

def Measure.vConcat (l r : Measure) : Measure := l.flush.concat r

/-
  the end goal is some document that's a combination of a, b, and c.
  [
    possible layotus for a := [a1, a2, .. an]
    possible layouts for b := [b1, b2, .. bn]
    possible layouts for c := [c1, c2, .. cn]
  ]

  For certain horizontal groups, all of the elements have to be zero height.

  If it turns out that (for example) there are no possibilities for `a` that are zero height,
  then we need to detect that and abandon the horizontal grouping altogether, using
  vertical grouping instead.
-/
def zeroHeights (grps : Array (Nat × (Array Measure))) : Option (Array (Array Measure)) :=
  let zeroes_only := grps.map (fun ⟨_, grp⟩ => grp.filterMap (fun s => if s.height = 0 then some s else none))
  if zeroes_only.any (fun grp => grp.size = 0) 
  then none 
  else some zeroes_only


def arrayBind {A B : Type} (xs : Array A) (f : A -> Array B) : Array B :=
  xs.foldl (fun sink next => sink ++ f next) #[]

/--
from [x1, x2, .. xn], [y1, y2, .. yn] make [f x1 y1, f x1 y2, .. f xn yn] 
-/
def flatCartesian {A B C : Type} (f : A -> B -> C) (xs : Array A) (ys : Array B) : Array C :=
arrayBind xs (fun x => ys.map (fun y => f x y))


/--
If there are any measure that fit in the specified width, keep only those.
If there aren't, keep the single least wide (the one that will overflow the least)
-/
partial def discardInvalid (w : Nat) (sils: Array Measure): Array Measure :=
  let fit := sils.filter (fun s => s.fits w)
  if fit.size > 0 then fit else 
  match sils.get? 0 with
  | none => panic "discardInvalid should never get an empty array"
  | some hd => #[sils.foldl (fun least challenger => 
    if challenger.maxWidth < least.maxWidth || (least.maxWidth = challenger.maxWidth && (least <= challenger))
    then challenger
    else least) hd]

/--
Deduplicate a sorted array of measures
-/
partial def dedupSorted (rem: Array Measure) : Array Measure :=
  let rec aux (n : Nat) (out: Array Measure) : Array Measure :=
  match rem.get? n, rem.get? n.succ with
  | none, _ => out
  | some x, none => out.push x
  | some x, some y => if x != y then aux n.succ (out.push x)  else aux n.succ out
  aux 0 #[]

/--
From a list of measures, remove all those that are dominated 
since we know they're not going to give an optimal layout
-/
partial def dominant (rem: Array Measure) : Array Measure :=
  let rec aux (n : Nat) (out : Array Measure) : Array Measure :=
  match rem.get? n with
  | none => out
  | some x => if out.any (fun m => m.dominates x) then aux n.succ out else aux n.succ (out.push x)
  aux 0 #[]


def sort (input : Array Measure) : Array Measure := input.qsort (fun m1 m2 => Measure.lt m1 m2)

def pareto (w : Nat) : Array Measure -> Array Measure := dominant ∘ dedupSorted ∘ sort ∘ (discardInvalid w)

/-
Calculate the measures for different possible horizontal groups.
This is the regular one where the LAST element is the only one 
allowed to be non-zero height

From 
[
  [a1, a2, a3],
  [b1, b2, b3],
  [c1, c2, c3]
]

[(a1, b1, c1), (a1, b1, c2), .., (a3, b3, c3)]
-/
def measureH
  (grps : Array (Nat × (Array Measure))) 
  (width : Nat) 
  (sep : Measure) 
  : Array Measure := 
match grps.back? with
| none => #[]
| some ⟨_, hd⟩ => 
  match zeroHeights (grps.toSubarray 0 (grps.size - 1)) with
  | none => #[]
  | some rest => 
  let concat_sep := fun (s1 s2 : Measure) => (s1.concat sep).concat s2
  let inner := rest.foldr (fun next sink => pareto width $ flatCartesian concat_sep next sink) hd
  inner.map (fun measure => measure.cons (Choice.H measure.lastWidth))


-- The irregular one for text.
-- The FIRST element is the only one allowed to be non-zero height
def measureT
  (grps : Array (Nat × (Array Measure))) 
  (width : Nat) 
  (sep : Measure) 
  : Array Measure := 
match grps.get? 0 with
| none => #[]
| some ⟨_, hd⟩ => 
  match zeroHeights (grps.toSubarray 1 (grps.size)) with
  | none => #[]
  | some rest => 
  let concat_sep := fun m1 m2 => (m1.concat sep).concat m2
  let inner := rest.foldl (fun sink next => pareto width $ flatCartesian concat_sep sink next) hd
  inner.map (fun measure => measure.cons (Choice.T))

  
/-
Calculate the measures for different possible vertical groups.

From 
[
  [a1, a2, a3],
  [b1, b2, b3],
  [c1, c2, c3]
]

[(a1, b1, c1), (a1, b1, c2), .., (a3, b3, c3)]
-/
def measureV
  (grps : Array (Nat × (Array Measure))) 
  (width : Nat) 
  : Array Measure := 
-- for each x in [x1, x2, .. xn], indent it by the proper amount by concatenating
-- some chunk on the left hand side.
let indented := grps.map (fun ⟨amt, grp⟩ => grp.map (fun x => (Measure.mk 0 amt 0 DList.empty).concat x))
match indented.get? 0 with
| none => #[]
| some hd => 
  let rest := indented.toSubarray 1 (grps.size)
  let concat_nl := fun (s1 s2 : Measure) => s1.vConcat s2
  let inner := rest.foldl (fun sink next => pareto width $ flatCartesian concat_nl sink next) hd
  inner.map (fun measure => measure.cons (Choice.V measure.lastWidth))

