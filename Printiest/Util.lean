
inductive Choice
| H : Nat -> Choice
| V : Nat -> Choice
| T : Choice

instance : Inhabited Choice := ⟨Choice.H 0⟩

def Choice.toString (c : Choice) : String :=
  match c with
  | Choice.H n => s!"H({n})"
  | Choice.V n => s!"V({n})"
  | Choice.T   => "T"
instance : ToString Choice := ⟨Choice.toString⟩

/-
Side and Spaces are helper types to keep track of info during rendering;
we need these to allow for streaming output.

Side keeps track of whether we're on the left or right side of a Concat
so we can determine how linebreaks/flushes should behave.

Spaces determines whether a given indentation is actually supposed to be
rendered or not.
-/
inductive Side
| Left : Side
| Right : Side

instance : Inhabited Side := ⟨Side.Left⟩

inductive Spaces
| Hot : Nat -> Spaces
| Cold : Nat -> Spaces

instance : Inhabited Spaces := ⟨Spaces.Cold 0⟩

def Spaces.toHot : Spaces -> Spaces
| Hot n => Hot n
| Cold n => Hot n

def Spaces.toCold : Spaces -> Spaces
| Hot n => Cold n
| Cold n => Cold n

def Spaces.toNat : Spaces -> Nat
| Hot n => n
| Cold n => n

def Spaces.isHot : Spaces -> Bool
| Hot _ => true
| Cold _ => false

def Spaces.isCold (s : Spaces) : Bool := !s.isHot

open Spaces Side

structure RenderState (A : Type) where
(sink: A)
(choices: List Choice)
(side: Side)
(spaces: Spaces)

instance {A : Type} : Stream (RenderState A) Choice where
  next? rs :=
  match rs.choices with
  | [] => none
  | hd :: tl => some (hd, { rs with choices := tl })

def RenderState.new {A : Type} (a : A) (choices: List Choice) : RenderState A :=
  ⟨a, choices, Left, Cold 0⟩

instance : Inhabited (RenderState String) := ⟨RenderState.new "" []⟩

structure Options where
(width : Nat)

class HasWrite (σ : Type) (m : Type -> Type v) where
(tell : String -> StateT (RenderState σ) m Unit)

instance : HasWrite String Id := {
  tell := fun s => fun st => return ((), { st with sink := st.sink.append s })
}

instance : HasWrite IO.FS.Stream IO := {
  tell :=
  fun s st => do
  st.sink.write s.toUTF8
  return ((), st)
}
