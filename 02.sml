datatype number = Zero | Succ of number | Pred of number;

(* Negira število a. Pretvorba v int ni dovoljena! *)
fun neg (a : number) : number =
    case a of
        Zero => Zero
    |   Succ i => Pred (neg i)
    |   Pred j => Succ (neg j);

(* Vrne vsoto števil a in b. Pretvorba v int ni dovoljena! *)
fun add (a : number, b : number) : number =
    case a of
        Zero => (case b of
            Zero => Zero
        |   Succ ii => Succ (add (Zero, ii))
        |   Pred jj => Pred (add (Zero, jj))
                )
    |   Succ i => Succ (add (i, b))
    |   Pred j => Pred (add (j, b));

(* Vrne rezultat primerjave števil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)

fun simp Zero = Zero
|   simp (Succ a) =
    (case simp a of
        Pred c => c
    |   c => Succ c)
|   simp (Pred a) =
    (case simp a of
        Succ c => c
    |   c => Pred c);

fun comp (a : number, b : number) : order =
    if (simp a) = (simp b) then EQUAL
    else LESS; (*Ni še rešeno za less in greater*)

datatype tree = Node of int * tree * tree | Leaf of int;

(* Vrne true, če drevo vsebuje element x. *)
fun contains (tree : tree, x : int) : bool =
    case tree of
        Leaf i => i = x
    |   Node (a,b,c) => if a=x then true else contains(b,x) orelse contains(c,x);

(* Vrne število listov v drevesu. *)
fun countLeaves (tree : tree) : int =
    case tree of
        Leaf i => 1
    |   Node (a,b,c) => 0 + (countLeaves b) + (countLeaves c);

(* Vrne število število vej v drevesu. *)
fun countBranches (tree : tree) : int =
    case tree of
        Leaf i => 0
    |   Node (a,b,c) => 2 + (countBranches b) + (countBranches c);

(* Vrne višino drevesa. Višina lista je 1. *)
fun height (tree : tree) : int =
    case tree of
        Leaf i => 1
    |   Node (a,b,c) => Int.max(
        (1+height(b)),(1+height(c))
    );

(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)
fun toList (tree : tree) : int list =
    case tree of
        Leaf i => [i] (*(toList b)::[a]::(toList c)*)
    |   Node (a,b,c) => [a];

(* Vrne true, če je drevo uravnoteženo:
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
fun isBalanced (tree : tree) : bool = true;

(* Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)
fun isBST (tree : tree) : bool = true;