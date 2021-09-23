structure Rational =
struct
    (* Definirajte podatkovni tip rational, ki podpira preverjanje enakosti. *)
    datatype rational = Ulomek of (int*int) | Celo of int

    (* Definirajte izjemo, ki se kliče pri delu z neveljavnimi ulomki - deljenju z nič. *)
    exception BadRational

    fun gcd(a : int, b : int): int = 
        if   b = 0
        then a
        else gcd (b, a mod b)

    (* Vrne racionalno število, ki je rezultat deljenja dveh podanih celih števil. *)
    fun makeRational (x,y) = if x mod y = 0 then Celo (x div y) else Ulomek ((x div gcd(x,y)),(y div gcd(x,y)))

    (* Vrne nasprotno vrednost podanega števila. *)
    fun neg (rat) = case rat of
                        Celo x => Celo (~x)
                    |   Ulomek (x,y) => Ulomek (~x,y)

    (* Vrne obratno vrednost podanega števila. *)
    fun inv (rat) = case rat of
                        Celo x => makeRational(1,x)
                    |   Ulomek (x,y) => makeRational(y,x)

    (* Funkcije za seštevanje in množenje. Rezultat vsake operacije naj sledi postavljenim pravilom. *)
    fun add (x,y) = case x of
                        Celo xx => (case y of
                            Celo xxx => Celo (xx+xxx)
                        |   Ulomek (xxx,yyy) => makeRational((xx*yyy+xxx),(yyy)))
                    |   Ulomek (xx,yy) => (case y of
                            Celo xxx => makeRational((xxx*yy+xx),(yy))
                        |   Ulomek (xxx,yyy) => makeRational((xx*yyy+xxx*yy),(yy*yyy)))

    fun mul (x,y) = case x of
                        Celo xx => (case y of
                            Celo xxx => Celo (xx*xxx)
                        |   Ulomek (xxx,yyy) => makeRational((xx*xxx),(yyy)))
                    |   Ulomek (xx,yy) => (case y of
                            Celo xxx => makeRational((xxx*xx),(yy))
                        |   Ulomek (xxx,yyy) => makeRational((xx*xxx),(yy*yyy)))

    (* Vrne niz, ki ustreza podanemu številu.
        Če je število celo, naj vrne niz oblike "x" oz. "~x".
        Če je število ulomek, naj vrne niz oblike "x/y" oz. "~x/y". *)
    fun toString (x) = case x of
                        Celo xx => Int.toString(xx)
                    |   Ulomek (xx,yy) => Int.toString(xx) ^ "/" ^ Int.toString(yy)
end;

signature EQ =
sig
    type t
    val eq : t -> t -> bool
end

signature SET =
sig
    (* podatkovni tip za elemente množice *)
    type item

    (* podatkovni tip množico *)
    type set

    (* prazna množica *)
    val empty : set

    (* vrne množico s samo podanim elementom *)
    val singleton : item -> set

    (* unija množic *)
    val union : set -> set -> set

    (* razlika množic (prva - druga) *)
    val difference : set -> set -> set

    (* a je prva množica podmnožica druge *)
    val subset : set -> set -> bool
end

functor SetFn (Eq : EQ) :> SET where type item = Eq.t=
struct
    (* podatkovni tip za elemente množice *)
    type item = Eq.t

    (* podatkovni tip množico *)
    type set = item list ref

    (* prazna množica *)
    val empty : set = ref []

    (* vrne množico s samo podanim elementom *)
    fun singleton element: set = ref [element]

    (* unija množic *)
    fun member_of (itm:item, list:item list): bool = List.exists (fn (x) => Eq.eq x itm) list
    fun union (set1:set) (set2:set): set = ref (List.foldl (fn (x, acc) => if member_of(x,acc) then acc else acc@[x]) (!set1) (!set2))

    (* razlika množic (prva - druga) *)
    fun difference (set1:set) (set2:set): set = set1

    (* a je prva množica podmnožica druge *)
    fun subset (set1:set) (set2:set): bool = true
end;