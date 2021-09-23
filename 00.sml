fun f3 (a,b)(c::d)e =
    if(!b)=(c mod 2 = 0)
    then fn x => e
    else fn(y,z) => valOf(z) andalso a mod 2 = 1

fun moja (a,b) =
    fn y => 2

fun x {a=b, c=d} h =
    case (b,d) of
    (SOME e, f::g) => e andalso (x {a=b, c=g}h)
|   (NONE, f::g) => f andalso (x {a=b, c=g}h)
|   _ => h

fun f1 (a,b,c::d)[i,j]=
    if c
    then fn a => b(SOME i)
    else fn b => a(j+1)

fun nekaj a b c d e f = a(b c) (d e)

fun test a b c d= a(b c) d

fun f x y =
 case x(y) of
     g::r => SOME (Real.max((!g)(true),
		     if isSome(f (fn _ => r) y)
		     then 1.0
		     else 0.0))
   | _ => NONE
