(* SML source file. Copyright (c) by 2222-42 2020.
* Chap16.1
*)

(* 
signature SUBSTRING =
  sig
    eqtype char
    eqtype string
    type substring
    val sub : substring * int -> char
    val size : substring -> int
    val base : substring -> string * int * int
    val extract : string * int * int option -> substring
    val substring : string * int * int -> substring
    val full : string -> substring
    val string : substring -> string
    val isEmpty : substring -> bool
    val getc : substring -> (char * substring) option
    val first : substring -> char option
    val triml : int -> substring -> substring
    val trimr : int -> substring -> substring
    val slice : substring * int * int option -> substring
    val concat : substring list -> string
    val concatWith : string -> substring list -> string
    val explode : substring -> char list
    val isPrefix : string -> substring -> bool
    val isSubstring : string -> substring -> bool
    val isSuffix : string -> substring -> bool
    val compare : substring * substring -> order
    val collate : (char * char -> order) -> substring * substring -> order
    val splitl : (char -> bool) -> substring -> substring * substring
    val splitr : (char -> bool) -> substring -> substring * substring
    val splitAt : substring * int -> substring * substring
    val dropl : (char -> bool) -> substring -> substring
    val dropr : (char -> bool) -> substring -> substring
    val takel : (char -> bool) -> substring -> substring
    val taker : (char -> bool) -> substring -> substring
    val position : string -> substring -> substring * substring
    val span : substring * substring -> substring
    val translate : (char -> string) -> substring -> string
    val tokens : (char -> bool) -> substring -> substring list
    val fields : (char -> bool) -> substring -> substring list
    val app : (char -> unit) -> substring -> unit
    val foldl : (char * 'a -> 'a) -> 'a -> substring -> 'a
    val foldr : (char * 'a -> 'a) -> 'a -> substring -> 'a
  end
*)

val test = "1234567890"; 
val sub1 = Substring.extract (test,2,SOME 2);
val printer = Substring.string sub1;
val baseStr = Substring.base sub1;
val subToEnd = Substring.extract (test,2, NONE);
Substring.string subToEnd;
(* textではfullだが、現行のバージョンfull *)
val subAll = Substring.full(test);
val SOME(c1, rest) = Substring.getc sub1;
val SOME c1 = Substring.first sub1;
val SOME(c2, rest2) = Substring.getc rest;
Substring.getc rest2;
Substring.first rest2;
(*
- val SOME(c3, rest3) = Substring.getc rest2;
uncaught exception Bind [nonexhaustive binding failure]
  raised at: stdIn:30.5-30.43 *)

val trimmedL2 = Substring.triml 2 subToEnd;
Substring.string trimmedL2;
val trimmedR3 = Substring.trimr 3 subToEnd;
Substring.string trimmedR3;

val subFromSubString1 = Substring.slice (subAll,2,SOME 2);
Substring.string subFromSubString1;

val (a1,b1) = Substring.position test subFromSubString1;
Substring.string a1;
Substring.string b1;

val (a2,b2) = Substring.position "34" subAll;
Substring.string a2;
Substring.string b2;

val a3 = Substring.span(subAll, subFromSubString1);
Substring.string a3;

(* 
signature STRING_CVT =
  sig
    datatype radix = BIN | DEC | HEX | OCT
    datatype realfmt
      = EXACT | FIX of int option | GEN of int option | SCI of int option
    type('a,'b) reader = 'b -> ('a * 'b) option
    val padLeft : char -> int -> string -> string
    val padRight : char -> int -> string -> string
    val splitl : (char -> bool) -> (char,'a) reader -> 'a -> string * 'a
    val takel : (char -> bool) -> (char,'a) reader -> 'a -> string
    val dropl : (char -> bool) -> (char,'a) reader -> 'a -> 'a
    val skipWS : (char,'a) reader -> 'a -> 'a
    type cs
    val
        scanString : ((char,cs) reader -> ('a,cs) reader)
                      -> string -> 'a option
  end
*)


val paddedTest = StringCvt.padLeft #"!" 20 test;

val g1 = StringCvt.GEN(SOME 5);
Real.fmt g1 Math.pi;
