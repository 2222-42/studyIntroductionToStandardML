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


(* Q16.1
getc が(char, substring) reader型を持つことの確認
- val getc : substring -> (char * substring) option
- type('a,'b) reader = 'b -> ('a * 'b) option

さらに確認するならば、getc を引数とする関数を用いて、型エラーが起きないことでも確認することができる。
*)

val test = "1234A5B6C7D890"; 
val sub1 = Substring.extract (test,2,SOME 5);
val SOME(c1, rest1) = Substring.getc sub1;
fun isAlpha c = c >= #"a" andalso c <= #"z" orelse c >= #"A" andalso c <= #"Z";
fun isNum c = c >= #"0" andalso c <= #"9";
val (c2, rest2) = StringCvt.splitl isAlpha Substring.getc sub1;
val (c3, rest3) = StringCvt.splitl isNum Substring.getc sub1;
(* --end Q16.1 *)

fun decScan x = Int.scan StringCvt.DEC x;
val intScan = decScan Substring.getc;
val s = Substring.full "123 abc";
intScan s;

(* Q16.2 *)
fun readInt str = 
    let 
        val s = ref (Substring.full str)
    in
        fn () => case (intScan (!s)) of
            SOME (i, r) => (s := r ;SOME i)
          | NONE => NONE
    end;

val f = readInt "123 456 abc";
(* val it = SOME 123 : int option
- f();
val it = SOME 456 : int option
- f();
val it = NONE : int option *)
val g = readInt "123 abc 456";
(* - g ();
val it = SOME 123 : int option
- g ();
val it = NONE : int option *)
