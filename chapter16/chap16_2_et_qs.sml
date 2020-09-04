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

(* Q16.3 *)

val realScan = Real.scan Substring.getc;
fun readReal str = 
    let 
        val s = ref (Substring.full str)
    in
        fn () => case (realScan (!s)) of
            SOME (i, r) => (s := r ;SOME i)
          | NONE => NONE
    end;
val rf = readReal "1.23 4.56 1 abc";
val rg = readReal "1.23 abc 1 4.56";

val boolScan = Bool.scan Substring.getc;
fun readBool str = 
    let 
        val s = ref (Substring.full str)
    in
        fn () => case (boolScan (!s)) of
            SOME (i, r) => (s := r ;SOME i)
          | NONE => NONE
    end;
val bf = readBool "true false 1 abc";
val bg = readBool "false true abc 4.56";

(* Q16.4 *)

fun genericReadInt reader inputData = 
    let 
        val s = ref inputData
        val modifiedReader = Int.scan StringCvt.DEC reader
    in
        fn () => case (modifiedReader (!s)) of
            SOME (i, r) => (s := r ;SOME i)
          | NONE => NONE
    end;

(* - genericReadInt;
val it = fn : (char,'a) StringCvt.reader -> 'a -> unit -> int option *)

(* Q16.5 *)
(* 
TextIO > StreamIO
val input1 : instream -> (elem * instream) option 
*)
fun readIntFromStream ins = 
  let 
    (* TextIO.stream -> StreamIO.stream *)
    val s = TextIO.getInstream ins
  in
    genericReadInt TextIO.StreamIO.input1 s
  end;
(* - readIntFromStream;
val it = fn : TextIO.instream -> unit -> int option *)


fun readIntFromFile inf = 
  let 
    val ins = TextIO.openIn inf
    val reader = readIntFromStream ins
    fun loop L = 
      case reader () of 
        SOME i => loop(L@[i])
      | NONE => L
  in
    loop nil before TextIO.closeIn ins
  end;

(* readIntFromFile "E:/SMLProject/studyIntroductionToStandardML/chapter16/testChar.txt"; *)

(* --end Q16.5 *)

signature PARSE_URL = sig
  exception urlFormat
  datatype url = 
      HTTP of {host: string list, path: string list option, anchor: string option}
    | FILE of {path: string list, anchor: string option}
    | RELATIVE of {path: string list, anchor: string option}
  val parseUrl : string -> url
end;

fun toLower c =
    let
        val dif = (ord #"a") - (ord #"A")
    in
        if isUpper c then
            chr((ord c) + dif) 
        else c
    end;
fun lower s = implode (map toLower (explode s));

structure Url:PARSE_URL = struct
  structure SS = Substring
  exception urlFormat
  datatype url =       
      HTTP of {host: string list, path: string list option, anchor: string option}
    | FILE of {path: string list, anchor: string option}
    | RELATIVE of {path: string list, anchor: string option}
  fun parseHttp s = 
    let
      val s = if SS.isPrefix "://"  s then
                SS.triml 3 s
              else raise urlFormat
      fun neq c x = not (x = c)
      fun eq c x = c = x
      val (host, body) = SS.splitl (neq #"/") s
      val domain = map SS.string (SS.tokens (eq #".") host)
      val (path, anchor) =
        if SS.isEmpty body then (NONE, NONE)
        else 
          let val (p, a) = SS.splitl(neq #"#") body
          in (SOME (map SS.setring (SS.tokens (eq #"/") p)),
              if SS.isEmpty a then NONE
              else SOME(SS.string(SS.triml 1 a)))
          end
    in {host=domain, path=path, anchor=anchor}
    end
  (* fun parseFile s = 
    let 
      val (path, anchor) =
        if SS.isEmpty s then (NONE, NONE)
        else 
          let val (p, a) = SS.splitl(neq #"#") s
          in (SOME (map SS.setring (SS.tokens (eq #"/") p)),
              if SS.isEmpty a then NONE
              else SOME(SS.string(SS.triml 1 a)))
          end
    in {path=path, anchor=anchor}
    end
  fun parseRelative s = 
    let 
      val (path, anchor) =
        if SS.isEmpty s then (NONE, NONE)
        else 
          let val (p, a) = SS.splitl(neq #"#") s
          in (SOME (map SS.setring (SS.tokens (eq #"/") p)),
              if SS.isEmpty a then NONE
              else SOME(SS.string(SS.triml 1 a)))
          end
    in {path=path, anchor=anchor}
    end *)
  fun parse Url s = 
    let 
      val s = SS.all s 
      val (scheme, body) = SS.splitl(fn x => c <> #":") s
    in
      if SS.isEmpty body then
        RELATIVE (parseRelative scheme)
      else
        case lower (SS.string scheme) of
          "http" => HTTP (parseHttp body)
        | "file" => FILE (parseFile body)
        | _ => raise urlFormat
    end
end
