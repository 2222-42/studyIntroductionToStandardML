(* SML source file. Copyright (c) by 2222-42 2020.
* Chap16.2
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

よって、substring -> (char * substring) optionは、 (char, substring) readerである。

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
    end

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

(* 筆者の解答:
回答者の解答と大きな違いはない *)
fun readIntByAuthor string =
  let
    val stream = ref (Substring.full string)
  in
    fn () =>
      case intScan (!stream) of
        SOME (i, substring) =>
        SOME i before stream := substring
      | NONE => NONE
  end

(* --end of Q16.2 *)

(* Q16.3 *)

val realScan = Real.scan Substring.getc;
fun readReal str = 
    let 
        val s = ref (Substring.full str)
    in
        fn () => case (realScan (!s)) of
            SOME (i, r) => (s := r ;SOME i)
          | NONE => NONE
    end
val rf = readReal "1.23 3E10  1 abc";
val rg = readReal "1.23 abc 1 4.56";

val boolScan = Bool.scan Substring.getc;
fun readBool str = 
    let 
        val s = ref (Substring.full str)
    in
        fn () => case (boolScan (!s)) of
            SOME (i, r) => (s := r ;SOME i)
          | NONE => NONE
    end
val bf = readBool "true false 1 abc";
val bg = readBool "false true abc 4.56";

(* 筆者の解答:
これら関数はすべて同型であるため、以下の例では、 高階の関数を定義し、それを各型のscan関数に適用している
*)

   fun makeRead scan string =
     let
       val reader = scan Substring.getc
       val stream = ref (Substring.full string)
     in
       fn () =>
         case reader (!stream) of
           SOME (i, substring) =>
           SOME i before stream := substring
         | NONE => NONE
    end
   val readRealByAuthor = makeRead Real.scan
   val readBoolByAuthor = makeRead Bool.scan

(* --end of Q16.3 *)

(* Q16.4 *)

fun genericReadInt reader inputData = 
    let 
        val s = ref inputData
        val modifiedReader = Int.scan StringCvt.DEC reader
    in
        fn () => case (modifiedReader (!s)) of
            SOME (i, r) => (s := r ;SOME i)
          | NONE => NONE
    end

(* - genericReadInt;
val it = fn : (char,'a) StringCvt.reader -> 'a -> unit -> int option *)

(* 筆者の解答: 同じなので省略 *)

(* --end of Q16.4 *)

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
  end
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
  end

(* readIntFromFile "E:/SMLProject/studyIntroductionToStandardML/chapter16/testChar.txt"; *)

(* --end Q16.5 *)

signature PARSE_URL = sig
  exception urlFormat
  datatype url = 
      HTTP of {host: string list, path: string list option, anchor: string option}
    | FILE of {path: string list, anchor: string option}
    | RELATIVE of {path: string list, anchor: string option}
  val parseUrl : string -> url
end

fun isUpper c = #"A" <= c andalso c <= #"Z"

fun toLower c =
    if isUpper c
    then chr (ord #"a" + (ord c - ord #"A"))
    else c;
fun lower s = implode (map toLower (explode s))

(* Q16.6 *)
structure Url:PARSE_URL = struct
  structure SS = Substring
  exception urlFormat
  datatype url =       
      HTTP of {host: string list, path: string list option, anchor: string option}
    | FILE of {path: string list, anchor: string option}
    | RELATIVE of {path: string list, anchor: string option}
  fun neq c x = not (x = c)
  fun eq c x = c = x
  fun parseHttp s = 
    let
      val s = if SS.isPrefix "://"  s then
                SS.triml 3 s
              else raise urlFormat
      val (host, body) = SS.splitl (neq #"/") s
      val domain = map SS.string (SS.tokens (eq #".") host)
      val (path, anchor) =
        if SS.isEmpty body then (NONE, NONE)
        else 
          let val (p, a) = SS.splitl(neq #"#") body
          in (SOME (map SS.string (SS.tokens (eq #"/") p)),
              if SS.isEmpty a then NONE
              else SOME(SS.string(SS.triml 1 a)))
          end
    in {host=domain, path=path, anchor=anchor}
    end
  fun parseFile s = 
    let 
      val s = if SS.isPrefix ":/"  s then
          SS.triml 2 s
        else raise urlFormat
      val (path, anchor) =
        if SS.isEmpty s then ([""], NONE)
        else 
          let val (p, a) = SS.splitl(neq #"#") s
          in ((map SS.string (SS.tokens (eq #"/") p)),
              if SS.isEmpty a then NONE
              else SOME(SS.string(SS.triml 1 a)))
          end
    in {path=path, anchor=anchor}
    end
  fun parseRelative s = 
    let 
      val (path, anchor) =
        if SS.isEmpty s then ([""], NONE)
        else 
          let val (p, a) = SS.splitl(neq #"#") s
          in ((map SS.string (SS.tokens (eq #"/") p)),
              if SS.isEmpty a then NONE
              else SOME(SS.string(SS.triml 1 a)))
          end
    in {path=path, anchor=anchor}
    end
  fun parseUrl s = 
    let 
      val s = SS.full s 
      val (scheme, body) = SS.splitl(fn c => c <> #":") s
    in
      if SS.isEmpty body then
        RELATIVE (parseRelative scheme)
      else
        case lower (SS.string scheme) of
          "http" => HTTP (parseHttp body)
        | "file" => FILE (parseFile body)
        | _ => raise urlFormat
    end
end;

Url.parseUrl "http://www.google.com/user/#1/show"; 
(* 
val it =
  HTTP {anchor=SOME "1/show",host=["www","google","com"],path=SOME ["user"]}
  : Url.url 

- Url.parseUrl "http://www.google.com/search";
val it = HTTP {anchor=NONE,host=["www","google","com"],path=SOME ["search"]}
  : Url.url
- Url.parseUrl "http://www.google.com/user/1/show";
val it =
  HTTP {anchor=NONE,host=["www","google","com"],path=SOME ["user","1","show"]}
  : Url.url
*)

Url.parseUrl "file://E:/SMLProject/studyIntroductionToStandardML";
(* 
val it =
  FILE {anchor=NONE,path=["E:","SMLProject","studyIntroductionToStandardML"]}
  : Url.url
*)

Url.parseUrl "chapter16/chap16_2_et_qs.sml";
(* 
val it = RELATIVE {anchor=NONE,path=["chapter16","chap16_2_et_qs.sml"]}
  : Url.url
*)
