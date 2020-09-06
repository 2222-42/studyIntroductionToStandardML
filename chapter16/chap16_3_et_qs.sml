(* SML source file. Copyright (c) by 2222-42 2020.
* Chap16.3
*)

datatype kind = INT of StringCvt.radix
              | REAL of StringCvt.realfmt
              | STRING
              | BOOL

datatype align = LEFT | RIGHT

type formatSpec = {kind: kind, width: int option, align: align}

datatype argument = 
    I of int
  | R of real
  | S of string
  | B of bool

exception formatError

fun formatData {kind, width, align} data =
    let
      val body = 
        case (kind, data) of
           (INT radix, I i) => Int.fmt radix i
         | (REAL fmt, R r) => Real.fmt fmt r
         | (STRING, S s) => s
         | (BOOL, B b) => Bool.toString b
         | _ => raise formatError
    in
      case width of 
        NONE => body
      | SOME w => (case align of
                        LEFT => StringCvt.padRight #" " w body
                      | RIGHT => StringCvt.padLeft #" " w body)
    end

fun printTriple (str1, str2, str3) listOfTupleOfInt =
    let
      val DefaultWidth = 10
      val top = formatData {kind=STRING, width=SOME DefaultWidth, align=RIGHT} (S str1)
                ^ formatData {kind=STRING, width=SOME DefaultWidth, align=RIGHT} (S str2)
                ^ formatData {kind=STRING, width=SOME DefaultWidth, align=RIGHT} (S str3)
                ^ "\n"
      fun loop L = 
          case L of nil => ()
                | ((i1,i2,i3)::t) => 
                    (print 
                        (formatData {kind=INT StringCvt.DEC, width=SOME DefaultWidth, align=RIGHT} (I i1)
                         ^ formatData {kind=INT StringCvt.DEC, width=SOME DefaultWidth, align=RIGHT} (I i2)
                         ^ formatData {kind=INT StringCvt.DEC, width=SOME DefaultWidth, align=RIGHT} (I i3)
                         ^ "\n");
                    loop t)
    in
      print top;
      loop listOfTupleOfInt
    end;

printTriple ("first", "second", "third") [(1,2,3), (4,5,6)];

datatype format = SPEC of formatSpec | LITERAL of string

structure SS = Substring

(* Q16.8 *)
fun decScan x = Int.scan StringCvt.DEC x;
val intScan = decScan Substring.getc;
fun scanInt ss = 
  case intScan ss of
     NONE => (NONE, ss)
   | SOME(n, sss) => (SOME n, sss);

fun oneFormat s =
  let
    val s = SS.triml 1 s
  in
    if SS.isPrefix "%" s then (LITERAL "%", SS.triml 1 s)
    else
      let
        val (a, s) = if SS.isPrefix "-" s
                      then (LEFT, SS.triml 1 s)
                     else (RIGHT, s)
        val (w, s) = scanInt s
        val (c, s) = case SS.getc s of NONE => raise formatError
                                     | SOME s => s
      in
        (SPEC{width=w, align=a,
          kind=case c of
             #"d" => INT StringCvt.DEC
           | #"s" => STRING
           | #"f" => REAL (StringCvt.FIX NONE)
           | #"e" => REAL (StringCvt.SCI NONE)
           | #"g" => REAL (StringCvt.GEN NONE)
           | _ => raise formatError
        }, s)
      end

  end

fun parse s =
  let 
    val (s1, s) = StringCvt.splitl (fn c => c <> #"%") SS.getc s
    val prefix = if s1 = "" then nil else [LITERAL s1]
  in
    if SS.isEmpty s then prefix
    else let
      val (f, s) = oneFormat s
      val L = parse s
    in
      prefix@(f::L)
    end
    
  end

fun format s L = 
  let
    val FL = parse (SS.full s)
    fun traverse (h::t) L =
      (case h of
         LITERAL s => s ^ traverse t L
       | SPEC fmt => (formatData fmt (List.hd L)
                      ^ (traverse t (List.tl L))))
    | traverse nil l = ""
  in
    (traverse FL L)
  end
