(* SML source file. Copyright (c) by 2222-42 2020.
* Q16.9
*)

signature FORMAT =
sig datatype kind = 
                INT of StringCvt.radix
              | REAL of StringCvt.realfmt
              | STRING
              | BOOL
    datatype align = LEFT | RIGHT
    datatype format = 
                LITERAL of string
              | SPEC of {kind: kind, width: int option, align: align}
    datatype argument = 
                I of int
              | R of real
              | S of string
              | B of bool
    exception formatError
    val format : string -> argument list -> string
    val printf : string -> argument list -> unit
end

structure Format : FORMAT =
struct
  datatype kind = 
          INT of StringCvt.radix
        | REAL of StringCvt.realfmt
        | STRING
        | BOOL
  datatype align = LEFT | RIGHT
  datatype format = 
          LITERAL of string
        | SPEC of {kind: kind, width: int option, align: align}
  datatype argument = 
          I of int
        | R of real
        | S of string
        | B of bool
  exception formatError
  structure SS = Substring

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

  fun printf s L = print (format s L)
end;

(* Format.printf "%10s\n" [Format.S "first"];

fun printTriple (str1, str2, str3) listOfTupleOfInt =
    let
      fun loop L = 
          case L of nil => ()
                | ((i1,i2,i3)::t) => 
                    (Format.printf "%10d%10d%10d\n" [Format.I i1,Format.I i2,Format.I i3];
                    loop t)
    in
      (Format.printf "%10s%10s%10s\n" [Format.S str1, Format.S str2, Format.S str2]);
      loop listOfTupleOfInt
    end *)
