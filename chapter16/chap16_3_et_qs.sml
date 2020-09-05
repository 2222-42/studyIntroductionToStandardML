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
