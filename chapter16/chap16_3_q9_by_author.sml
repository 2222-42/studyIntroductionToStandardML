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
     exception formatError
     structure S = Substring
     datatype kind =  INT of StringCvt.radix
                    | REAL of StringCvt.realfmt
                    | STRING
                    | BOOL
     datatype align = LEFT | RIGHT
     datatype format =
              LITERAL of string
            | SPEC of {kind:kind,width:int option,align:align}
     datatype argument = I of int
                       | R of real
                       | S of string
                       | B of bool
     fun formatData {kind,width,align} data=
         let val body =
                 case (kind,data) of
                   (INT radix,I i) => Int.fmt radix i
                 | (REAL fmt,R r) => Real.fmt fmt r
                 | (STRING,S s) => s
                 | (BOOL,B b) => Bool.toString b
                 | _ => raise formatError
         in case width of
              NONE => body
            | SOME w => (case align of
                           LEFT => StringCvt.padRight #" " w body
                         | RIGHT => StringCvt.padLeft #" " w body)
         end
     fun split s = StringCvt.splitl (fn c => c <> #"%") S.getc s
     fun scanInt s =
         let val r= Int.scan StringCvt.DEC S.getc s
         in case r of NONE => (NONE,s)
                    | SOME(n,s) => (SOME n,s)
         end
     fun oneFormat s =
         let val s = S.triml 1 s
         in if S.isPrefix "%" s then (LITERAL "%",S.triml 1 s)
            else
              let val (a,s) = if S.isPrefix "-" s
                              then (LEFT,S.triml 1 s)
                              else (RIGHT,s)
                  val (w,s) = scanInt s
                  val (c,s) = case S.getc s of NONE => raise formatError
                                             | SOME s  => s
              in (SPEC {width=w,align=a,
                        kind=case c of
                               #"d" => INT StringCvt.DEC
                             | #"s" => STRING
                             | #"f" => REAL (StringCvt.FIX NONE)
                             | #"e" => REAL (StringCvt.SCI NONE)
                             | #"g" => REAL (StringCvt.GEN NONE)
                             | _ => raise formatError},
                  s)
              end
         end
     fun parse s =
         let
           val (s1,s) = split s
           val prefix = if s1 = "" then nil
                        else [LITERAL s1]
         in if S.isEmpty s then prefix
            else let val (f,s) = oneFormat s
                     val L = parse s
                 in prefix@(f::L)
                 end
         end
     fun format s L =
         let val FL = parse (S.full s)
             fun splice (h::t) L =
                 (case h of
                    LITERAL s => s ^ (splice t L)
                  | SPEC s => (formatData s (List.hd L) ^ (splice t (List.tl L))))
               | splice nil l = ""
         in
           (splice FL L)
         end
     fun printf s L = print (format s L)
   end