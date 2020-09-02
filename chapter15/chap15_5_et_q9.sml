(* SML source file. Copyright (c) by 2222-42 2020.
* Chap15.5
*)

(* Q15.9 *)
local
    open TextIO
in
    fun filterStream f ins outs = 
        if endOfStream ins then ()
        else case input1 ins of 
                  SOME c => (output1(outs, f c); filterStream f ins outs)
                | NONE => filterStream f ins outs

    fun filterFile f inf outf = 
        let val ins = openIn inf handle IO.Io {name, function, cause} =>
                    (print ("IO Error : " ^ function ^ " failed. ");
                     case cause of
                            OS.SysErr (s,e) => print (s^": ")
                          | _ => print (exnMessage cause ^ ": ");
                     print (name ^ "\n");
                     openString ""
                    )
            val outs = openOut outf
        in 
            (filterStream f ins outs; closeIn ins; closeOut outs)
        end

    fun lowerFile inf outf = filterFile Char.toLower inf outf;
end
(* lowerFile "E:/SMLProject/studyIntroductionToStandardML/header_pattern.sml" "E:/SMLProject/studyIntroductionToStandardML/chapter15/header_pattern.sml" ;  *)
(* - lowerFile "E:/SMLProject/studyIntroductionToStandardML/header_patterns.sml" "E:/SMLProject/studyIntroductionToStandardML/chapter15/header_pattern.sml" ;

uncaught exception Io [Io: openIn failed on "E:/SMLProject/studyIntroductionToStandardML/header_patterns.sml", Win32TextPrimIO.openRd: failed]
  raised at: Basis/Implementation/IO/text-io-fn.sml:792.25-792.71 *)

(* 
- lowerFile "E:/SMLProject/studyIntroductionToStandardML/header_patterns.sml" "E:/SMLProject/studyIntroductionToStandardML/chapter15/header_pattern.sml"
= ;
IO Error : openIn failed. Win32TextPrimIO.openRd: failed: E:/SMLProject/studyIntroductionToStandardML/header_patterns.sml
val it = () : unit

これだとファイルが作られてしまうので、いまいち
*)
