  structure ShowTime =
   struct
   local
     structure S = Substring
   in
     exception FormatError
     (* 回答者コメント:  `padLeft`で埋めるために使うべきCharは、問題文より、`#" "`ではなく`#"0"`である　*)
     fun dH t = StringCvt.padLeft #" " 2 (Int.toString (Date.hour (Date.fromTimeLocal t)))
     fun dI t =
         let val h = Date.hour (Date.fromTimeLocal t)
             val i = if h = 0 then 12 else if h > 12 then h - 12 else h
         in StringCvt.padLeft #" " 2 (Int.toString i)
         end
     fun dk t = Int.toString (Date.hour (Date.fromTimeLocal t))
     fun dM t = Int.toString (Date.minute (Date.fromTimeLocal t))
     fun dS t = Int.toString (Date.second (Date.fromTimeLocal t))
     (* 回答者コメント: 問題文の設定を見ると、`#"0"`で埋めておくようにする方が正しい *)
     fun dd t = Int.toString (Date.day (Date.fromTimeLocal t))
     fun dm t = case Date.month (Date.fromTimeLocal t) of
                  Date.Jan => "01"
                | Date.Feb => "02"
                | Date.Mar => "03"
                | Date.Apr => "04"
                | Date.May => "05"
                | Date.Jun => "06"
                | Date.Jul => "07"
                | Date.Aug => "08"
                | Date.Sep => "09"
                | Date.Oct => "10"
                | Date.Nov => "11"
                | Date.Dec => "12"
     fun dY t = Int.toString (Date.year (Date.fromTimeLocal t))
     datatype spec = EMBED of Time.time -> string | LITERAL of string
     (* 回答者のコメント: oneFormatで関数を返すようにしている。
                         これによって、formatDataの機能を統合させている *)
     fun oneFormat s =
         let val s = S.triml 1 s
         in if S.isPrefix "%" s then (LITERAL "%",S.triml 1 s)
            else
              let
                val (c,s) = case S.getc s of NONE => raise FormatError
                                           | SOME s  => s
              in (EMBED (case c of
                          #"H" => dH
                        | #"I" => dI
                        | #"k" => dk
                        | #"M" => dM
                        | #"S" => dS
                        | #"d" => dd
                        | #"m" => dm
                        | #"Y" => dY
                        | _ => raise FormatError),
                  s)
              end
         end
     fun parse s =
         let
           val (s1,s) = StringCvt.splitl (fn c => c <> #"%") S.getc s
           val prefix = if s1 = "" then nil
                        else [LITERAL s1]
         in if S.isEmpty s then prefix
            else let val (f,s) = oneFormat s
                     val L = parse s
                 in prefix@(f::L)
                 end
         end
     fun format s tm =
         let val FL = parse (S.full s)
             fun splice (h::t) =
                 (case h of
                    LITERAL s => s ^ (splice t)
                  | EMBED f => f tm ^ (splice t))
               | splice nil = ""
         in
           splice FL
         end
     fun showTime s =
         print (format s (Time.now()))
   end
   end