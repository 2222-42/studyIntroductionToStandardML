(* SML source file. Copyright (c) by 2222-42 2020.
* Chap5.2 Q15.1 Q15.2 Q15.3
*)

(* 
signature TEXT_IO =
  sig
    type vector = string
    type elem = char
    type instream
    type outstream
    val input : instream -> vector
    val input1 : instream -> elem option       
    val inputN : instream * int -> vector      
    val inputAll : instream -> vector
    val canInput : instream * int -> int option
    val lookahead : instream -> elem option    
    val closeIn : instream -> unit
    val endOfStream : instream -> bool
    val output : outstream * vector -> unit
    val output1 : outstream * elem -> unit
    val flushOut : outstream -> unit
    val closeOut : outstream -> unit
    structure StreamIO :
      sig
        type vector = string
        type elem = char
        type reader
        type writer
        type instream
        type outstream
        type pos
        type out_pos
        val input : instream -> vector * instream
        val input1 : instream -> (elem * instream) option
        val inputN : instream * int -> vector * instream
        val inputAll : instream -> vector * instream
        val canInput : instream * int -> int option
        val closeIn : instream -> unit
        val endOfStream : instream -> bool
        val mkInstream : reader * vector -> instream
        val getReader : instream -> reader * vector
        val filePosIn : instream -> pos
        val output : outstream * vector -> unit
        val output1 : outstream * elem -> unit
        val flushOut : outstream -> unit
        val closeOut : outstream -> unit
        val setBufferMode : outstream * IO.buffer_mode -> unit
        val getBufferMode : outstream -> IO.buffer_mode
        val mkOutstream : writer * IO.buffer_mode -> outstream
        val getWriter : outstream -> writer * IO.buffer_mode
        val getPosOut : outstream -> out_pos
        val setPosOut : out_pos -> unit
        val filePosOut : out_pos -> pos
        val inputLine : instream -> (string * instream) option
        val outputSubstr : outstream * substring -> unit
      end
    val mkInstream : StreamIO.instream -> instream
    val getInstream : instream -> StreamIO.instream
    val setInstream : instream * StreamIO.instream -> unit
    val getPosOut : outstream -> StreamIO.out_pos
    val setPosOut : outstream * StreamIO.out_pos -> unit
    val mkOutstream : StreamIO.outstream -> outstream
    val getOutstream : outstream -> StreamIO.outstream
    val setOutstream : outstream * StreamIO.outstream -> unit
    val inputLine : instream -> string option
    val outputSubstr : outstream * substring -> unit
    val openIn : string -> instream
    val openString : string -> instream
    val openOut : string -> outstream
    val openAppend : string -> outstream
    val stdIn : instream
    val stdOut : outstream
    val stdErr : outstream
    val print : string -> unit
    val
        scanStream : ((elem,StreamIO.instream) StringCvt.reader
                       -> ('a,StreamIO.instream) StringCvt.reader)
                      -> instream -> 'a option
  end
*)

open TextIO
fun copyStream ins outs = 
    if endOfStream ins then ()
    else case input1 ins of 
                SOME c => (output1(outs,c);
                           copyStream ins outs)
              | NONE => copyStream ins outs
fun copyFile inf outf = 
    let val ins = openIn inf
        val outs = openOut outf
    in (copyStream ins outs;
        closeIn ins; closeOut outs)
    end;

(* copyFile "E:/SMLProject/studyIntroductionToStandardML/chapter15/test.txt" "E:/SMLProject/studyIntroductionToStandardML/chapter15/copy.txt";  *)

(* Q15.1 *)
fun cat infList outf =
    let
        val outs = openOut outf
        fun copySub outs inf =
            let val ins = openIn inf
            in (copyStream ins outs; closeIn ins)
            end
    in
        (map (copySub outs) infList; closeOut outs)
    end;

(* 筆者の解答:まとめてオープンして、それを出力先にcopyStreamして、まとめてcloseする方針。
回答者の回答は1つずつ実施している。
*)
fun catByAuthor L out =
    let
        val sources = map openIn L
        val sink = openOut out
    in
        (foldl (fn (x,_) => copyStream x sink) () sources;
        map closeIn sources;
        closeOut sink)
    end

val fileList = ["E:/SMLProject/studyIntroductionToStandardML/chapter15/test1.txt", "E:/SMLProject/studyIntroductionToStandardML/chapter15/test2.txt", "E:/SMLProject/studyIntroductionToStandardML/chapter15/test3.txt"] 
(* 
cat fileList "E:/SMLProject/studyIntroductionToStandardML/chapter15/newCopy1.txt" 
catByAuthor fileList "E:/SMLProject/studyIntroductionToStandardML/chapter15/newCopy1.txt" 
*)

(* Q15.2 *)
fun wc inf =
    let val ins = openIn inf
        val countOfLine = ref 0
        val countOfString = ref 0
        fun getLine ins = 
                case inputLine(ins) of
                        SOME v => (countOfLine := !countOfLine + 1; 
                                   countOfString := !countOfString + size v;
                                   getLine ins)
                      | NONE => ()
    in (
            getLine ins;
            print ("countOfLine is: "^Int.toString(!countOfLine)^".\n");
            print ("countOfString is: "^Int.toString(!countOfString)^".\n");
            closeIn ins
    )
    end;
(* これだと、改行文字も文字数に含めてしまう。
-> あくまでもchar型に含まれるから、数えていいか。
*)

(* wc "E:/SMLProject/studyIntroductionToStandardML/chapter15/newCopy.txt";  *)

   local
     open TextIO
   in
     fun wcByAuthor file =
        let
          val ins = openIn file
          fun count (l,c) =
             if endOfStream ins then (l,c)
             else case input1 ins of
                SOME #"\n" => count (l+1, c+1)
              | SOME _ => count (l, c+1)
              | NONE => (l,c)
          val (l,c) = count (0,0)
          val _ = print (Int.toString l ^ " ")
          val _ = print (Int.toString c ^ "\n")
          val _ = closeIn ins
        in
          ()
        end
   end;
(* 筆者の解答だと、行数が1つすくなく数えられてしまう。改行文字が見つからないから。
-> 回答者の回答も、改行が2回以上続くと、行数を1つ少なく数えてしまう。
*)
wcByAuthor "E:/SMLProject/studyIntroductionToStandardML/chapter15/1letter.txt";

local
    open TextIO
in
    fun wcModifiedWithLine file =
    let
        val ins = openIn file
        fun count (l,c) =
            if endOfStream ins then (l,c)
            else case inputLine(ins) of
                    SOME v => count(l+1, c+(size v))
                    | NONE => (l,c)
        val (l,c) = count (0,0)
        val _ = print (Int.toString l ^ " ")
        val _ = print (Int.toString c ^ "\n")
        val _ = closeIn ins
    in
        ()
    end
end;
wcModifiedWithLine "E:/SMLProject/studyIntroductionToStandardML/chapter15/1letter.txt";

(* local
    open TextIO
in
    fun wcModifiedWithInput1 file =
    let
        val ins = openIn file
        fun count (l,c) =
            if endOfStream ins then (l,c)
            else case input1 ins of
                SOME #"\n" => count (l+1, c+1)
                | SOME #"\f" => count (l+1, c+1)
                | SOME _ => count (l, c+1)
                | NONE => (l,c)
        val (l,c) = count (0,0)
        val _ = print (Int.toString l ^ " ")
        val _ = print (Int.toString c ^ "\n")
        val _ = closeIn ins
    in
        ()
    end
end;
wcModifiedWithInput1 "E:/SMLProject/studyIntroductionToStandardML/chapter15/1letter.txt"; *)

fun filterStream f ins outs = 
    if endOfStream ins then ()
    else case input1 ins of 
                SOME c => (output1(outs, f c); filterStream f ins outs)
              | NONE => filterStream f ins outs

fun filterFile f inf outf = 
    let val ins = openIn inf
        val outs = openOut outf
    in (filterStream f ins outs; closeIn ins; closeOut outs)
    end

(* Q15.3 *)
fun isUpper c = #"A" <= c andalso c <= #"Z"

fun toLower c =
    if isUpper c
    then chr (ord #"a" + (ord c - ord #"A"))
    else c;

fun lowerFile inf outf = filterFile toLower inf outf;

(* lowerFile "E:/SMLProject/studyIntroductionToStandardML/header_pattern.sml" "E:/SMLProject/studyIntroductionToStandardML/chapter15/header_pattern.sml" ;  *)

(* Q15.4 *)

fun echo() = (
    print("?");
    let 
        val inputData = inputLine stdIn
    in
        if isSome inputData then (print(valOf inputData)) else ()
    end;
    echo()
)
(* stdIn:1.2-1.8 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...) *)
