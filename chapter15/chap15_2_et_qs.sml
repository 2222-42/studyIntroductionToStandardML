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

val fileList = ["E:/SMLProject/studyIntroductionToStandardML/chapter15/test.txt", "E:/SMLProject/studyIntroductionToStandardML/chapter15/test2.txt", "E:/SMLProject/studyIntroductionToStandardML/chapter15/test3.txt"] 
(* cat fileList "E:/SMLProject/studyIntroductionToStandardML/chapter15/newCopy.txt" *)

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

(* wc "E:/SMLProject/studyIntroductionToStandardML/header_pattern.sml";  *)

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
