(* SML source file. Copyright (c) by 2222-42 2020.
* Chap15.4 Q.15.5
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

datatype token = 
    EOF                 | ID of string
  | DIGITS of string    | SPECIAL of char
  | BANG   (* !*)       | DOUBLEQUOTE
  | HASH                | DOLLAR
  | PERCENT             | AMPERSAND
  | QUOTE               | LPAREN
  | RPAREN              | TILDE
  | EQUALSYM            | HYPHEN
  | HAT                 | UNDERBAR
  | SLASH               | BAR
  | AT                  | BACKQUOTE
  | LBRACKET            | LBRACE
  | SEMICOLON           | PLUS
  | COLON               | ASTERISK
  | RBRACKET            | RBRACE
  | COMMA               | LANGLE
  | PERIOD              | RANGLE
  | BACKSLASH           | QUESTION
;

structure T = TextIO
fun skipSpaces ins = 
    case T.lookahead ins of
        SOME c => if Char.isSpace c
                  then (T.input1 ins; skipSpaces ins)
                  else ()
      | _ => ()

fun getID ins = 
    let fun getRest s = 
            case T.lookahead ins of
                SOME c => if Char.isAlphaNum c then
                                getRest (s ^ T.inputN(ins, 1))
                          else s
              | _ => s
    in ID(getRest "")
    end

(* Q.15.5 *)
fun getNum ins = 
  let
    fun getRest s =
          case T.lookahead ins of
          SOME c => if Char.isDigit c then
                          getRest (s ^ T.inputN(ins, 1))
                    else s
        | _ => s
  in DIGITS(getRest "")
  end
