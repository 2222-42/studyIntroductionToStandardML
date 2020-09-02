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

(* 筆者の解答 *)
   fun getNumByAuthor ins =
     let
       fun getRest s =
           case T.lookahead ins of
             NONE => s
           | SOME c =>
             if Char.isDigit c then
               getRest (s ^ T.inputN(ins,1))
             else s
     in
       DIGITS (getRest "")
     end
(* end of Q15.5 *)

(* Q15.7 *)
fun lex ins = 
  (skipSpaces ins;
   if T.endOfStream ins then EOF
   else 
    let val c = valOf (T.lookahead ins)
    in
      if Char.isDigit c then getNum ins
      else if Char.isAlpha c then getID ins
      else case valOf (T.input1 ins) of
        #"!" => BANG
      | #"\"" => DOUBLEQUOTE
      | #"#" => HASH
      | #"$" => DOLLAR
      | #"%" => PERCENT
      | #"&" => AMPERSAND
      | #"'" => QUOTE
      | #"(" => LPAREN
      | #")" => RPAREN
      | #"~" => TILDE
      | #"=" => EQUALSYM
      | #"-" => HYPHEN
      | #"^" => HAT
      | #"_" => UNDERBAR
      | #"\\" => SLASH
      | #"|" => BAR
      | #"@" => AT
      | #"`" => BACKQUOTE
      | #"[" => LBRACKET
      | #"{" => LBRACE
      | #";" => SEMICOLON
      | #"+" => PLUS
      | #":" => COLON
      | #"*" => ASTERISK
      | #"]" => RBRACKET
      | #"}" => RBRACE
      | #"," => COMMA
      | #"<" => LANGLE
      | #"." => PERIOD
      | #">" => RANGLE
      | #"/" => BACKSLASH
      | #"?" => QUESTION
      | _ => SPECIAL c
    end)

(* Q15.6 *)
fun testLex() = 
  let 
    val token = lex TextIO.stdIn
    fun toString t = 
      case t of
        ID(s) => "ID("^s^")"
      | DIGITS(s) => "DIGITS("^s^")"
      | BANG => "BANG"
      | DOUBLEQUOTE => "DOUBLEQUOTE"
      | HASH => "HASH"
      | DOLLAR => "DOLLAR"
      | PERCENT => "PERCENT"
      | AMPERSAND => "AMPERSAND"
      | QUOTE => "QUOTE"
      | LPAREN => "LPAREN"
      | RPAREN => "RPAREN"
      | TILDE => "TILDE"
      | EQUALSYM => "EQUALSYM"
      | HYPHEN => "HYPHEN"
      | HAT => "HAT"
      | UNDERBAR => "UNDERBAR"
      | SLASH => "SLASH"
      | BAR => "BAR"
      | AT => "AT"
      | BACKQUOTE => "BACKQUOTE"
      | LBRACKET => "LBRACKET"
      | LBRACE => "LBRACE"
      | SEMICOLON => "SEMICOLON"
      | PLUS => "PLUS"
      | COLON => "COLON"
      | ASTERISK => "ASTERISK"
      | RBRACKET => "RBRACKET"
      | RBRACE => "RBRACE"
      | COMMA => "COMMA"
      | LANGLE => "LANGLE"
      | PERIOD => "PERIOD"
      | RANGLE => "RANGLE"
      | BACKSLASH => "BACKSLASH"
      | QUESTION => "QUESTION"
      | SPECIAL c => "SPECIAL("^str(c)^")"
  in case token of 
        EOF => ()
      | _ => (print (toString token ^ "\n");testLex())
  end

(* 筆者の解答ではChar.toStringを使っている
         | SPECIAL c => "SPECIAL" ^ Char.toString c ^ ")"
本質的な違いはなし
*)
(* end Q15.6 *)

(* Q15.7 *)
(* 
use temp.txt
ID(use)
ID(temp)
PERIOD
ID(txt)

方針:
> use fileName
とある。
読み込む
`use`がまず入る。
skipSpaceする。
`fileName`が入る。
  ファイル名の形式は？
  -> token だとダメ。
  -> ファイル名には空白が含まれることがあるから、ダブルクォーテーションとかでくくっている必要がある
    -> テキストでその指定はない。
    ・文の末尾まで見るか -> とりあえずこれで。
      ・改行記号を含めてはならない 
    ・ダブルクォーテーションを要求するか
  cf: http://web.mit.edu/~firebird/arch/sun4x_59/bin/sml/cm/entity/description.sml
そのファイル名のファイルを開く。
そのファイルの中身をプリントする。
閉じる。
後を続ける。
*)


fun toString t = 
  case t of
    ID(s) => "ID("^s^")"
  | DIGITS(s) => "DIGITS("^s^")"
  | BANG => "BANG"
  | DOUBLEQUOTE => "DOUBLEQUOTE"
  | HASH => "HASH"
  | DOLLAR => "DOLLAR"
  | PERCENT => "PERCENT"
  | AMPERSAND => "AMPERSAND"
  | QUOTE => "QUOTE"
  | LPAREN => "LPAREN"
  | RPAREN => "RPAREN"
  | TILDE => "TILDE"
  | EQUALSYM => "EQUALSYM"
  | HYPHEN => "HYPHEN"
  | HAT => "HAT"
  | UNDERBAR => "UNDERBAR"
  | SLASH => "SLASH"
  | BAR => "BAR"
  | AT => "AT"
  | BACKQUOTE => "BACKQUOTE"
  | LBRACKET => "LBRACKET"
  | LBRACE => "LBRACE"
  | SEMICOLON => "SEMICOLON"
  | PLUS => "PLUS"
  | COLON => "COLON"
  | ASTERISK => "ASTERISK"
  | RBRACKET => "RBRACKET"
  | RBRACE => "RBRACE"
  | COMMA => "COMMA"
  | LANGLE => "LANGLE"
  | PERIOD => "PERIOD"
  | RANGLE => "RANGLE"
  | BACKSLASH => "BACKSLASH"
  | QUESTION => "QUESTION"
  | SPECIAL c => "SPECIAL("^str(c)^")";

fun testSub ins =
    let
      val token = lex ins
      fun getFileName ins string = 
            case T.input1 ins of
                SOME #"\n" => string
              | SOME s => getFileName ins (string^str(s))
              | NONE => string
          (* case T.inputLine ins of
            SOME string => string
          | NONE => "" *)
    in
      case token of
          EOF => ()
        | ID "use" =>
            let
              val fileName = (skipSpaces ins; getFileName ins "")
              val newIns = T.openIn fileName
            in
              (testSub newIns; T.closeIn newIns; testSub ins)
            end
        | _ => (print (toString token ^ "\n");
                testSub ins)
    end
fun testLexWithUse () = testSub T.stdIn;

(* testLexWithUse (); *)

(* E:/SMLProject/studyIntroductionToStandardML/chapter15/Q15_8_testfile/temp.txt *)
(* E:/SMLProject/studyIntroductionToStandardML/chapter15/Q15_8_testfile/nest.txt *)
