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