   signature LEX = sig
     datatype token
       =  EOF                         | ID of string
       | DIGITS of string             | SPECIAL of char
       | BANG            (* ! *)      | DOUBLEQUOTE     (* " *)
       | HASH            (* # *)      | DOLLAR          (* $ *)
       | PERCENT         (* % *)      | AMPERSAND       (* & *)
       | QUOTE           (* ’ *)      | LPAREN          (* ( *)
       | RPAREN          (* ) *)      | TILDE           (* ~ *)
       | EQUALSYM        (* = *)      | HYPHEN          (* - *)
       | HAT             (* ^ *)      | UNDERBAR        (* _ *)
       | SLASH           (* \ *)      | BAR             (* | *)
       | AT              (* @ *)      | BACKQUOTE       (* ‘ *)
       | LBRACKET        (* [ *)      | LBRACE          (* { *)
       | SEMICOLON       (* ; *)      | PLUS            (* + *)
       | COLON           (* : *)      | ASTERISK        (* * *)
       | RBRACKET        (* ] *)      | RBRACE          (* } *)
       | COMMA           (* , *)      | LANGLE          (* < *)
       | PERIOD          (* . *)      | RANGLE          (* > *)
       | BACKSLASH       (* / *)      | QUESTION        (* ? *)
     val lex : TextIO.instream -> token
     val toString : token -> string
     val testLex : unit -> unit
   end

   structure Lex : LEX =
   struct
     structure T = TextIO
         datatype token
           = EOF                          | ID of string
           | DIGITS of string             | SPECIAL of char
           | BANG            (* ! *)      | DOUBLEQUOTE     (* " *)
           | HASH            (* # *)      | DOLLAR          (* $ *)
           | PERCENT         (* % *)      | AMPERSAND       (* & *)
           | QUOTE           (* ’ *)      | LPAREN          (* ( *)
           | RPAREN          (* ) *)      | TILDE           (* ~ *)
           | EQUALSYM        (* = *)      | HYPHEN          (* - *)
           | HAT             (* ^ *)      | UNDERBAR        (* _ *)
           | SLASH           (* \ *)      | BAR             (* | *)
           | AT              (* @ *)      | BACKQUOTE       (* ‘ *)
           | LBRACKET        (* [ *)      | LBRACE          (* { *)
           | SEMICOLON       (* ; *)      | PLUS            (* + *)
           | COLON           (* : *)      | ASTERISK        (* * *)
           | RBRACKET        (* ] *)      | RBRACE          (* } *)
           | COMMA           (* , *)      | LANGLE          (* < *)
           | PERIOD          (* . *)      | RANGLE          (* > *)
           | BACKSLASH       (* / *)      | QUESTION        (* ? *)
     fun skipSpaces ins =
         case T.lookahead ins of
           SOME c => if Char.isSpace c
                     then (T.input1 ins;skipSpaces ins)
                     else ()
         | _ => ()
     fun getID ins =
         let fun getRest s =
             case T.lookahead ins of
               SOME c => if Char.isAlphaNum c then
                            getRest (s ^ T.inputN(ins,1))
                         else s
             | _ => s
         in ID(getRest "")
         end
     fun getNum ins =
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
     fun lex ins =
         (skipSpaces ins;
          if T.endOfStream ins then EOF
          else
           let
             val c = valOf (T.lookahead ins)
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
             | #"`" =>  BACKQUOTE
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

     fun toString tok =
         case tok of
         EOF => "EOF"
           | ID s => "ID(" ^ s ^ ")"
           | DIGITS s => "DIGITS(" ^ s ^ ")"
           | SPECIAL c => "SPECIAL" ^ Char.toString c ^ ")"
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
     (* fun testLex () =
         let
           val token = lex (TextIO.stdIn)
         in
           case token of
             EOF => ()
           | _ => (print (toString token ^ "\n");
                   testLex ())
         end *)
      fun getFileName ins string = 
          case T.input1 ins of
              SOME #"\n" => string
            | SOME s => getFileName ins (string^str(s))
            | NONE => string
      fun testMain ins =
        let
          val token = lex ins
        in
          case token of
             EOF => ()
           | ID "use" =>
               let
                 val fileName = (skipSpaces ins; getFileName ins "")
                 val newIns = TextIO.openIn fileName
               in
                 (print ("[opening file \""^fileName^"\"]\n");
                   testMain newIns;
                   TextIO.closeIn newIns;
                   print ("[closing file \""^fileName^"\"]\n");
                    testMain ins)
               end
            | _ => (print (toString token ^ "\n");
                    testMain ins)
        end
    fun testLex () = testMain TextIO.stdIn
   end

Lex.testLex();
