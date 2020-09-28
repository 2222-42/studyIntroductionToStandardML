(* SML source file. Copyright (c) by 2222-42 2020.
* Chap18.4
*)

  signature LEX = sig
    type instream
    type source = {stream: instream, promptMode:bool}
    datatype token
       =  STRING of string 
       | EOF                          | ID of string
       | DIGITS of string             | SPECIAL of char
       | BANG            (* ! *)     (* | DOUBLEQUOTE     (* " *)  *)
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
     val lex : source -> token
     val initToken : source -> unit
     val nextToken : source -> token
     val currentToken : token option ref
   end

  structure Lex : LEX =
  struct
    structure T = TextIO
    type instream = T.instream
    type source = {stream: instream, promptMode:bool}
    datatype token
      =  STRING of string 
      | EOF                          | ID of string
      | DIGITS of string             | SPECIAL of char
      | BANG            (* ! *)      
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
    val currentToken = (ref NONE : token option ref)
    
    fun skipSpaces ins =
         case T.lookahead ins of
           SOME c => if Char.isSpace c
                     then (T.input1 ins;skipSpaces ins)
                     else ()
         | _ => ()
    fun getString ins = 
      let 
        fun getRest (s, i) = 
          if i = 0 then s
          else 
            case T.lookahead ins of
              SOME c => if #"\"" = c 
                        then (T.inputN(ins,1); getRest (s, i - 1))
                        else getRest (s ^ T.inputN(ins,1), i)
            | NONE => s
      in
        STRING(getRest ("", 2))
      end
      (* 
      structure T = TextIO
      val ins = T.openIn "test.txt"
      getString ins;
       *)
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
    (* TODO: implment initToken *)
    fun initToken source = ()
    (* TODO: implment nextToken *)
    fun lex source =
      let
        fun getStream ({stream, ...}: source) = stream
        val ins = getStream source
      in
        case currentToken of
          ref (SOME tk) => tk
        | ref NONE =>
         (skipSpaces ins;
          if T.endOfStream ins then EOF
          else
           let
             val c = valOf (T.lookahead ins)
           in
             if #"\"" = c then getString ins
             else if Char.isDigit c then getNum ins
             else if Char.isAlpha c then getID ins
             else case valOf (T.input1 ins) of
               #"!" => BANG
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
      end
    fun nextToken source = 
      case currentToken of
         ref (SOME tk) => tk
       | ref NONE => 
         let
          val newToken = lex source
         in
          currentToken := SOME newToken; newToken
         end
       

         (* 補足: 「ファイル名は空白以外の任意の文字列とし、use sp* fileNameがトークン列のどこに現れてもよい」との仕様に相当 *)
      fun getFileName ins =
          let fun getRest s = 
                  case (T.lookahead ins) of
                    SOME c => 
                if Char.isSpace c then s
                    else getRest (s ^ T.inputN(ins,1))
                  | NONE => s
          in getRest ""
          end
    (* fun testMain ins =
        let
          val token = lex ins
        in
          case token of
             EOF => ()
           | ID "use" =>
               let
                 val fileName = (skipSpaces ins; getFileName ins)
                 val newIns = TextIO.openIn fileName
               in
                 (testMain newIns; testMain ins)
               end
            | _ => (print (toString token ^ "\n");
                    testMain ins)
        end
    fun testLex () = testMain TextIO.stdIn *)
   end
