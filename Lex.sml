(* SML source file. Copyright (c) by 2222-42 2020.
* Q18.2
*)

use "./Control.sml";
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
    val testLex : unit -> unit
    (* 以下はテスト用のため *)
    val toString : token -> string
    (* val getStream : source -> instream *)
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
    
    fun getStream ({stream, ...}: source) = stream
    fun getPrompt ({promptMode, ...}: source) = promptMode
    fun printPromt () = 
      if (!(Control.doFirstLinePrompt))
      then print (! Control.firstLinePrompt)
      else print (! Control.secondLinePrompt)

    fun skipSpaces (ins, mode) =
         case T.lookahead ins of
           SOME c => if c = #"\n" 
                     then (T.input1 ins; if mode then printPromt() else ();skipSpaces (ins, mode))
                     else if Char.isSpace c
                          then (T.input1 ins;skipSpaces (ins, mode))
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

    fun initToken source = currentToken := NONE

    fun lex source =
      case currentToken of
        ref (SOME tk) => tk before (initToken source)
      | ref NONE =>
        let
          val ins = getStream source
        in
         (skipSpaces (ins, getPrompt source);
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

    fun toString tok =
      case tok of
          STRING s => "STRING\"" ^ s ^"\""
        | EOF => "EOF"
        | ID s => "ID(" ^ s ^ ")"
        | DIGITS s => "DIGITS(" ^ s ^ ")"
        | SPECIAL c => "SPECIAL" ^ Char.toString c ^ ")"
        | BANG => "BANG"
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

    fun nextToken source = 
      case currentToken of
         ref (SOME tk) => tk
       | ref NONE => 
         let
          val newToken = lex source
         in
          currentToken := SOME newToken;
          newToken
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
    (* これだと無限に読み込んでしまう
        -> sourceのstreamの内容が更新されていない *)
    fun testMain source =
       (if (getPrompt source) then printPromt() else ();
        let
          val ahead = nextToken source
          val token = lex source
          val ins = getStream source
        in
          case token of
             EOF => ()
           (* useで次の中身を見るために、nextTokenが更新されないためダメ *)
           | ID "use" =>
               let
                 val fileName = (skipSpaces (ins, getPrompt source); getFileName (ins))
                 val newSource = {stream=TextIO.openIn fileName, promptMode=(getPrompt source)}
               in
                 (testMain newSource; testMain source)
               end
           | _ => (print ("lookahead: " ^ toString ahead ^ "\n");
                   print ("lex: " ^ toString token ^ "\n" );
                   testMain source)
        end)
    fun testLex () = testMain {stream=TextIO.stdIn, promptMode=true}
   end

(* ;
Lex.testLex ();
123 *)

(*  Lex.testLex ();     
-> val id = "test";
val id = "test";
init
lookahead: ID(val)
lex: ID(val)
->init
init
lookahead: ID(id)
lex: ID(id)
->init
init
lookahead: EQUALSYM
lex: EQUALSYM
->init
init
lookahead: STRING"test"
lex: STRING"test"
->init
init
lookahead: SEMICOLON
lex: SEMICOLON
->init *)
