signature PARSEHTML =
sig
  val parseHtml : Types.url -> Types.url -> Types.value
  val nextRef : Types.url -> Lex.source -> Types.url option
end
(* 
textの記述:
val nextRef : Types.url -> Types.source -> Types.url option

しかし、sourceはLexで定義されている。
Typesに追加してもよいが、Lexのままでいいかな。
*)

fun toLower c =
    if isUpper c
    then chr (ord #"a" + (ord c - ord #"A"))
    else c;
fun lower s = implode (map toLower (explode s))

(* 
TODO: 型エラーが起きているのを修正。
  operator domain: Lex.source
  operand:         TextIO.instream
  in expression:
    L.lex source

ExternalIOのinstream はTextIO.instreamで、
Lexのsource は {stream: instream, promptMode:bool}

つまり、T.lookahead sourceのsourceはTextIO.instream
*)
structure ParseHtml =
struct
local open Types Control
in
  structure L = Lex
  structure T = ExternalIO
  fun parseHtml url1 url2 =
    PAGE{url=url2, links=nil}
  fun nextRef root source =
    let
      fun check source tk = 
        if L.nextToken(source) = tk
        then L.initToken(source)
        else raise urlFormat
      fun skipUntil tk = 
        let val ins = (fn ({stream, ...}: L.source) => stream) source
        in
        case T.lookahead ins of
           SOME c => if c = #"<" then (T.input1 ins; ())
                     else (T.input1 ins;skipUntil tk)
         | _ => ()
        end
    in
      (skipUntil L.LANGLE;
       case (L.lex source, L.lex source) of
          (L.ID(s1), L.ID(s2)) => 
            if lower s1 = "a" andalso lower s2 = "href" orelse
               lower s1 = "img" andalso lower s2 = "src" then
               let
                 val s = (check source L.EQUALSYM; L.nextToken source)
               in
                 (case s of
                    L.STRING s => SOME(L.lex source; Url.parseUrl root s)
                  | _ => nextRef root source)
                  handle urlFormat => nextRef root source
               end
            else nextRef root source
        | _ => nextRef root source)
        handle endOfInput => NONE
    end
end
end
