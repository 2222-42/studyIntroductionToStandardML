
use "./Control.sml";
use "./Types.sml";
use "./Lex.sml";
structure Parse =
struct
  local
    open Types Control
    structure L = Lex
  in
    fun parse source =
      let
        fun skip () = (* ; まで読み飛ばす*)
          case L.nextToken source of
            L.SEMICOLON => ()
          | _ => (L.initToken(source); skip())

        fun syntaxError () = (* report Error *)
          (skip(); raise Syntax)

        (* TODO:この関数を呼び出すどこかでエラーが起きているのを修正 *)
        fun check tk = (* check whether tk or not *)
          if L.nextToken(source) = tk
          then ()
          else (skip();
                (* 以下の一行はテスト用のため *)
                print ("token is different: " ^ L.toString(L.nextToken(source)));
                raise Syntax)

        fun getInt () = (* read data whose type is int *)
           case L.nextToken(source) of
               L.DIGITS s => valOf(Int.fromString(s))
             | _ => raise Syntax

        fun parseExpr () = 
          case L.lex source of
             L.ID(s) =>
                (case s of
                   "link" => (* read link formula *)
                      let
                        val _ = L.lex source
                        val _ = check L.LPAREN
                        val e = parseExpr ()
                        val _ = check L.RPAREN
                      in
                        LINKEXP e
                      end
                 | "follow" => (* read follow formula*)
                      let
                        val _ = L.lex source
                        val _ = check L.LPAREN
                        val e = parseExpr ()
                        val _ = check L.COMMA
                        val i = case L.lex source of
                                    L.DIGITS n => valOf(Int.fromString n)
                                  | _ => syntaxError()
                        val _ = check L.RPAREN
                      in
                        FOLLOWEXP(e, i)
                      end
                 | _ => IDEXP s)
           | L.STRING s => STREXP s
           | _ => (print "parse Error\n";
                   syntaxError())
      in
        (* 改行時の問題修正 *)
        (* L.initToken(source); *)
        case L.nextToken source of
           L.SEMICOLON => (L.lex source;
                           Control.doFirstLinePrompt := true;
                           parse source)
         | L.ID("val") => (let
                            val _ = L.lex source
                            val id = case L.lex source of
                                        L.ID s => (print (s^"\n");
                                                   s)
                                      | _ => syntaxError()
                            (* TODO:ここでSEMICOLONを読み込んでいる問題の処理 *)
                            val _ = (print "checkEQ\n";
                                     check L.EQUALSYM)
                            val e = (print "parseExpr\n";
                                     parseExpr ())
                            val _ = (print "checkSEMIC\n";
                                     check L.SEMICOLON)
                          in
                            VAL(id,e)
                          end before print "checkID")
         | L.ID("cd") => (* read cd sentence *)
                          let
                            val _ = L.lex source
                            val e = parseExpr ()
                            val _ = check L.SEMICOLON
                          in
                            CD e
                          end
         | L.ID("use") => (* read use sentence *)
                          let
                            val _ = L.lex source
                            val id = case L.lex source of
                                        L.ID s => s
                                      | _ => syntaxError()
                            val _ = check L.SEMICOLON
                          in
                            USE id
                          end
         | L.ID("print") => (* read print sentence *)
                          let
                            val _ = L.lex source
                            val e = parseExpr ()
                            val _ = check L.SEMICOLON
                          in
                            PRINT e
                          end
         | L.ID("copy") => (* read copy sentence *)
                          let
                            val _ = L.lex source
                            val e1 = parseExpr ()
                            val _ = check (L.ID "to")
                            val e2 = parseExpr ()
                            val _ = check L.SEMICOLON
                          in
                            COPY (e1,e2)
                          end
         | L.ID("help") => (* read help sentence *)
                          let
                            val _ = L.lex source
                            val _ = check L.SEMICOLON
                          in
                            HELP
                          end
         | L.ID("env") => (* read env sentence *)
                          let
                            val _ = L.lex source
                            val _ = check L.SEMICOLON
                          in
                            ENV
                          end
         | _ => EXPR (parseExpr () before check L.SEMICOLON)
      end
  end
end

(* 
val ins = (TextIO.openIn "test.txt");
val source = ({stream=ins, promptMode=true}:Lex.source);
Parse.parse source;

->checkEQSEMICOLON
uncaught exception Syntax
  raised at: Parse.sml:25.70-25.76

    structure L = Lex
    L.testLex()
val source = ({stream=TextIO.stdIn, promptMode=true}:Lex.source);
Parse.parse source;
*)