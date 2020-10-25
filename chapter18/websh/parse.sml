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

        fun check tk = (* check whether tk or not *)
          if L.nextToken(source) = tk
          then ( 
            (* print ("token is same. \nexpected: " ^ (L.toString tk) ^ "\nresult:" ^ L.toString(L.nextToken(source)) ^ "\n"); *)
            ()
          )
          else (skip();
                (* 以下の一行はテスト用のため *)
                print ("token is different. \nexpected: " ^ (L.toString tk) ^ "\nresult:" ^ L.toString(L.nextToken(source)) ^ "\n");
                raise Syntax)

        fun getInt () = (* read data whose type is int *)
           case L.nextToken(source) of
               L.DIGITS s => valOf(Int.fromString(s))
             | _ => raise Syntax

        fun parseExpr () = 
          (* 以下のlet in endでくくってテストに表示させるようにする *)
          let
            val exp = L.lex source
          in
            (* print (L.toString exp ^ "\n"); *)
            case exp of
              L.ID(s) =>
                  (
                    (* print "ID in parseExpr\n"; *)
                    case s of
                    "link" => (* read link formula *)
                        let
                          val _ = L.nextToken source
                          val _ = check L.LPAREN
                          val _ = L.lex source
                          val e = parseExpr ()
                          val _ = check L.RPAREN
                          val _ = L.lex source
                        in
                          LINKEXP e
                        end
                  | "follow" => (* read follow formula*)
                        let
                          val _ = L.nextToken source
                          val _ = check L.LPAREN
                          val _ = L.lex source
                          val e = parseExpr ()
                          val _ = check L.COMMA
                          val _ = L.lex source
                          val i = case L.lex source of
                                      L.DIGITS n => valOf(Int.fromString n)
                                    | _ => syntaxError()
                          val _ = check L.RPAREN
                          val _ = L.lex source
                        in
                          FOLLOWEXP(e, i)
                        end
                  | _ => IDEXP s)
            | L.SSTRING s => STREXP s
            | L.DSTRING s => STREXP s
            | L.EOF => raise endOfInput (* C-dでParseErrorが発生することへの対処 *)
            | _ => (print "parse Error\n";
                    syntaxError())
        end
      in
        case L.nextToken source of
           L.SEMICOLON => (L.lex source;
                           Control.doFirstLinePrompt := true;
                           parse source)
         | L.ID("val") => let
                            val _ = L.lex source
                            val id = case L.lex source of
                                        L.ID s => s
                                      | _ => syntaxError()
                            val _ = check L.EQUALSYM
                            val _ = L.lex source
                            val e = parseExpr ()
                            val _ = check L.SEMICOLON
                          in
                            VAL(id,e)
                          end
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
                            val _ = L.lex source
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

