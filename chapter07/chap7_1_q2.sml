(* SML source file. Copyright (c) by 2222-42 2020.
* Q7.2
*)
datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

        (* fun searchLP s p = 
          if substring(s, p, 1) = "(" then p
          else searchLP s (p+1);
        fun searchRP s p n = 
          case (substring(s, p, 1), n) 
          of (")", 0) => p
          | (")", n) => searchRP s (p+1) (n-1)
          | ("(", n) => searchRP s (p+1) (n + 1)
          | (_, n) => searchRP s (p + 1) n;
val test = "a(b()())(c(d()())())";
val lp1 = searchLP test 0;
val rp1 = searchRP test (lp1 + 1) 0;
val lp2 = searchLP test (rp1 + 1);
val rp2 = searchRP test (lp2 + 1) 0;
substring (test, 0, lp1);
substring (test, lp1 + 1, rp1-lp1-1);
substring (test, lp2 + 1, rp2-lp2-1); *)
(* fun searchLP s p = 
          if substring(s, p, 1) = "(" then p
          else searchLP s (p+1);

  searchLP "a(b()())(c(d()())())" 0;
-> LPの方は問題なさそう       
*)

fun fromPreOrder s =
  let fun decompose s = 
      let 
        fun searchLP s p = 
          if substring(s, p, 1) = "(" then p
          else searchLP s (p+1)
        fun searchRP s p n = 
          case (substring(s, p, 1), n) 
          of (")", 0) => p
          | (")", n) => searchRP s (p+1) (n-1)
          | ("(", n) => searchRP s (p+1) (n + 1)
          | (_, n) => searchRP s (p + 1) n
        val lp1 = searchLP s 0
        val rp1 = searchRP s (lp1 + 1) 0
        val lp2 = searchLP s (rp1 + 1)
        val rp2 = searchRP s (lp2 + 1) 0
      in
        (
          substring (s, 0, lp1),
          substring (s, lp1 + 1, rp1-lp1-1),
          substring (s, lp2 + 1, rp2-lp2-1)
        )
      end
  in if s = "" then Empty
     else let val (root, left, right) = decompose s
          in Node(root, fromPreOrder left, fromPreOrder right)
          end
  end;
(* 筆者の解答:
        fun searchRP s p n =
            case substring(s,p,1) of
                "(" => searchRP s (p+1) (n+1)
              | ")" => if n=0 then p else searchRP s (p+1) (n - 1)
              | _ => searchRP s (p+1) n

`case exp of ...` の `exp` の部分を上記のように設定する選択はもちろんありうる。
*)

fromPreOrder "";
fromPreOrder "a(b()())(c(d()())())";
(* 
uncaught exception Subscript [subscript out of bounds]
  raised at: smlnj/init/pervasive.sml:344.17-344.31 
どこ？　-> decomposeがあやしい
-> searchLPだとエラーが起きない
-> searchRPで期待した値が返ってこない
返り値がおかしい個所があったので修正したら直った。
*)

(* 
val it = Node ("a",Node ("b",Empty,Empty),Node ("c",Node #,Empty))

`Node #`ではなく、`Node "d", Empty, Empty` が欲しい
-> もしかして、深さによって、デフォルトでそうなってる？

Control.Print.printDepth := 20;
で値を変えると表示される。
*)