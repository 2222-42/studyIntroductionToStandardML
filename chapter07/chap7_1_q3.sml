(* SML source file. Copyright (c) by 2222-42 2020.
* Q7.3
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
val test = "(()()b)((()()d)()c)a";
val l1 = searchLP test 0;
val r1 = searchRP test (l1 + 1) 0;
val l2 = searchLP test (r1 + 1);
val r2 = searchRP test (l2 + 1) 0;
substring (test, l1 + 1, r1 - l1 - 1);
substring (test, l2 + 1, r2 - l2 - 1);
substring (test, r2 + 1, 1); *)

fun fromPostOrder s = 
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
          substring (s, lp1 + 1, rp1-lp1-1),
          substring (s, lp2 + 1, rp2-lp2-1),
          substring (s, rp2 + 1, size s - rp2 - 1)
        )
      end
  in if s = "" then Empty
     else let val (left, right, root) = decompose s
          in Node(root, fromPostOrder left, fromPostOrder right)
          end
  end;

Control.Print.printDepth := 20;
fromPostOrder "(()()b)((()()d)()c)a";

(* 筆者の解答 *)
fun searchLP s p =
    if substring(s,p,1) = "(" then p
    else searchLP s (p+1);

fun searchRP s p n =
    case substring(s,p,1) of
        "(" => searchRP s (p+1) (n+1)
       | ")" => if n=0 then p else searchRP s (p+1) (n - 1)
       | _ => searchRP s (p+1) n;

fun decomposePost s =
   let val lp1 = searchLP s 0
       val rp1 = searchRP s (lp1+1) 0
       val lp2 = searchLP s  rp1
       val rp2 = searchRP s (lp2+1) 0
   in {root=substring (s,rp2+1,size s - rp2 - 1),
       left=substring (s,lp1+1,rp1-lp1 -1),
       right=substring (s,lp2+1,rp2 - lp2-1)}
   end

fun fromPostOrderByAuthor s =
    if s = "" then Empty
    else let val {root,left,right} = decomposePost s
         in Node(root,fromPostOrderByAuthor left,fromPostOrderByAuthor right)
         end;

(* 回答者のコメント:
size を使用しているのは確かにそちらの方が安全だからそれを使おう
*)

(* 
val it = Node ("a",Node ("b",Empty,Empty),Node ("c",Node #,Empty))
*)

(* fun searchLP s p = 
          if substring(s, p, 1) = "(" then p
          else searchLP s (p+1);
        fun searchRP s p n = 
          case (substring(s, p, 1), n) 
          of (")", 0) => p
          | (")", n) => searchRP s (p+1) (n-1)
          | ("(", n) => searchRP s (p+1) (n + 1)
          | (_, n) => searchRP s (p + 1) n;
val test = "(()b())a((()d())c())";
val l1 = searchLP test 0;
val r1 = searchRP test (l1 + 1) 0;
val l2 = searchLP test (r1 + 1);
val r2 = searchRP test (l2 + 1) 0;
substring (test, l1 + 1, r1 - l1 - 1);
substring (test, r1 + 1, 1);
substring (test, l2 + 1, r2 - l2 - 1); *)

fun fromInOrder s = 
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
          substring (s, lp1 + 1, rp1-lp1-1),
          substring (s, rp1 + 1, 1),
          substring (s, lp2 + 1, rp2-lp2-1)
        )
      end
  in if s = "" then Empty
     else let val (left, root, right) = decompose s
          in Node(root, fromInOrder left, fromInOrder right)
          end
  end;

fromInOrder "(()b())a((()d())c())";

(* 筆者の解答 *)

fun decomposeIn s =
   let val lp1 = searchLP s 0
       val rp1 = searchRP s (lp1+1) 0
       val lp2 = searchLP s  rp1
       val rp2 = searchRP s (lp2+1) 0
   in {root=substring (s,rp1+1,lp2 - rp1 - 1),
       left=substring (s,lp1+1,rp1-lp1 -1),
       right=substring (s,lp2+1,rp2 - lp2-1)}
   end;

fun fromInOrderByAuthor s =
    if s = "" then Empty
    else let val {root,left,right} = decomposeIn s
         in Node(root,fromInOrderByAuthor left,fromInOrderByAuthor right)
         end;

(* 回答者のコメント:

返り値の型をレコード型にしているのは可読性の向上につながる
*)
