(* SML source file. Copyright (c) by 2222-42 2020.
* Q7.4 Q 7.5
*)

(* 
Q 7.4
「空の木」を使わず、「子を持たない木」を使った定義
*)

datatype 'a newTree = Leaf of 'a 
                    | Node of 'a * 'a newTree * 'a newTree 
                    | NodeL of 'a * 'a newTree
                    | NodeR of 'a * 'a newTree;

Node ("a", Leaf("b"), NodeL("c", Leaf("d")));

(* 
pre-orderによる木の表記法

- 空の木Empty は、空文字列を表す
- Node(a, L, R)
  - L(左部分木) -> S_L
  - R(右部分木) -> S_R
  - a(S_L)(S_R)
*)

val testPre = "a(b()())(c(d()())())"

fun fromPreOrderToNewTree s =
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
  in 
    let 
      val (root, left, right) = decompose s
    in 
      if (size left > 2) andalso (size right > 2) then 
        Node(root, fromPreOrderToNewTree left, fromPreOrderToNewTree right)
      else if (size left > 2) andalso (size right = 0) then
        NodeL(root, fromPreOrderToNewTree left)
      else if (size left = 0) andalso (size right > 2) then
        NodeR(root, fromPreOrderToNewTree right)
      else
        Leaf(root)
    end
  end;

fromPreOrderToNewTree testPre;
(* 
in-orderによる木の表記法

- 空の木Empty は、空文字列を表す
- Node(a, L, R)
  - L(左部分木) -> S_L
  - R(右部分木) -> S_R
  - (S_L)a(S_R)
*)
fun fromInOrderToNewTree s =
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
  in 
    let 
      val (left, root, right) = decompose s
    in 
      if (size left > 2) andalso (size right > 2) then 
        Node(root, fromInOrderToNewTree left, fromInOrderToNewTree right)
      else if (size left > 2) andalso (size right = 0) then
        NodeL(root, fromInOrderToNewTree left)
      else if (size left = 0) andalso (size right > 2) then
        NodeR(root, fromInOrderToNewTree right)
      else
        Leaf(root)
    end
  end;

val testIn = "(()b())a((()d())c())";
fromInOrderToNewTree testIn;

(* 
post-orderによる木の表記法

- 空の木Empty は、空文字列を表す
- Node(a, L, R)
  - L(左部分木) -> S_L
  - R(右部分木) -> S_R
  - (S_L)(S_R)a
*)
fun fromPostOrderToNewTree s =
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
  in 
    let 
      val (left, right, root) = decompose s
    in 
      if (size left > 2) andalso (size right > 2) then 
        Node(root, fromPostOrderToNewTree left, fromPostOrderToNewTree right)
      else if (size left > 2) andalso (size right = 0) then
        NodeL(root, fromPostOrderToNewTree left)
      else if (size left = 0) andalso (size right > 2) then
        NodeR(root, fromPostOrderToNewTree right)
      else
        Leaf(root)
    end
  end;

val testPost = "(()()b)((()()d)()c)a";
fromInOrderToNewTree testIn;
