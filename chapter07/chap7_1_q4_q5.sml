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
          substring (s, lp2 + 1, rp2-lp2-1),
          substring (s, rp2 + 1, size s - rp2 - 1)
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
fromPostOrderToNewTree testPost;

(* 筆者の解答 *)
datatype 'a newTree
   = Leaf of 'a
   | Node of 'a * 'a newTree * 'a newTree;

datatype decompose
  = TRIPLE of {root:string, left:string, right:string}
  | SINGLETON of string;

fun searchLP s p =
    if substring(s,p,1) = "(" then p
    else searchLP s (p+1);

fun searchRP s p n =
    case substring(s,p,1) of
        "(" => searchRP s (p+1) (n+1)
       | ")" => if n=0 then p else searchRP s (p+1) (n - 1)
       | _ => searchRP s (p+1) n;

fun decomposeN s =
   let val lp1 = searchLP s 0
       val rp1 = searchRP s (lp1+1) 0
       val lp2 = searchLP s  rp1
       val rp2 = searchRP s (lp2+1) 0
   in TRIPLE
      {
       root=substring (s,0,lp1),
       left=substring (s,lp1+1,rp1-lp1 -1),
       right=substring (s,lp2+1,rp2-lp2-1)
      }
   end handle Subscript => SINGLETON s;

fun decomposeInN s =
   let val lp1 = searchLP s 0
       val rp1 = searchRP s (lp1+1) 0
       val lp2 = searchLP s  rp1
       val rp2 = searchRP s (lp2+1) 0
   in TRIPLE
      {
       root=substring (s,rp1+1,lp2-rp1 - 1),
       left=substring (s,lp1+1,rp1-lp1 -1),
       right=substring (s,lp2+1,rp2-lp2-1)
      }
   end handle Subscript => SINGLETON s;

fun decomposePostN s =
   let val lp1 = searchLP s 0
       val rp1 = searchRP s (lp1+1) 0
       val lp2 = searchLP s  rp1
       val rp2 = searchRP s (lp2+1) 0
   in TRIPLE
      {
       root=substring (s,rp2+1,size s - rp2 - 1),
       left=substring (s,lp1+1,rp1-lp1 -1),
       right=substring (s,lp2+1,rp2 - lp2-1)
      }
   end handle Subscript => SINGLETON s;

fun decoposeToNewTree decomp s =
    case decomp s of
         SINGLETON s => Leaf s
       | TRIPLE {root, left, right} =>
         Node(root, decoposeToNewTree decomp left, decoposeToNewTree decomp right);

fun preOrderToNewTree s = decoposeToNewTree decomposeN s;

fun inOrderToNewTree s = decoposeToNewTree decomposeInN s;

fun postOrderToNewTree s = decoposeToNewTree decomposePostN s; 

Control.Print.printDepth := 20;
preOrderToNewTree testPre;
inOrderToNewTree testIn;
postOrderToNewTree testPost;

(* 筆者の解答の改善版 *)
datatype 'a newTree
   = Leaf of 'a
   | Node of 'a * 'a newTree * 'a newTree
   | NodeL of 'a * 'a newTree
   | NodeR of 'a * 'a newTree;

datatype decompose
  = TRIPLE of {root:string, left:string, right:string}
  | SINGLETON of string;

fun searchLP s p =
    if substring(s,p,1) = "(" then p
    else searchLP s (p+1);

fun searchRP s p n =
    case substring(s,p,1) of
        "(" => searchRP s (p+1) (n+1)
       | ")" => if n=0 then p else searchRP s (p+1) (n - 1)
       | _ => searchRP s (p+1) n;

fun decomposeN s =
   let val lp1 = searchLP s 0
       val rp1 = searchRP s (lp1+1) 0
       val lp2 = searchLP s  rp1
       val rp2 = searchRP s (lp2+1) 0
   in 
    TRIPLE
      {
       root=substring (s,rp1+1,lp2-rp1 - 1),
       left=substring (s,lp1+1,rp1-lp1 -1),
       right=substring (s,lp2+1,rp2-lp2-1)
      }
   end handle Subscript => SINGLETON s;

fun decomposeInN s =
   let val lp1 = searchLP s 0
       val rp1 = searchRP s (lp1+1) 0
       val lp2 = searchLP s  rp1
       val rp2 = searchRP s (lp2+1) 0
   in TRIPLE
      {
       root=substring (s,rp1+1,lp2-rp1 - 1),
       left=substring (s,lp1+1,rp1-lp1 -1),
       right=substring (s,lp2+1,rp2-lp2-1)
      }
   end handle Subscript => SINGLETON s;

fun decomposePostN s =
   let val lp1 = searchLP s 0
       val rp1 = searchRP s (lp1+1) 0
       val lp2 = searchLP s  rp1
       val rp2 = searchRP s (lp2+1) 0
   in TRIPLE
      {
       root=substring (s,rp2+1,size s - rp2 - 1),
       left=substring (s,lp1+1,rp1-lp1 -1),
       right=substring (s,lp2+1,rp2 - lp2-1)
      }
   end handle Subscript => SINGLETON s;

fun decoposeToNewTree decomp s =
    case decomp s of
         SINGLETON s => Leaf s |
         TRIPLE {root, left, right=""} => NodeL(root, decoposeToNewTree decomp left) |
         TRIPLE {root, left="", right} => NodeR(root, decoposeToNewTree decomp right) |
         TRIPLE {root, left, right} => Node(root, decoposeToNewTree decomp left, decoposeToNewTree decomp right)


fun preOrderToNewTree s = decoposeToNewTree decomposeN s;

fun inOrderToNewTree s = decoposeToNewTree decomposeInN s;

fun postOrderToNewTree s = decoposeToNewTree decomposePostN s; 

(* Control.Print.printDepth := 20; *)
preOrderToNewTree testPre;
inOrderToNewTree testIn;
postOrderToNewTree testPost;

(* 筆者の回答の完全版 *)
datatype 'a newTree
   = Leaf of 'a
   | Node of 'a * 'a newTree * 'a newTree
   | NodeL of 'a * 'a newTree
   | NodeR of 'a * 'a newTree;
(* SML#には、汎用の文字列への変換関数Dynamic.formatが 定義されており、以下のように使用することができる。

   # Dynamic.format;
   val it = fn : [’a#reify. ’a -> string]
   # Dynamic.format (1,2);
   val it = "(1, 2)" : string

これを文字列表現の定義として採用するためには、これら文字列への変 換関数の逆関数が必要である。
残念ながら、Dynamic.formatに対応する汎用の逆関数はサポートされていない。 そこで、個々の型に対して定義する必要がある。
(というか、SMLの教科書だからSML#をいきなり出されても、という感情がある)

そこで、区切り記号を、可能な限り、種々の文字列表現に現れないものを選ぶ必要がある。 
ここでは、 "\000" と "\001" の２つを利用することとする
  これは、プリント可能なstring型を含む殆どの原子型や、それ らから構成される組型、リスト型の文字列表現に現れないので
  ほぼ条件を満たす
*)
  val lp = "\000"
  val rp = "\001"
  val LP = #"\000"
  val RP = #"\001";

(* 変数に対してはパターンマッチングが使用できない *)
fun searchLP s p =
  if substring(s,p,1) = lp then p
  else searchLP s (p+1);

fun searchRP s p n =
  if substring(s,p,1) = lp then searchRP s (p+1) (n+1)
  else if substring(s,p,1) = rp then
    if n=0 then p else searchRP s (p+1) (n - 1)
  else searchRP s (p+1) n

fun decompose s =
   let val lp1 = searchLP s 0
       val rp1 = searchRP s (lp1+1) 0
       val lp2 = searchLP s  rp1
       val rp2 = searchRP s (lp2+1) 0
   in 
    (substring (s,0,lp1),
          substring (s,lp1+1,rp1-lp1 -1),
          substring (s,lp2+1,rp2 - lp2-1))
   end

fun decomposeIn s =
   let val lp1 = searchLP s 0
       val rp1 = searchRP s (lp1+1) 0
       val lp2 = searchLP s  rp1
       val rp2 = searchRP s (lp2+1) 0
   in (substring (s, rp1+1, lp2 - rp1 - 1),
      substring (s, lp1+1, rp1 - lp1 -1),
      substring (s, lp2+1, rp2 - lp2 - 1))
   end

fun decomposePost s =
   let val lp1 = searchLP s 0
       val rp1 = searchRP s (lp1+1) 0
       val lp2 = searchLP s  rp1
       val rp2 = searchRP s (lp2+1) 0
   in (substring (s,rp2+1,size s - rp2 - 1),
      substring (s,lp1+1,rp1-lp1 -1),
      substring (s,lp2+1,rp2 - lp2-1))
   end;

fun toPreOrder toString t =
    let
      val preOrder = toPreOrder toString
    in
      case t of
        Leaf v => toString v ^ lp ^ rp ^ lp ^ rp
      | Node(v, L, R) => toString v ^ lp ^ preOrder L ^ rp ^ lp ^ preOrder R ^ rp
      | NodeL(v, L) => toString v ^ lp ^ preOrder L ^ rp ^ lp ^ rp
      | NodeR(v, R) => toString v ^ lp ^ rp ^ lp ^ preOrder R ^ rp
    end

fun toInOrder toString t =
    let
      val inOrder = toInOrder toString
    in
      case t of
        Leaf v => lp ^ rp ^ toString v ^ lp ^ rp
      | Node(v, L, R) => lp ^ inOrder L ^ rp ^ toString v ^ lp ^ inOrder R ^ rp
      | NodeL(v, L) => lp ^ inOrder L ^ rp ^ toString v ^ lp ^ rp
      | NodeR(v, R) => lp ^ rp ^ toString v ^ lp ^ inOrder R ^ rp
    end

fun toPostOrder toString t =
    let
      val postOrder = toPostOrder toString
    in
      case t of
        Leaf v => lp ^ rp ^ lp ^ rp ^ toString v
      | Node(v, L, R) => lp ^ postOrder L ^ rp ^ lp ^ postOrder R ^ rp ^ toString v
      | NodeL(v, L) => lp ^ postOrder L ^ rp ^ lp ^ rp ^ toString v
      | NodeR(v, R) => lp ^ rp ^ lp ^ postOrder R ^ rp ^ toString v
    end

(* この生成処理は、使用するdecompose関数を除き同一であるから、 高階の関数toNewTreeにまとめ、それを利用して各関数を以下のように定義 *)
fun toNewTree decomp fromString s =
    let
      val (root,left,right) = decomp s
      val toTree = toNewTree decomp fromString
    in
      case (left, right) of
        ("","") => Leaf (fromString root)
      | (_, "") => NodeL (fromString root, toTree left)
      | ("",_) => NodeR (fromString root, toTree right)
      | _ => Node (fromString root, toTree left, toTree right)
    end;

fun fromPreOrderToNewTree fromString s = toNewTree decompose fromString s
fun fromInOrderToNewTree fromString s = toNewTree decomposeIn fromString s
fun fromPostOrderToNewTree fromString s = toNewTree decomposePost fromString s;

(* Control.Print.printDepth := 20; *)
(* fromPreOrderToNewTree testPre;
fromInOrderToNewTree testIn;
fromPostOrderToNewTree testPost; *)

   fun stringToIntInt s =
       let
         val ss = Substring.full s
         val (x,y) = Substring.splitl (fn x => x <> #",") ss
         val x = Substring.dropl (fn x => x = #"(") x
         val x = Substring.string (Substring.takel Char.isDigit x)
         val y = Substring.dropl (fn x => x = #"," orelse Char.isSpace x) y
         val y = Substring.string (Substring.takel Char.isDigit y)
         val i1 = valOf (Int.fromString x)
         val i2 = valOf (Int.fromString y)
       in
         (i1,i2)
       end

   fun fromPreOrderToIntIntNewTree s = fromPreOrderToNewTree stringToIntInt s
   fun fromInOrderToIntIntNewTree s = fromInOrderToNewTree stringToIntInt s
   fun fromPostOrderToIntIntNewTree s = fromPostOrderToNewTree stringToIntInt s

val preOrderOfT =
     "(1, 2)\^@(2, 3)\^@\^A\^@\^A\^A\^@(3, 4)\^@\^A\^@(4, 5)\^@\^A\^@\^A\^A\^A"

val inOrderOfT =
     "\^@\^@\^A(2, 3)\^@\^A\^A(1, 2)\^@\^@\^A(3, 4)\^@\^@\^A(4, 5)\^@\^A\^A\^A"
 val postOrderOfT =
     "\^@\^@\^A\^@\^A(2, 3)\^A\^@\^@\^A\^@\^@\^A\^@\^A(4, 5)\^A(3, 4)\^A(1, 2)";

fromPreOrderToIntIntNewTree preOrderOfT;
fromInOrderToIntIntNewTree inOrderOfT;
fromPostOrderToIntIntNewTree postOrderOfT;;