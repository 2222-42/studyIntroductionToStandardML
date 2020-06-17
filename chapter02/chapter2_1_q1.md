定義された関数の型は以下のようになる

- `val f = fn : 'a -> 'a * int`
- `val g = fn : int * int -> int`

関数適用の左結合が理由で誤りが出る関数
operator is not a function:

- f f(1);
- ~g f(1);~ (-> 型の不一致が原因のため、理由が異なる)
- f g(1, 2);


型が一致しないため、エラーとなるもの
operator and operand do not agree:

- g f(1);
- g (f 1);
- g (f 1, f 2);
- f (g 1, g 2);

成功する関数

- f (f 1);
- f (f (1));
- f (1, 2);
- (f 1, f 2);
- g(1, 2);
- f (g(1,2));
