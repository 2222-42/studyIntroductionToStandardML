(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 5.4
*)

(* 
フィールド取り出し演算子
レコードバターンを引数とする関数式
これらはラベルlを含む種々の型のレコードに適用可能
*)

#Name {Name="Joe", Age=21};
#Name{Name={First="Joe",Last="Doe"}, Office="51B"};

(* 
`fn {Name=x, ...} => x`は、Nameフィールドを含む種々の型に適用可能な汎用性を持つ多相関数
*)

(* 
OK:
fun name {Name, Age} = Name;

NG:
fun name {Name=x, ...} = x;
fun name x = #Name x;

理由：
引数のレコードのラベル集合が不明なので。
*)