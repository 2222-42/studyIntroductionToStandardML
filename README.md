# studyIntroductionToStandardML

大堀(2001)『プログラミング言語StandardML入門』の読書ノート

## Tips

関数の最後に`;`を付けずに、その関数を実行させると、エラーが出る。

## how to use websh

Run the following sml command in the target directory(./chapter18/websh/):
```
SMLofNJ.exportFn ("Websh", 
  fn (x, argList) => (Websh.websh();OS.Process.success));
```

Set the following shellscript:
```
rlwrap sml @SMLload=chapter18/websh/Websh.x86-linux
```