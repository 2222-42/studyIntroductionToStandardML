fun fib 0 = 1
    | fib 1 = 1
    | fib n = fib (n - 1) + fib (n - 2)
fun makeMemoFun f =
    let exception NotThere
        val memo = ref (fn x => (raise NotThere))
    in fn x => !memo x
            handle NotThere =>
                let val v = f x
                    val oldMemo = !memo
                in (memo := (fn y => if x = y then v else oldMemo y);
                    v)
                end
    end
val memoFib = makeMemoFun fib
val _ = memoFib 43