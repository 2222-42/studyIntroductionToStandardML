   local
      exception NotThere
      fun f memo 0  = 1
        | f memo 1  = 1
        | f memo n =
            !memo n
              handle NotThere =>
                     let val v = f memo (n - 1) + f memo (n - 2)
                         val oldMemo = !memo
                         val _ = memo := (fn y => if n = y then v else oldMemo y)
                     in v
                     end
   in val fastFib = f (ref  (fn x => raise NotThere))
   end

   val _ = fastFib 43