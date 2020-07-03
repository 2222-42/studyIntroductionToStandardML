(* SML source file. Copyright (c) by 2222-42 2020.
* Q 4.5
*)

fun isPrefix string1 string2 =
    let 
        val size1 = size string1
        val size2 = size string2
    in
        if size1 = 0 then true 
        else if size1 < size2 then false
        else substring(string1, 0, size2) = string2
    end;