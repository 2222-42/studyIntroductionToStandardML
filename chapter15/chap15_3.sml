(* SML source file. Copyright (c) by 2222-42 2020.
* Chap15.3
*)

(* 
signature TEXT_IO =
  sig
    type vector = string
    type elem = char
    type instream
    type outstream
    val input : instream -> vector
    val input1 : instream -> elem option       
    val inputN : instream * int -> vector      
    val inputAll : instream -> vector
    val canInput : instream * int -> int option
    val lookahead : instream -> elem option    
    val closeIn : instream -> unit
    val endOfStream : instream -> bool
    val output : outstream * vector -> unit
    val output1 : outstream * elem -> unit
    val flushOut : outstream -> unit
    val closeOut : outstream -> unit
    structure StreamIO :
      sig
        type vector = string
        type elem = char
        type reader
        type writer
        type instream
        type outstream
        type pos
        type out_pos
        val input : instream -> vector * instream
        val input1 : instream -> (elem * instream) option
        val inputN : instream * int -> vector * instream
        val inputAll : instream -> vector * instream
        val canInput : instream * int -> int option
        val closeIn : instream -> unit
        val endOfStream : instream -> bool
        val mkInstream : reader * vector -> instream
        val getReader : instream -> reader * vector
        val filePosIn : instream -> pos
        val output : outstream * vector -> unit
        val output1 : outstream * elem -> unit
        val flushOut : outstream -> unit
        val closeOut : outstream -> unit
        val setBufferMode : outstream * IO.buffer_mode -> unit
        val getBufferMode : outstream -> IO.buffer_mode
        val mkOutstream : writer * IO.buffer_mode -> outstream
        val getWriter : outstream -> writer * IO.buffer_mode
        val getPosOut : outstream -> out_pos
        val setPosOut : out_pos -> unit
        val filePosOut : out_pos -> pos
        val inputLine : instream -> (string * instream) option
        val outputSubstr : outstream * substring -> unit
      end
    val mkInstream : StreamIO.instream -> instream
    val getInstream : instream -> StreamIO.instream
    val setInstream : instream * StreamIO.instream -> unit
    val getPosOut : outstream -> StreamIO.out_pos
    val setPosOut : outstream * StreamIO.out_pos -> unit
    val mkOutstream : StreamIO.outstream -> outstream
    val getOutstream : outstream -> StreamIO.outstream
    val setOutstream : outstream * StreamIO.outstream -> unit
    val inputLine : instream -> string option
    val outputSubstr : outstream * substring -> unit
    val openIn : string -> instream
    val openString : string -> instream
    val openOut : string -> outstream
    val openAppend : string -> outstream
    val stdIn : instream
    val stdOut : outstream
    val stdErr : outstream
    val print : string -> unit
    val
        scanStream : ((elem,StreamIO.instream) StringCvt.reader
                       -> ('a,StreamIO.instream) StringCvt.reader)
                      -> instream -> 'a option
  end
*)

signature ADVANCED_IO = sig
    type instream
    type outstream
    val openIn : string -> instream
    val openOut : string -> outstream
    val inputN : instream * int -> string
    val lookAheadN : instream * int -> string
    val endOfStream : instream -> bool
    val canInput : instream*int -> int option
    val check : instream -> unit
    val reset : instream -> unit
    val redirectIn : instream * instream -> unit
    val redirectOut : outstream * outstream -> unit
end

structure AdvancedIO :> ADVANCED_IO = struct
    structure T = TextIO
    structure S = TextIO.StreamIO
    type instream = T.instream * S.instream ref
    type outstream = T.outstream
    fun openIn f = let val s = T.openIn f
                   in (s, ref (T.getInstream s))
                   end
    fun inputN ((s,_), n) = T.inputN (s,n)
    fun lookAheadN ((s,_), n) = let val ss = T.getInstream s
                                in #1 (S.inputN (ss,n))
                                end
    fun endOfStream(s,_) = T.endOfStream s
    fun canInput ((s,_), n) = T.canInput(s,n)
    fun check (s, ss) = ss := T.getInstream s
    fun reset (s, ref ss) = T.setInstream(s, ss)
    fun redirectIn ((s1,_),(s2,_)) = T.setInstream (s1, T.getInstream s2)
    fun redirectOut (s1, s2) = T.setOutstream (s1, T.getOutstream s2)
    val openOut = T.openOut
    val output = T.output
end
