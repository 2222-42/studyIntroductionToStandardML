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
    val scanStream : ((elem,StreamIO.instream) StringCvt.reader
                      -> ('a,StreamIO.instream) StringCvt.reader)
                     -> instream -> 'a option
  end
*)

signature EXTERNALIO =
sig
  datatype instream = StreamIn of TextIO.instream
                    | ProcessIn of (TextIO.instream,TextIO.outstream) Unix.proc
  type outstream
  val openIn : Types.url -> instream
  val closeIn : instream -> unit
  val openOut : Types.url -> outstream
  val closeOut : outstream -> unit
  val endOfStream : instream -> bool
  val lookahead : instream -> char option
  val input1 : instream -> char option
  val inputN : instream * int -> string
  val output1 : outstream * char -> unit
  val flushOut : outstream -> unit
  val copyStream : instream -> outstream -> unit
  (* printだと名前衝突が起きるので *)
  (* val printPageOrUrl : Types.url -> unit *)
end

structure ExternalIO : EXTERNALIO =
struct
  local
    open Types Control 
  in
    type url = Types.url
    datatype instream = StreamIn of TextIO.instream
                      | ProcessIn of (TextIO.instream,TextIO.outstream) Unix.proc
    type outstream = TextIO.outstream
    fun openIn url = 
      let
        val url' = Url.canonicalUrl url
      in
        case url' of
           HTTP _ => ProcessIn(Unix.execute ("/usr/bin/lynx", ["-source", Url.urlToString url']))
         | FILE {path, ...} => 
              StreamIn
                (TextIO.openIn
                  (OS.Path.toString {arcs=path, isAbs=true, vol=""}))
         | RELATIVE _ => 
              raise urlFormat       
      end
    fun closeIn source =
      case source of
         StreamIn s => TextIO.closeIn s
       | ProcessIn p => 
          let
            val (ins, _) = Unix.streamsOf p
          in
            TextIO.closeIn ins
          end
    fun openOut (adrs as FILE {path,...}) =
        TextIO.openOut
          (OS.Path.toString {arcs=path,vol="", isAbs=true})
      | openOut _ = raise urlFormat
    fun closeOut source =
       TextIO.closeOut source
    fun endOfStream source = 
      case source of
         StreamIn s => TextIO.endOfStream s
       | ProcessIn p => 
          let
            val (ins, _) = Unix.streamsOf p
          in
            TextIO.endOfStream ins
          end
    fun lookahead source =
      case source of
         StreamIn s => TextIO.lookahead s
       | ProcessIn p => 
          let
            val (ins, _) = Unix.streamsOf p
          in
            TextIO.lookahead ins
          end
    fun input1 source = 
      case source of
         StreamIn s => TextIO.input1 s
       | ProcessIn p => 
          let
            val (is, _) = Unix.streamsOf p
          in
            TextIO.input1 is
          end
    fun inputN (source, n) = 
      case source of
         StreamIn s => TextIO.inputN(s, n)
       | ProcessIn p => 
          let
            val (is, _) = Unix.streamsOf p
          in
            TextIO.inputN(is, n)
          end
    val output1 = TextIO.output1
    val flushOut = TextIO.flushOut
    fun copyStream source outs = 
      if endOfStream source then ()
      else case input1 source of
            SOME c => (output1(outs,c);
                        copyStream source outs)
          | NONE => copyStream source outs
    (* cyclic ML dependenciesが起きるので *)
    (* fun printPageOrUrl url = 
     print (Print.valueToString (ParseHtml.parseHtml url url)) *)
  end
end