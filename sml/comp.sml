
(*
  read lines
  split functions
  trace paths
  make structured
  optimizations?
  code generation
  file output
*)

load "TextIO";

(***** FILE INPUT *****)

val read_lines = String.tokens (fn c=>c = #"\n") o TextIO.inputAll

(***** PARSING DECLARATIONS *****)

type Field = string vector
datatype Direction = UP | DN | LF | RT

(* TODO *)

(***** IR *****)

type Coordinate = int * int
type Place = Coordinate * Direction
type Mark = bool * bool * bool * bool

type IR_Cell =
  { src: char * char
  , mrk: Mark
  }
type IR_Field = IR_Cell vector vector

val to_ir : Field -> IR_Field =
    Vector.map
      let fun pairs acc (a::b::rest) =
                pairs ( { src = (a,b)
                        , mrk = (false, false, false, false)
                        }::acc )
                      rest
            | pairs acc _ = acc
      in
        Vector.fromList o List.rev o pairs [] o String.explode
      end

(***** PATH TRACING *****)

datatype IR_Cmd
  = Call of (char * char)
  | Primitive of (char * char)
  | PushNum of int
  | If of Place
  | Nop
  | Return
type IR = (Place * IR_Cmd * Place) list

fun delta UP = ( 0,~1)
  | delta DN = ( 0, 1)
  | delta LF = (~1, 0)
  | delta RT = ( 1, 0)

val getmark : Direction -> Mark -> bool =
  ( fn d => case d
     of UP => #1
      | DN => #2
      | LF => #3
      | RT => #4
  )

fun setmark dir (u, d, l, r) B = case dir
 of UP => (B, d, l, r)
  | DN => (u, B, l, r)
  | LF => (u, d, B, r)
  | RT => (u, d, l, B)

fun advance f (x, y) d =
  let val (dx, dy) = delta d in
    ( (x+dx) mod (size (Vector.sub(f, y)) div 2)
    , y+dy )
  end

fun get_cell f (x, y) =
  let
    val row = Vector.sub(f, y)
  in
    SOME (Vector.sub(row, x)) handle Subscript => NONE
  end

(* val parse : Place -> IR -> IR_Field -> IR * IR_Field *)
fun parse (xy, dir) acc field : IR =
  case get_cell field xy
   of NONE => parse (advance field xy dir, dir) acc field
    | SOME {src, mrk} =>
      if getmark dir mrk then (acc, field) (* if already traversed, done *)
      else
        case src
         of (#".", #"@") => 

(*
  #["~:?!?v01+-"
    ".@  .<"]
  parses to
  [00r Primitive ~: 10r, 10r Primitive ?! 20r, 20r If 21d 30r,
   30r PushNum 01 40r, 40r Primitive +- 00r, 21d Nop 11l, 11l Nop 01l,
   01l Return 21l]
*)

