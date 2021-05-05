
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
load "Int";

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

fun advance f d (x, y) =
  let val (dx, dy) = delta d in
    ( (x+dx) mod Vector.length (Vector.sub(f, y))
    , y+dy )
  end

fun get_cell f (x, y) : IR_Cell =
  case Vector.sub(f, y) of row => (Vector.sub(row, x))
      handle Subscript => {src=(#" ", #" "), mrk=(false, false, false, false)}
fun set_cell f (x, y) v =
  Vector.update(f, y, Vector.update(Vector.sub(f, y), x, v))

fun ch2dir c d =
  case c
   of #"^" => UP
    | #"v" => DN
    | #"<" => LF
    | #">" => RT
    | #"{" => LF
    | #"}" => RT
    | #"[" => LF
    | #"]" => RT
    | #"/"  => (case d of UP => RT | DN => LF | LF => DN | RT => UP)
    | #"\\" => (case d of UP => LF | DN => RT | LF => UP | RT => DN)
    | _ => raise Match

(* val parse : Place -> IR -> IR_Field -> IR * IR_Field *)
fun parse (xy, dir) acc field : IR * IR_Field =
  case get_cell field xy of {src, mrk} =>
    if getmark dir mrk then (acc, field) (* if already traversed, done *)
    else let
      val field = set_cell field xy {src=src, mrk=(setmark dir mrk true)}
      val xy' = advance field dir xy
    in
      case src
       of (#".", #"@") => (((xy, dir), Return, (xy, dir))::acc, field)
        | (#".", #"#") => let val xy'' = advance field dir xy' in
                            parse (xy'', dir)
                                  (((xy, dir), Nop, (xy'', dir))::acc) field
                          end
        | (#".", dirc) => (case fn d =>
                              case advance field d xy of xy' =>
                                parse (xy', d)
                                      (((xy, dir), Nop, (xy', d))::acc) field
                            of turn =>
                              turn (ch2dir dirc dir))
        | (#" ", #" ") => parse (xy', dir)
                                (((xy, dir), Nop, (xy', dir))::acc) field
        (* TODO: More movement commands *)
        | _ => parse (xy', dir)
                     (((xy, dir), Call src, (xy, dir))::acc) field
    end

(*
  #["~:?!?v01+-"
    ".@  .<"]
  parses to
  [00r Primitive ~: 10r, 10r Primitive ?! 20r, 20r If 21d 30r,
   30r PushNum 01 40r, 40r Primitive +- 00r, 21d Nop 11l, 11l Nop 01l,
   01l Return 21l]

  #[".>.v"
    ".^.<"]
  parses to
  [00r Nop 10r, 10r Nop 11d, 11d Nop 01l, 01l Nop 00u, 00u Nop 10r]
*)

(*** convenience for testing

fun testread (i : string vector) : IR * IR_Field =
  parse ((0,0), RT) [] (to_ir i)
val s = #[
  #[".>.v",
    ".^.<"],
  #["  .@"]
]

***)

