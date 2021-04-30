
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

datatype IR_Cmd
  = Call of (char * char)
  | Primitive of (char * char)
  | Choice of {t: IR_Cmd, f: IR_Cmd}
  | Goto of Place
  | Nop
  | Return
type IR_Cell =
  { src: char * char
  , cmd: IR_Cmd option
  , x_y: Coordinate
  }
type IR_Field = IR_Cell vector vector

val to_ir : Field -> IR_Field =
    Vector.mapi
    ( fn (y, row) =>
      let fun pairs x acc (a::b::rest) =
                pairs (x+1)
                      ( { src = (a,b)
                        , cmd = NONE : IR_Cmd option
                        , x_y = (x, y)
                        }::acc )
                      rest
            | pairs x acc _ = acc
      in
        (Vector.fromList o List.rev o pairs 0 [] o String.explode) row
      end
    )

(***** PATH TRACING *****)

fun delta UP = ( 0,~1)
  | delta DN = ( 0, 1)
  | delta LF = (~1, 0)
  | delta RT = ( 1, 0)

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

