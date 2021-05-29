
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

exception SyntaxE of string;

(***** AST *****)

type Field = string vector
datatype Direction = UP | DN | LF | RT
type Coordinate = int * int
type Place = Coordinate * Direction
type Mark = bool * bool * bool * bool

type AST_Cell =
  { src: char * char
  , mrk: Mark
  }
type AST_Field = AST_Cell vector vector

fun cmd2str (a,b) = String.str a ^ String.str b

val to_ast : Field -> AST_Field =
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

(***** PARSING *****)

datatype AST_Cmd
  = Call of (char * char)
  | PushNum of int
  | If of Place
  | IfRet
  | Nop
  | Return
type AST = (Place * AST_Cmd * Place) list

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
    handle Subscript => raise SyntaxE "off the map"
  end

val default_cell = {src=(#" ", #" "), mrk=(false, false, false, false)}
fun get_cell f (x, y) : AST_Cell =
  case Vector.sub(f, y) of row => (Vector.sub(row, x))
      handle Subscript => default_cell
fun set_cell f (x, y) v =
  let fun update_padded(vec, x, v, padding) =
    Vector.tabulate ( x+1,
      fn i => if i < Vector.length vec then Vector.sub(vec, i)
              else if i = x then v else padding
    )
  in
    Vector.update(f, y,
      Vector.update(Vector.sub(f, y), x, v)
        handle Subscript =>
          update_padded( Vector.sub(f, y), x, v, default_cell)
    )
  end

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

(* val parse : Place -> AST -> AST_Field -> AST * AST_Field *)
fun parse (xy, dir) acc field : AST * AST_Field =
  case get_cell field xy of {src, mrk} =>
    if getmark dir mrk then (acc, field) (* if already traversed, done *)
    else let
      val field = set_cell field xy {src=src, mrk=(setmark dir mrk true)}
      val xy' = advance field dir xy
    in
      case src
       of (#".", #"@") => (((xy, dir), Return, (xy, dir))::acc, field)
        | (#".", #"#") =>
            let val xy'' = advance field dir xy' in
              parse (xy'', dir) (((xy, dir), Nop, (xy'', dir))::acc) field
            end
        | (#".", dirc) =>
            let
              val dir' = ch2dir dirc dir
              val xy' = advance field dir' xy
            in
              parse (xy', dir') (((xy, dir), Nop, (xy', dir'))::acc) field
            end
        | (#" ", #" ") =>
            parse (xy', dir) (((xy, dir), Nop, (xy', dir))::acc) field
        (* TODO: More movement commands *)
        | (#"?", #"@") =>
            parse (xy', dir) (((xy, dir), IfRet, (xy', dir))::acc) field
        | (#"?", dirc) =>
            (*if String.isSubstring (String.str dirc) "{}^v/\\"*)
                          let
                            val branchdir = if dirc = #"#" then dir
                                            else ch2dir dirc dir
                              handle Match => raise SyntaxE
                                ("Not a valid condition: " ^ cmd2str src)
                            val branchxy = advance field branchdir xy
                            val branchxy = if dirc = #"#" then
                                             advance field branchdir branchxy
                                           else branchxy
                            val (condbranchacc, field') =
                              parse (branchxy, branchdir) acc field
                          in
                            parse (xy', dir)
                              ( ( (xy, dir)
                                , If (branchxy, branchdir)
                                , (xy', dir)
                                )::condbranchacc ) field'
                          end
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

(***** SECOND IR *****)

(* TODO *)

(***** PARSING DECLARATIONS *****)

type Function = (char * char) * AST * string list
type Program = Function list

fun parse_lines (lines : string list) : Program =
  let
    fun lines2fn name lines pragmas : Function =
      ( name
      , case parse ((0,0), RT) [] (to_ast (Vector.fromList (List.rev lines)))
          of (parsed_cmds_in_list : AST, unparsed : AST_Field)
                  => parsed_cmds_in_list
      , pragmas
      )
    val split = String.tokens (fn c => c = #" " orelse c = #"\t")
    fun f (l, state) : ( (char * char) (* function being parsed *)
                       * string list (* function's lines built up backwards *)
                       * Function list (* all functions parsed (backwards) *)
                       * string list (* compiler directives for
                                        the current function *)
                       ) =
          case state of ( name : char * char
                        , lines : string list
                        , functions : Function list
                        , pragmas : string list
                        ) =>
          if String.size l >= 2 andalso String.sub(l, 0) = #" " then
            case String.sub(l, 1)
             of #"@" =>
                ( (String.sub(l, 2), String.sub(l, 3))
                    (* start a new function body named ^ *)
                    handle Subscript =>
                      raise SyntaxE "' @' requires function name"
                , [] (* with no lines *)
                , (lines2fn name lines pragmas) :: functions
                    (* and parse the last one *)
                , split (String.extract(l, 4, NONE))
                    (* with optional pragmas *)
                )
              | #"#" => state (* a comment *)
              | #"~" => ( name, lines, functions
                        , split (String.extract(l,2,NONE)) )
              | other => (name, l::lines, functions, pragmas)
          else
            (name, l::lines, functions, pragmas)
    val parsed = foldl f ((#" ",#" "), [], [], []) lines
  in
    #3 (f (" @cd", parsed))
    (* pretend we're defining a new function so f
       will parse the last one defined *)
  end

(***** FILE INPUT *****)

val read_lines = String.fields (fn c=>c = #"\n") o TextIO.inputAll

(***** CODE GENERATION *****)

fun function_name (#" ", #" ") = "main"
  | function_name (   a,    b) = "w_f_" ^ String.implode [a,b]

fun valid_num (a,b) = Char.isDigit a andalso Char.isDigit b
fun valid_name (a,b) = Char.isAlphaNum a
               andalso (not (valid_num (a,b)))
               andalso Char.isAlphaNum b

