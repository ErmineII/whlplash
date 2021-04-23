
load "Int";

datatype Value = N of int | Arr of Value vector
type Stack = Value list

datatype Dir = UP | DOWN | LEFT | RIGHT
type Field = string vector

type State = {s: Stack, x: int, y: int, d: Dir, f: Field}
exception WhlplashE of string

datatype Update = Change of State | Return of State

fun advance {s, x, y, d, f} =
  { s=s, d=d, f=f
  , x=case d of UP => x
              | DOWN => x
              | LEFT => x-1
              | RIGHT => x+1
  , y=case d of UP => y-1
              | DOWN => y+1
              | LEFT => y
              | RIGHT => y
  }

fun delta d = case d of UP    => ( 0,~1)
                      | DOWN  => ( 0, 1)
                      | LEFT  => (~1, 0)
                      | RIGHT => ( 1, 0)

fun advance {s, x, y, d, f} =
  let val (dx, dy) = delta d in
    { x=(x+dx) mod (size (Vector.sub(f, y)) div 2)
    , y=y+dy
    , d=d, f=f, s=s
    }
  end

fun at_ip {s, x, y, d, f} =
  let
    val row = Vector.sub(f, y)
  in
    String.substring(row, (x*2), 2) handle Subscript => "  "
  end

fun do_cell cell state =
  let
    fun bin_op operator {s, x, y, d, f} =
      case s
        of (N a)::(N b)::s' =>
            { s= (N (operator (a, b)))::s', x=x, y=y, d=d, f=f }
         | _ => raise WhlplashE "binary operator needs 2 numbers"
    fun un_op (operator : Value -> Value) {s, x, y, d, f} : State =
      case s of a::s' => {s=(operator a)::s', x=x, y=y, d=d, f=f }
              | [] => raise WhlplashE "unary operator needs an argument"
    fun change_direction dir {s, x, y, d, f} =
      { d=dir, s=s, x=x, y=y, f=f }
    fun cond_change_direction dir state =
      case #s state
        of (N 0)::s' => state
         |  anyval::s' =>
            { s=s', x= #x state, y= #y state, f= #f state, d=dir }
         |          [] => raise WhlplashE "conditional turn needs an argument"
    fun negate v = case v of N n => N (~n)
                           | any => raise WhlplashE "can only negate numbers"
    fun bool2num b = if b then 1 else 0
    val bool_not = N o bool2num o (fn v => v = N 0)
    val {s,x,y,d,f} = state
  in
    if cell = ".@" then
      Return (state)
    else
      Change
      ( case cell
        of "  " => state
         | ".#" => advance state
         | ".^" => change_direction UP    state
         | ".v" => change_direction DOWN  state
         | ".<" => change_direction LEFT  state
         | ".>" => change_direction RIGHT state
         | "++" => bin_op op + state
         | "+-" => bin_op op - state
         | "+*" => bin_op op * state
         | "+/" => bin_op op div state
         | "+!" => un_op negate state
         | "?=" => bin_op (bool2num o op =) state
         | "?<" => bin_op (bool2num o op <) state
         | "?>" => bin_op (bool2num o op >) state
         | "?[" => bin_op (bool2num o op <=) state
         | "?]" => bin_op (bool2num o op >=) state
         | "?!" => un_op bool_not state
         | "?^" => cond_change_direction UP    state
         | "?v" => cond_change_direction DOWN  state
         | "?{" => cond_change_direction LEFT  state
         | "?}" => cond_change_direction RIGHT state
         | any  => case Int.fromString any
                     of SOME num => { s=(N num)::s, x=x, y=y, d=d, f=f }
                      | NONE => (* todo: user-defined functions *)
                                raise WhlplashE "unknown function"
      )
  end

fun run_frame state =
  case do_cell (at_ip state) state
    of Change state' => run_frame (advance state')
     | Return value  => value

(* val run_program : string vector -> State *)
(* val split_fns : string vector ->
                     (char * char, string vector) <some_mapping_thing> *)

(*debug*)
val samples =
  [ { s= []
    , x= 0, y= 0
    , d= RIGHT
    , f= #[ ".#.v0305++.#"
          , ".v.<"
          , ".>.#.@.<"
          ]
    }
  , { s=[N 1]
    , x=0, y=0
    , d= RIGHT
    , f= #[ "?v.@"
          , ".@"
          ]
    }
  ]
