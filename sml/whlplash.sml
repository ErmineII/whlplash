
datatype Value = N of int | Arr of Value vector
type Stack = Value list

datatype Dir = UP | DOWN | LEFT | RIGHT
type Field = string vector

type State = {s: Stack, x: int, y: int, d: Dir, f: Field}
exception WhlplashE of string

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

fun strvec_coords x y f = String.sub(Vector.sub(f, y), x)
fun at_ip {s, x, y, d, f} =
  (strvec_coords (x*2) y f, strvec_coords (x*2 + 1) y f)

fun do_cell cell state =
  let val {s,x,y,d,f} = state in
    case cell
    of (#" ", #" ") => state
     | (#"+", #"+") =>
       let val (N p)::(N q)::s' = s in
         {   s=(N (p+q))::s',   x=x, y=y, d=d, f=f }
       end
     | (#".", #"#") => advance state
       (* ... *)
     | ( n0 ,  n1 ) => if Char.isDigit n0 andalso Char.isDigit n1
                       then
                         let
                           fun digit2int c = (ord c) - (ord #"0")
                           val num = digit2int n0 * 10 + digit2int n1
                         in
                           {   s=(N num)::s,   x=x, y=y, d=d, f=f }
                         end
                       else raise WhlplashE "invalid command"
  end
fun step state = advance (do_cell (at_ip state) state)

(* debug *)
val sample_state =
  { s= [N 0,N 0,N 0]
  , x= 0, y= 0
  , d= RIGHT
  , f= #[ "  0305++"
        , ".v.<"
        , ".>.@"
        ]
  }

