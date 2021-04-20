
datatype Value = N of int | Arr of Value vector
type Stack = Value list

datatype Dir = UP | DOWN | LEFT | RIGHT
type Field = string vector

type State = {s: Stack, x: int, y: int, d: Dir, f: Field}

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
    { x=(x+dx) mod (size (Vector.sub(f, y)) / 2)
    , y=y+dy
    , d=d, f=f, s=s
    }
  end

fun at_ip {s, x, y, d, f} =
  (strvec_coords (x*2) y f, strvec_coords (x*2 + 1) y f)
fun strvec_coords x y f = String.sub(Vector.sub(f, y), x)

(* debug *)
val sample_state =
  { s= [N 0,N 0,N 0]
  , x= 0, y= 0
  , d= RIGHT
  , f= #[ "  .v"
        , ".v.<"
        , ".>.@"
        ]
  }

