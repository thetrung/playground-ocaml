(* contra_kernel.ml *)
open Raylib

type vec2 = {  x: float ref; y: float ref}

type kind = Player | Enemy | Bullet

type entity = {
  kind : kind;
  pos : vec2 ref;
  vel : vec2 ref;
  hp : int ref ;
  alive : bool ref;
}

let entities = Array.make 128 { kind = Player; !pos = {x=0.;y=0.}; !vel = {x=0.;y=0.}; !hp=0; !alive=false }
let mutable entity_count = 0

let spawn kind pos vel hp =
  if entity_count < Array.length entities then
    let e = { kind; pos; vel; hp; alive=true } in
    entities.(entity_count) <- e;
    entity_count <- entity_count + 1

let update_entity e =
  match e.kind with
  | Player ->
      if is_key_down Key.D then e.pos.x <- e.pos.x +. 2.;
      if is_key_down Key.A then e.pos.x <- e.pos.x -. 2.;
      if is_key_pressed Key.Space then
        spawn Bullet {x=e.pos.x +. 8.; y=e.pos.y} {x=4.; y=0.} 1
  | Enemy ->
      e.pos.x <- e.pos.x +. e.vel.x;
      if e.pos.x < 20. || e.pos.x > 380. then e.vel.x <- -.e.vel.x
  | Bullet ->
      e.pos.x <- e.pos.x +. e.vel.x;
      if e.pos.x > 400. then e.alive <- false

let update_all () =
  for i = 0 to entity_count - 1 do
    let e = entities.(i) in
    if e.alive then update_entity e
  done;
  (* remove dead *)
  let live = ref 0 in
  for i = 0 to entity_count - 1 do
    let e = entities.(i) in
    if e.alive then begin
      entities.(!live) <- e;
      incr live
    end
  done;
  entity_count <- !live

let draw_entity e =
  let color =
    match e.kind with
    | Player -> Color.create 0 200 255 255
    | Enemy -> Color.create 255 0 0 255
    | Bullet -> Color.create 255 255 0 255
  in
  draw_rectangle (int_of_float e.pos.x) (int_of_float e.pos.y) 8 8 color

let () =
  init_window 400 240 "Contra Kernel (OCaml)"
  |> ignore;
  set_target_fps 60;
  spawn Player {x=40.;y=120.} {x=0.;y=0.} 3;
  spawn Enemy {x=300.;y=120.} {x=(-1.);y=0.} 3;
  while not (window_should_close ()) do
    update_all ();
    begin_drawing ();
    clear_background Color.black;
    for i = 0 to entity_count - 1 do draw_entity entities.(i) done;
    end_drawing ();
  done;
  close_window ()
