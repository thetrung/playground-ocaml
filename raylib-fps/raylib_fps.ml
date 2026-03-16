open Raylib

let setup () = 
  init_window 800 600 "Raylib FPS";
  set_target_fps 60

let loop () = 
  while not (window_should_close()) do
    match window_should_close() with
    | true -> close_window ();
    | false -> 
        begin_drawing();
        clear_background Color.raywhite;
        end_drawing();
    done
  
let () = setup () |> loop
