open Raylib

let setup () = 
  init_window 800 600 "Raylib FPS";
  set_target_fps 60

let camera_setup () = 
  Camera.create
    (Vector3.create 6.0 6.0 6.0) (* camera position *)
    (Vector3.create 0.0 2.0 0.0) (*looking at *)
    (Vector3.create 0.0 1.0 0.0) (*up vector*)
    45.0
    CameraProjection.Perspective

let loop () = 
  let camera = camera_setup () in
  while not (window_should_close()) do
    match window_should_close() with
    | true -> close_window ();
    | false -> 
        update_camera (addr camera) CameraMode.Orbital;
        begin_drawing();
        clear_background Color.raywhite;
          begin_mode_3d camera;
            (* something *)
            draw_grid 10 1.0;
            draw_cube (Vector3.create 0.0 0.0 0.0) 1.0 1.0 1.0 Color.red;
          end_mode_3d ();
        end_drawing();
    done
  
let () = setup () |> loop
