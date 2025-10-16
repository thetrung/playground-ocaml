let setup () =
  Raylib.init_window 800 250 "raylib / GLB Loader on Ocaml";
  Raylib.set_target_fps 60

let asset_path (filename: string) = 
  Raylib.get_application_directory() ^ filename

let carray_char_to_string (char_arr: char Ctypes_static.carray) : string =
  let ptr = Ctypes.CArray.start char_arr in 
  Ctypes.string_from_ptr ptr ~length:32

let camera_setup () =
  let open Raylib in
  Camera.create 
    (Vector3.create 6.0 6.0 6.0) (* camera position *)
    (Vector3.create 0.0 2.0 0.0) (* looking at point *)
    (Vector3.create 0.0 1.0 0.0) (* up vector *)
    45.0 (* FOV *)
    CameraProjection.Perspective

let loop () =
  let open Raylib in


  (* Load Asset + Config *)
  let model_path = asset_path "robot.glb" in 
  let model = load_model model_path in
  let anims = load_model_animations model_path in (* some fn are much different from Raylib/C *)
  let anim_count = Ctypes.CArray.length anims in
  let anim_index, anim_current_frame = (ref 0, ref 0) in (* Ocaml variable is immutable by default *)
  
  (* Camera *)
  let camera = camera_setup() in
  let position = (Vector3.create 0.0 0.0 0.0) in 


  (* Main Loop  *)
  while true do
    match window_should_close() with 
    | true -> 
        unload_model model;
        close_window();
        
    | false -> 
        update_camera (addr camera) CameraMode.Orbital;  (* Raylib.addr convert ctype-> ctype ptr *)

        (* Change Animation by Clicks *)
        anim_index := (* Re-Assign value with := & !var *)
          if is_mouse_button_pressed MouseButton.Right then  (!anim_index + 1) mod anim_count
          else if is_mouse_button_pressed MouseButton.Left then  (!anim_index + anim_count - 1) mod anim_count
          else !anim_index;
        
        (* Update mutable Animation Frame *)
        let anim = Ctypes.CArray.get anims !anim_index in
        let anim_name = carray_char_to_string (ModelAnimation.name anim) in
        anim_current_frame := (!anim_current_frame + 1) mod (ModelAnimation.frame_count anim);
        update_model_animation model anim !anim_current_frame;
        
        (* Drawing *)
        begin_drawing ();
        clear_background Color.raywhite;
          begin_mode_3d camera;
            draw_model model position 1.0 Color.white;
            draw_grid 10 1.0;
          end_mode_3d ();
          draw_text ("animation: " ^ anim_name) 10 10 20 Color.black;
        end_drawing ();
    done

let () = setup () |> loop