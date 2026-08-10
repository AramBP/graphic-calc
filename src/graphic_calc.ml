let setup () =
  let window_height = 450 in
  let window_width = 800 in
  let window_name = "2D Grid" in 
  Raylib.init_window window_width window_height window_name;
  Raylib.set_target_fps 60;
  Grid.init ()

let rec loop (grid: Grid.t) =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false -> 
    Raylib.begin_drawing ();
    let grid' = Grid.update_grid grid in
    Raylib.clear_background Raylib.Color.raywhite;
    Raylib.begin_mode_2d grid'.camera;
    Grid.draw_grid grid';
    Raylib.end_mode_2d ();

    Raylib.draw_text ("Zoom : " ^ (Raylib.Camera2D.zoom grid'.camera |> Float.to_string)) 20 50 20 Raylib.Color.darkgray;
    Raylib.end_drawing ();

    loop grid'

let () = setup () |> loop 
