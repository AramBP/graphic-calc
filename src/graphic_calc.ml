open Screen
let setup () =
  let window_height = 450 in
  let window_width = 800 in
  let window_name = "2D Grid" in 
  Raylib.init_window window_width window_height window_name;
  Raylib.set_target_fps 60;
  (Grid.init (), Input_box.init ())

let rec loop (grid: Grid.t) (input_box: Input_box.t) =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false -> 
    Raylib.begin_drawing ();
    let grid = Grid.update_grid grid in
    let input_box = Input_box.update input_box in
    Raylib.clear_background Raylib.Color.raywhite;

    Raylib.begin_mode_2d grid.camera;
    Grid.draw_grid grid;
    Raylib.end_mode_2d ();
    Input_box.draw input_box;

    Raylib.end_drawing ();

    loop grid input_box

let () = 
  let grid, input_box = setup () in
  loop grid input_box
