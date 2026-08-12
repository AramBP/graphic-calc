open Screen

let setup () =
  let window_height = 450 in
  let window_width = 800 in
  let window_name = "2D Grid" in 
  Raylib.init_window window_width window_height window_name;
  let font = Raylib.load_font_ex "assets/Roboto-Regular.ttf" 32 None in
  Raylib.set_texture_filter (Raylib.Font.texture font) Raylib.TextureFilter.Bilinear;
  Raylib.set_target_fps 60;
  
  (Grid.init (), Input_box.init (), Calculator.init_env (), Grid.init_func_info (), font)

let rec loop (grid: Grid.t) (input_box: Input_box.t) (env : Calculator.env_t) (func_info : Grid.func_info_t) font =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false -> 
    Raylib.begin_drawing ();
    let grid_prev = grid in
    let grid = Grid.update grid in
    let input_box, env = Input_box.update input_box env in
    Raylib.clear_background Raylib.Color.raywhite;

    Raylib.begin_mode_2d grid.camera;
    let sampled_points_map = Grid.draw grid grid_prev func_info env font in
    Raylib.end_mode_2d ();
    Input_box.draw input_box font;

    Raylib.end_drawing ();

    loop grid input_box env sampled_points_map font

let () = 
  let grid, input_box, env, sampled_points_map, font = setup () in
  loop grid input_box env sampled_points_map font
