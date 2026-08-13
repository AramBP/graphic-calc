module State = struct
  type t = { 
    grid      : Screen.Grid.t ;
    grid_prev : Screen.Grid.t ;
    input_box : Screen.Input_box.t ;
    env       : Calculator.env_t ;
    func_info : Screen.Grid.func_info_t ;
    font : Raylib.Font.t 
  }

  let init () = 
    let exe_dir = Filename.dirname Sys.executable_name in
    let path = Filename.concat exe_dir "Roboto-Regular.ttf" in    
    let font = Raylib.load_font_ex path 32 None in

    Raylib.set_texture_filter (Raylib.Font.texture font) Raylib.TextureFilter.Bilinear;
    {
      grid      = Screen.Grid.init () ;
      grid_prev = Screen.Grid.init() ;
      input_box = Screen.Input_box.init () ;
      env       = Calculator.init_env () ;
      func_info = Screen.Grid.init_func_info ();
      font      = font   
    }

  let next state =
    let grid = Screen.Grid.update state.grid in
    let grid_prev = state.grid in
    let input_box, env = Screen.Input_box.update state.input_box state.env in
    let func_info = Screen.Grid.update_func_info grid grid_prev state.func_info env in
    { state with grid = grid ; grid_prev = grid_prev ; input_box = input_box ; env = env ; func_info = func_info }

  let draw state =
    Raylib.begin_drawing ();
    Raylib.clear_background Raylib.Color.raywhite;
    
    Raylib.begin_mode_2d state.grid.camera;
    Screen.Grid.draw state.grid state.grid_prev state.func_info state.env state.font;
    Raylib.end_mode_2d ();
    Screen.Input_box.draw state.input_box state.font;
    Raylib.end_drawing ()

end 

let setup () =
  let window_height = 450 in
  let window_width = 800 in
  let window_name = "2D Grid" in 
  Raylib.init_window window_width window_height window_name;
  Raylib.set_target_fps 60;
  State.init ()  

let rec loop (state : State.t) =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false -> 
    let next = State.next state in
    State.draw next;
    loop next

let () = setup () |> loop
