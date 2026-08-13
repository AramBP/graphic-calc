type t = {origin : Raylib.Vector2.t; 
  camera : Raylib.Camera2D.t; 
  pixels_per_unit : float;
  unit_multiplier : float}

module Func_Info = Map.Make(String)
type func_info_t = info Func_Info.t
and info = { color : Raylib.Color.t ; sampled_points : Raylib.Vector2.t list ; body : string }

let init_func_info () = Func_Info.empty

let colors = Raylib.Color.[| blue ; yellow ; green ; orange ; green ; red ; purple |]

let init () =
  let offset = Raylib.Vector2.zero () in
  let target = Raylib.Vector2.zero () in
  let rotation = 0. in
  let zoom = 1. in
  let camera = Raylib.Camera2D.create offset target rotation zoom in

  let origin = Raylib.Vector2.create
    (Float.of_int (Raylib.get_screen_width ()) /. 2.)
    (Float.of_int (Raylib.get_screen_height ()) /. 2.)
  in
  {origin = origin ; camera = camera ; pixels_per_unit = 100. ; unit_multiplier = 1.0}

let coord_to_world_pos (grid : t) (coord : Raylib.Vector2.t) =
  let coord = Raylib.Vector2.(multiply coord (create (1.) (-1.))) in
  let coord_pix = Raylib.Vector2.scale coord grid.pixels_per_unit in 
  let coord_scaled = Raylib.Vector2.scale coord_pix (1. /. grid.unit_multiplier) in
  let coord_origin = Raylib.Vector2.add coord_scaled grid.origin in
  coord_origin
 
let world_pos_to_coord (grid : t) (world_pos : Raylib.Vector2.t) =
  let world_pos_origin = Raylib.Vector2.(add world_pos (scale grid.origin (-1.))) in 
  let coord_inv_y = Raylib.Vector2.scale world_pos_origin (1. /. grid.pixels_per_unit) in
  let coord = Raylib.Vector2.(multiply coord_inv_y (create (1.) (-1.))) in
  Raylib.Vector2.scale coord grid.unit_multiplier

let x_axis_range (grid : t) =
  let x_axis_min_world = Raylib.get_screen_to_world_2d (Raylib.Vector2.(create 0. 0.)) grid.camera in
  let x_axis_max_world = Raylib.get_screen_to_world_2d (Raylib.Vector2.(create (Float.of_int (Raylib.get_screen_width ())) 0.)) grid.camera in

  let x_axis_min = Raylib.Vector2.x (world_pos_to_coord grid x_axis_min_world) in
  let x_axis_max = Raylib.Vector2.x (world_pos_to_coord grid x_axis_max_world) in  
  (x_axis_min, x_axis_max)

let weyl_list (range : (float * float)) =
  let n = 2000 in
  let (a, b) = range in
  let step_size = Float.exp (-1.) in
  let weyl = 
    List.(sort Float.compare (init n (
      fun i -> 
        let k = Float.of_int (i+1) in
        a +. (b -. a) *. (Float.rem (k *. step_size) 1.0)
    ))) 
  in
  weyl
    
let sample_func_points (weyl_x: float list) (func_name : string) (env : Calculator.env_t) =
  let rec get_points = function
    | x::xs -> 
        let v = Calculator.call_func env func_name x in
        if Option.is_none v then get_points xs
        else begin
          let y = Option.get v in
          if Float.abs(y) > 1000. then get_points xs
          else (x,y)::(get_points xs) 
        end
    | [] -> []
  in
  get_points weyl_x
   
let draw_func (grid : t) (func_name : string) (func_info_map : func_info_t) (env : Calculator.env_t) =
  match Func_Info.find_opt func_name func_info_map with
  | None -> ()
  | Some { color ; sampled_points ; body } ->
    let world_thick = 2. /. Raylib.Camera2D.zoom grid.camera in
    let draw_fun_lines points = 
      let rec aux prev_point = function    
      | point::tl -> 
          if Raylib.Vector2.distance prev_point point < (1. *. grid.unit_multiplier *. grid.pixels_per_unit) then Raylib.draw_line_ex prev_point point world_thick color;
          aux point tl
      | [] -> ()
      in
      aux (List.hd points) (List.tl points);
    in
    draw_fun_lines sampled_points

let draw (grid : t) (grid_prev : t) (func_info_map : func_info_t) (env : Calculator.env_t) font =
  let origin = grid.origin in
  let camera = grid.camera in
  let pixels_per_unit = grid.pixels_per_unit in

  let draw_vert_lines distance_from_origin thick color =
    let width = Float.of_int (Raylib.get_screen_width ()) in
    let y = Raylib.Vector2.y origin +. distance_from_origin in
    let p = Raylib.Vector2.(create (x origin) (y origin +. distance_from_origin)) in
    let p1 = Raylib.(get_screen_to_world_2d (Vector2.create 0. y) camera) in
    Raylib.Vector2.set_y p1 y;

    let p2 = Raylib.(get_screen_to_world_2d (Vector2.create width y) camera) in
    Raylib.Vector2.set_y p2 y;

    let world_thick = thick /. Raylib.Camera2D.zoom grid.camera in
    Raylib.draw_line_ex p p1 world_thick color;
    Raylib.draw_line_ex p p2 world_thick color
  in

  let draw_hor_lines distance_from_origin thick color =
    let height = Float.of_int (Raylib.get_screen_height ()) in
    let x = Raylib.Vector2.x origin +. distance_from_origin in
    let p = Raylib.Vector2.(create (x origin +. distance_from_origin) (y origin)) in
    let p1 = Raylib.(get_screen_to_world_2d (Vector2.create x 0.) camera) in
    Raylib.Vector2.set_x p1 x;

    let p2 = Raylib.(get_screen_to_world_2d (Vector2.create x height) camera) in
    Raylib.Vector2.set_x p2 x;

    let world_thick = thick /. Raylib.Camera2D.zoom grid.camera in
    Raylib.draw_line_ex p p1 world_thick color;
    Raylib.draw_line_ex p p2 world_thick color
  in

  let n_lines = Int.of_float (10000. /. pixels_per_unit) in

  for i = 0 to n_lines*5 do
    let distance_between_lines = pixels_per_unit /. 5. in
    let incr = distance_between_lines *. Float.of_int i in
    draw_hor_lines (distance_between_lines +. incr) 1. Raylib.Color.lightgray;
    draw_hor_lines (-1. *. (distance_between_lines +. incr)) 1. Raylib.Color.lightgray;
    draw_vert_lines (distance_between_lines +. incr) 1. Raylib.Color.lightgray;
    draw_vert_lines (-1. *. (distance_between_lines +. incr)) 1. Raylib.Color.lightgray
  done;

  let font_size = 16. /. Raylib.Camera2D.zoom grid.camera in
  for i = 0 to n_lines do
    let incr = pixels_per_unit *. Float.of_int i in
    let pos = pixels_per_unit +. incr in
    let neg = -1. *. pos in

    draw_hor_lines pos 1. Raylib.Color.gray;
    draw_hor_lines neg 1. Raylib.Color.gray;
    draw_vert_lines pos 1. Raylib.Color.gray;
    draw_vert_lines neg 1. Raylib.Color.gray;

    let round n = Float.(n *. 100. |> round |> fun x -> x /. 100.) in
    let num = grid.unit_multiplier *. pos /. pixels_per_unit |> round in 
    let vert_num_y = Raylib.Vector2.y origin +. 5. in
    Raylib.draw_text_ex 
      font
      (-1. *. num |> Float.to_string) 
      Raylib.Vector2.(create (neg +. x origin) vert_num_y) 
      font_size 2. Raylib.Color.darkgray;
    Raylib.draw_text_ex
      font
      (num |> Float.to_string)
      Raylib.Vector2.(create (pos +. x origin) vert_num_y) 
      font_size 2. Raylib.Color.darkgray;

    let hor_num_x = Raylib.Vector2.x origin -. 20. in
    Raylib.draw_text_ex
      font
      (-1. *. num |> Float.to_string)
      Raylib.Vector2.(create (hor_num_x -. 5.) (pos +. y origin))
      font_size 2. Raylib.Color.darkgray;
    Raylib.draw_text_ex
      font
      (num |> Float.to_string)
      Raylib.Vector2.(create hor_num_x (neg +. y origin))
      font_size 2. Raylib.Color.darkgray
  done;

  draw_vert_lines 0. 2. Raylib.Color.black;
  draw_hor_lines 0. 2. Raylib.Color.black;
  Raylib.draw_text_ex
    font
    "0"
    Raylib.Vector2.(create (x origin -. 10.) (y origin +. 5.))
    font_size 2. Raylib.Color.darkgray;

  let func_names = List.map (fun (k, v) -> k) (Calculator.Env.bindings env.funcs) in
  List.iter (fun func_name -> draw_func grid func_name func_info_map env) func_names

let update_func (grid : t) (grid_prev : t) (weyl_x) (func_info_map : func_info_t) (func_name : string) (env : Calculator.env_t) =
  let (a_prev, b_prev) = x_axis_range grid_prev in
  let (a_new, b_new) = x_axis_range grid in
 
  let func_info_opt = Func_Info.find_opt func_name func_info_map in 
  let func_exists = Func_Info.mem func_name func_info_map in 

  let sample () =
     let coords = sample_func_points weyl_x func_name env in
     List.map (fun (x, y) -> coord_to_world_pos grid (Raylib.Vector2.create x y)) coords
  in
  
  let func_expr = match (Calculator.Env.find func_name env.funcs) with | (_, e) -> e  in
  let def_body = Calculator.expr_to_string func_expr env in
  
  let sampled_points = 
    if func_exists then
      let func_info = Option.get func_info_opt in
      if String.equal func_info.body def_body then 
        if a_prev = a_new && b_prev = b_new then func_info.sampled_points
        else sample ()
      else sample ()
    else sample ()
  in
  
  let color = 
    if func_exists then (Option.get func_info_opt).color
    else 
      let n_colors_in_use = List.length (Func_Info.to_list func_info_map) in
      if n_colors_in_use > (Array.length colors) - 1 then Raylib.Color.black
      else colors.(n_colors_in_use)
  in

  Func_Info.add func_name { color = color ; sampled_points = sampled_points ; body = def_body } func_info_map

let update_func_info (grid : t) (grid_prev : t) (func_info_map : func_info_t) (env : Calculator.env_t) =
  let func_names = List.map (fun (k, v) -> k) (Calculator.Env.bindings env.funcs) in
  let weyl = weyl_list (x_axis_range grid) in
  List.fold_left (fun acc func_name -> update_func grid grid_prev weyl acc func_name env) func_info_map func_names

let update (grid: t) =
  let clamp x mi ma = Float.(max mi (min ma x)) in
  let camera = grid.camera in

  (* Zoom in/out using mouse wheel *)
  let wheel = Raylib.get_mouse_wheel_move () in
  let offset, target = 
    if not (Float.equal wheel 0.) then
      let mouse_pos = Raylib.get_mouse_position () in
      (mouse_pos, Raylib.get_screen_to_world_2d mouse_pos camera)
    else 
      (Raylib.Camera2D.offset camera, Raylib.Camera2D.target camera)
  in
  let zoom = clamp (Raylib.Camera2D.zoom camera +. wheel *. 0.035 ) 0.125 10.0 in

  (* Drag camera by holding left click *)
  let target = 
    if Raylib.is_mouse_button_down Raylib.MouseButton.Left then
      let delta = Raylib.get_mouse_delta () in
      Raylib.(get_screen_to_world_2d (Vector2.add (Camera2D.offset camera) delta) camera)
    else target
  in
  
  let zoom, unit_multiplier =
    if zoom >= 2. then (zoom /. 2., grid.unit_multiplier /. 2.)
    else if zoom <= 0.5 then (zoom *. 2., grid.unit_multiplier *. 2.)
    else (zoom, grid.unit_multiplier)
  in

  let camera' = Raylib.Camera2D.create offset target (Raylib.Camera2D.rotation camera) zoom in
  {grid with camera = camera' ; unit_multiplier = unit_multiplier}

