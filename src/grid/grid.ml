type t = {origin : Raylib.Vector2.t; 
  camera : Raylib.Camera2D.t; 
  pixels_per_unit : float;
  unit_multiplier : float}

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

let draw_grid (grid: t) =
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
  (* Render the lines of the grid *)
  for i = 0 to n_lines*5 do
    let distance_between_lines = pixels_per_unit /. 5. in
    let incr = distance_between_lines *. Float.of_int i in
    draw_hor_lines (distance_between_lines +. incr) 1. Raylib.Color.lightgray;
    draw_hor_lines (-1. *. (distance_between_lines +. incr)) 1. Raylib.Color.lightgray;
    draw_vert_lines (distance_between_lines +. incr) 1. Raylib.Color.lightgray;
    draw_vert_lines (-1. *. (distance_between_lines +. incr)) 1. Raylib.Color.lightgray
  done;

  let font_size = 10. /. Raylib.Camera2D.zoom grid.camera |> Float.to_int in
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
    let vert_num_y = Raylib.Vector2.y origin +. 5. |> Float.to_int in
    Raylib.draw_text 
      (-1. *. num |> Float.to_string) 
      (Float.to_int (neg +. Raylib.Vector2.x origin)) 
      vert_num_y font_size Raylib.Color.darkgray;
    Raylib.draw_text 
      (num |> Float.to_string)
      (Float.to_int (pos +. Raylib.Vector2.x origin))
      vert_num_y font_size Raylib.Color.darkgray;

    let hor_num_x = Raylib.Vector2.x origin -. 20. |> Float.to_int in
    Raylib.draw_text
      (-1. *. num |> Float.to_string)
      (hor_num_x - 5)
      (Float.to_int (pos +. Raylib.Vector2.y origin))
      font_size Raylib.Color.darkgray;
    Raylib.draw_text
      (num |> Float.to_string)
      hor_num_x 
      (Float.to_int (neg +. Raylib.Vector2.y origin))
      font_size Raylib.Color.darkgray
      
  done;

  draw_vert_lines 0. 2. Raylib.Color.black;
  draw_hor_lines 0. 2. Raylib.Color.black;
  Raylib.draw_text 
    "0"
    (Raylib.Vector2.x origin -. 10. |> Float.to_int)
    (Raylib.Vector2.y origin +. 5. |> Float.to_int)
    font_size 
    Raylib.Color.darkgray


let update_grid (grid: t) =
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

