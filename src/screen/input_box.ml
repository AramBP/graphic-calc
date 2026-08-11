type t = {
  box : Raylib.Rectangle.t ;
  font_size : float ;
  input_fields : input_field array
}
and input_field = {
  content : string ; 
  output : string ;
  field : Raylib.Rectangle.t ; 
  mouse_on_text : bool ; 
  is_init : bool ;
}

let max_chars input_box = (Raylib.Rectangle.width input_box.box |> Int.of_float) / (Int.of_float (input_box.font_size /. 2.)) 

let init () =
  let x, y = (10., 10.) in
  let width = Raylib.get_screen_width () / 4 |> Float.of_int in
  let height = (Raylib.get_screen_height () |> Float.of_int) -. 2. *. y in
  
  let input_field_height = 60. in
  let n_fields = height /. input_field_height |> Int.of_float in  
  let white_space = height -. (input_field_height *. Float.of_int n_fields) in

  {
    box = Raylib.Rectangle.create x y (width +. 4.) height ;
    font_size = 16. ;
    input_fields = Array.init n_fields (fun i -> 
      { 
        content = "Input..." ; 
        output = "Output..." ;
        field = Raylib.Rectangle.create (x +. 2.) (y +. white_space /. 2. +. input_field_height *. Float.of_int i) width (input_field_height -. 2.) ; 
        mouse_on_text = false ;
        is_init = true 
      }
    )
  }

let draw input_box font =
  Raylib.draw_rectangle_rec input_box.box Raylib.Color.white;
  Raylib.draw_rectangle_lines_ex input_box.box 1. Raylib.Color.gray;

  Array.iter 
    (fun input_field -> 
      Raylib.draw_rectangle_rec input_field.field Raylib.Color.white; 

      let lines_color = if input_field.mouse_on_text then Raylib.Color.red else Raylib.Color.black in
      let output_rectangle = Raylib.Rectangle.(create 
        (x input_field.field) 
        (y input_field.field +. height input_field.field /. 2.)
        (width input_field.field)
        (height input_field.field /. 2.))
      in
      Raylib.draw_rectangle_lines_ex output_rectangle 1. Raylib.Color.gray;
      Raylib.draw_rectangle_lines_ex input_field.field 1. lines_color;

      let text_color = if input_field.is_init then Raylib.Color.gray else Raylib.Color.black in 
      Raylib.draw_text_ex 
        font
        input_field.content 
        Raylib.(Vector2.create (5. +. Rectangle.x input_field.field) (Rectangle.(height input_field.field /. 4. +. y input_field.field) -. input_box.font_size /. 2.)) 
        input_box.font_size 2. text_color;
      
      let output = String.drop_last ((2 + String.length input_field.output) - (max_chars input_box)) input_field.output in
      let output = if String.equal output input_field.output then output else (String.drop_last 3 output) ^ "..." in
      Raylib.draw_text_ex
        font
        ("> " ^ output)
        Raylib.(Vector2.create (5. +. Rectangle.x input_field.field) (Rectangle.(height input_field.field *. (3./.4.) +. y input_field.field) -. input_box.font_size /. 2.))
        input_box.font_size 2. text_color
    )  
    input_box.input_fields

let update input_box env =
  let hover = ref false in
  let max_chars = max_chars input_box in
  let env, input_fields = Array.fold_left_map
    (fun env_acc input_field ->
      if Raylib.(check_collision_point_rec (get_mouse_position ()) input_field.field) 
      then (
        if not !hover then hover := true;
        let rec add_content content key_uchar =
          let key, key_num = (Uchar.to_char key_uchar, Uchar.to_int key_uchar) in
          if key_num <= 0 then content
          else (    
            let next_char = if (key_num >= 32 && key_num <= 125) then String.of_char key else "" in
            if input_field.is_init then next_char
            else
              (* Magic number 5 for the max characteris in the input field, it overflows otherwise *)
              if 5 + String.length content < max_chars then add_content (content ^ next_char) (Raylib.get_char_pressed ()) else content
          )     
        in
       
        let content = add_content (input_field.content) (Raylib.get_char_pressed ()) in 
        let content =
          if Raylib.is_key_pressed Raylib.Key.Backspace && input_field.is_init then ""
          else if Raylib.is_key_pressed Raylib.Key.Backspace then String.drop_last 1 content
          else content
        in

        let is_init = 
          if not (String.equal input_field.content content) && input_field.is_init then false
          else input_field.is_init
        in
        
        let output, env_acc = 
          if is_init then (input_field.output, env_acc)
          else Calculator.interp env_acc content 
        in

        (env_acc, { input_field with content = content ; output = output ; mouse_on_text = true ; is_init = is_init }))
      else (env_acc, { input_field with mouse_on_text = false })
    ) env input_box.input_fields
  in
  if !hover then Raylib.set_mouse_cursor Raylib.MouseCursor.Ibeam else Raylib.set_mouse_cursor Raylib.MouseCursor.Default;
  ({ input_box with input_fields = input_fields }, env)

