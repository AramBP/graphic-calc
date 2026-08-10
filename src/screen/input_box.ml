type t = {
  box : Raylib.Rectangle.t
}

let init () =
  let x, y = (10., 10.) in
  let width = Raylib.get_screen_width () / 4 |> Float.of_int in
  let height = (Raylib.get_screen_height () |> Float.of_int) -. 2. *. y in
  {
    box = Raylib.Rectangle.create x y width height
  }

let draw input_box =
  Raylib.draw_rectangle_rec input_box.box Raylib.Color.red

let update input_box = input_box

