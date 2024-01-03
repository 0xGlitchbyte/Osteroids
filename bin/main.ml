open Minttea

let ship = "^"
let floor = " "

let init _ = Command.Seq [Enter_alt_screen; Command.Hide_cursor]

module Tilemap = struct
  let w = 80
  let h = 40

  let bg = 
    let bg = Array.make_matrix h w ship in  
    Array.set bg 0 (Array.make w floor);
    Array.set bg (h-1) (Array.make w floor);
    for i = 0 to h-1 do 
      bg.(i).(0) <- floor;
      bg.(i).(w-1) <- floor
    done;
    bg

  let overlay overrides = 
    let base = Array.make_matrix h w "" in 
    List.iter (fun(icon,(x,y)) -> base.(y).(x) <- icon) overrides;
    base

  let view map = 
    let buf = Buffer.create 1024 in 
    let fmt = Format.formatter_of_buffer buf in 
    Format.fprintf fmt "\r\n";
    for i = 0 to Array.length bg - 1 do 
      for j = 0 to Array.length bg.(i) - 1 do 
        let tile = map.(i).(j) in 
        let tile = if tile = "" then bg.(i).(j) else tile in 
        Format.fprintf fmt "%s" tile
      done;
      Format.fprintf fmt "\r\n"
    done;
    Format.fprintf fmt "%!";
    Buffer.contents buf
end

type model = {
  screen : string array array;
  ship_pos : int * int;
}

let initial_model = 
{
  screen = Tilemap.overlay [];
  ship_pos = (12,7)
}


let update event model = 
  match event with 
  | Event.KeyDown (Key "q") -> (model, Command.Quit)
  | _ -> (model, Command.Noop)

let view _model=
  let hi = ship in Format.sprintf "%s" hi

let () = Minttea.app ~init ~update ~view () |> Minttea.start ~initial_model

