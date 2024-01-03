open Minttea

let init _ = Command.Seq [Enter_alt_screen]

let initial_model = 3

let update event model = 
  match event with 
  | Event.KeyDown (Key "q") -> (model, Command.Quit)
  | _ -> (model, Command.Noop)

let view _model=
  let hi = "Hi" in Format.sprintf "%s" hi

let () = Minttea.app ~init ~update ~view () |> Minttea.start ~initial_model
