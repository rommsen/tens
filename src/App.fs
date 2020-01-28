module App

open Elmish
open Elmish.React
open Feliz

type Running =
  {
    Clicked : int list
    Numbers : int list
    Points : int
  }

type State =
  | Not_Started
  | Running of Running 
  | Finished of int

type Msg =
    | Tick of int
    | Started
    | Clicked of int 

let init() =
  Not_Started

let removeIndex index list =
  list 
  |> List.mapi (fun i element -> (i <> index, element))
  |> List.filter fst 
  |> List.map snd

let update (msg: Msg) (state: State): State =
    match state,msg with
    | Running state, Clicked index ->
        state.Numbers
        |> List.tryItem index 
        |> Option.map (fun number ->
            let newState =
              { state with 
                  Clicked = state.Clicked @ [number] 
                  Numbers = state.Numbers |> removeIndex index
              }

            if newState.Clicked |> List.sum = 10 then 
              Running { state with Clicked = [] ; Points = state.Points + 1 }
            elif newState.Clicked |> List.length = 3 then 
              Finished state.Points
            else
              Running state
          )
        |> Option.defaultValue (Running state)    

    | Running state, Tick number ->
        if state.Numbers |> List.length = 11 then 
          Finished state.Points
        else 
          Running { state with Numbers = state.Numbers @ [number] }

    | _ -> state     


let renderButton dispatch (number : int) index  =
  Html.button [
    prop.onClick (fun _ -> dispatch (Clicked index))
    prop.text number
  ]   

let renderRunning state dispatch =
  let buttons =
    state.Numbers 
    |> List.mapi (renderButton dispatch)

  Html.div buttons



let render (state: State) (dispatch: Msg -> unit) =
  match state with 
  | Not_Started ->
      Html.button [
        prop.onClick (fun _ -> dispatch Started)
        prop.text "Start"
      ]

  | Running state ->
      renderRunning state dispatch

  | Finished points ->
      Html.span points    
  

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run