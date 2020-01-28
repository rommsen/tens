module App

open Elmish
open Elmish.React
open Feliz
open Fable.Core


[<Emit("setInterval($0, $1)")>]
let setInterval (f: unit -> unit) (n: int) : int = jsNative

[<Emit("clearInterval($0)")>]
let clearInterval (n: int) : unit = jsNative

let random = new System.Random()




type Running =
  {
    Clicked : int list
    Numbers : int list
    Points : int
    TickId : int
  }

type State =
  | Not_Started
  | Running of Running 
  | Finished of int

type Msg =
    | Tick of int
    | StartTicking of int
    | Started
    | Clicked of int 

let startTicking = 
  let start dispatch =
    let tickId = setInterval (fun () -> dispatch (Tick (random.Next(1,9)))) 1000
    dispatch (StartTicking tickId)

  Cmd.ofSub start

let stopTicking id = 
  let stop _ =
    clearInterval id
    ()

  Cmd.ofSub stop



let init() =
  Not_Started,Cmd.none

let removeIndex index list =
  list 
  |> List.mapi (fun i element -> (i <> index, element))
  |> List.filter fst 
  |> List.map snd

let update (msg: Msg) (state: State) =
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

            printfn "index %i" index            
            printfn "index %i" index            

            if newState.Clicked |> List.sum = 10 then 
              Running { newState with Clicked = [] ; Points = state.Points + 1 },Cmd.none
            elif newState.Clicked |> List.length = 3 then 
              Finished state.Points, stopTicking state.TickId
            else
              Running newState, Cmd.none
          )
        |> Option.defaultValue (Running state,Cmd.none)    

    | Running state, Tick number ->
        if state.Numbers |> List.length = 11 then 
          Finished state.Points,stopTicking state.TickId
        else 
          Running { state with Numbers = state.Numbers @ [number] },Cmd.none


    | Not_Started, Started ->
        state, startTicking

    | Not_Started, StartTicking tickId ->
        ({
          Clicked = []
          Numbers = []
          Points = 0
          TickId = tickId
        }
        |> Running), Cmd.none              

    | _ -> state,Cmd.none     


let renderButton dispatch index (number : int)   =
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
  

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run