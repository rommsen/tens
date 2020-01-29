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
  | GameStarted of int
  | Start
  | Restart
  | Clicked of int 

let startTicking = 
  let start dispatch =
    let tickId = setInterval (fun () -> dispatch (Tick (random.Next(1,9)))) 1000
    dispatch (GameStarted tickId)

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

let (|ReachedExactlyTen|NotTenAndClickNumberExceeded|StillGood|) clicked =
  if clicked |> List.sum = 10 then 
    ReachedExactlyTen
  elif clicked |> List.length = 3 then 
    NotTenAndClickNumberExceeded
  else 
    StillGood

let (|MaxNumbersExceeded|WithinNumberLimit|) numbers =
  if numbers |> List.length = 11 then 
    MaxNumbersExceeded
  else
    WithinNumberLimit  



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

            match state.Clicked with 
            | ReachedExactlyTen ->  
                Running { newState with Clicked = [] ; Points = state.Points + 1 },Cmd.none

            | NotTenAndClickNumberExceeded ->
                Finished state.Points, stopTicking state.TickId

            | StillGood ->    
                Running newState, Cmd.none                       
          )
        |> Option.defaultValue (Running state,Cmd.none)    

    | Running state, Tick number ->
        match state.Numbers with 
        | MaxNumbersExceeded ->
            Finished state.Points,stopTicking state.TickId

        | WithinNumberLimit ->
            Running { state with Numbers = state.Numbers @ [number] },Cmd.none


    | Not_Started, Start ->
        state, startTicking

    | Finished _, Restart ->
        Not_Started, startTicking

    | Not_Started, GameStarted tickId ->
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
    prop.style [ style.padding 20 ; style.fontSize 20 ]
    prop.onClick (fun _ -> dispatch (Clicked index))
    prop.text number
  ]   

let renderRunning state dispatch =
  let buttons =
    state.Numbers 
    |> List.mapi (renderButton dispatch)

  Html.div buttons

let renderFinished points dispatch =
  let score =
    Html.div [
      Html.h2 [
        prop.text (sprintf "Final Score: %i" points)
      ]
    ]
  Html.div [
    score
    Html.button [
      prop.style [ style.padding 20 ; style.fontSize 20 ]
      prop.onClick (fun _ -> dispatch Restart)
      prop.text "Restart"
    ]
  ]

let render (state: State) (dispatch: Msg -> unit) =
  match state with 
  | Not_Started ->
      Html.button [
        prop.style [ style.padding 20 ; style.fontSize 20 ]
        prop.onClick (fun _ -> dispatch Start)
        prop.text "Start"
      ]

  | Running state ->
      renderRunning state dispatch

  | Finished points ->
      renderFinished points dispatch
  

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run