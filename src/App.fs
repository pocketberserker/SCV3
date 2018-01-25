module SCV3

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

let mutable image = Browser.document.createElement_img()
image.src <- "shachiku.png"
let yoko = 270.
let mutable tate = 220.
let mutable move = 0.

let mutable enemyImage = Browser.document.createElement_img()
enemyImage.src <- "enemy.png"
let mutable enemyYoko = 0.
let enemyTate = 330.
let mutable enemyMove = 5.

type GameState =
  | Continue
  | Gameover

let mutable state = Continue

let checkHit () =
  let sx1 = yoko
  let sx2 = yoko + 100.
  let ex1 = enemyYoko
  let ex2 = enemyYoko + 43.
  let ashi = tate + 140.

  if sx1 < ex2 && ex1 < sx2 then
    if ashi > 330. then
      state <- Gameover

let random = System.Random()

let fireEnemy () =
  let rand = random.Next(2)
  if rand = 1 then
     enemyYoko <- -43.
     enemyMove <- 5.
  else
    enemyYoko <- 640.
    enemyMove <- -5.

let moveShachiku () =
  if tate < 100. then
    move <- 4.
  tate <- tate + move
  if tate > 220. then
    tate <- 220.
    move <- 0.

let rec loop (context: Browser.CanvasRenderingContext2D) (_: float) =

  moveShachiku ()

  if enemyMove = 0. then
    fireEnemy ()

  enemyYoko <- enemyYoko + enemyMove
  if enemyYoko < -43. || enemyYoko > 640. then
    enemyMove <- 0.

  context.clearRect(0., 0., 640., 360.)

  context.drawImage(U3.Case1 image, yoko, tate)

  context.drawImage(U3.Case1 enemyImage, enemyYoko, enemyTate)

  checkHit()

  match state with
  | Gameover ->
    context.fillText("ゲームオーバー！", 50., 100.)
  | Continue ->
    Browser.window.requestAnimationFrame(Browser.FrameRequestCallback (loop context)) |> ignore

let start() =
  let canvas = Browser.document.getElementsByTagName_canvas().[0]
  let context = canvas.getContext_2d()

  canvas.addEventListener_click(fun _ ->
    if move = 0. then
      move <- -4.
    null
  )

  Browser.window.requestAnimationFrame(Browser.FrameRequestCallback (loop context)) |> ignore

start ()
