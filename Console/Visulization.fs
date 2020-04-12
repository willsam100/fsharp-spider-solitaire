module SpiderSolitare.Visulization
open SpiderSolitare.MonteCarloTreeSearch
open ImageMagick
open System.Diagnostics
open SpiderSolitare.Game

let createImage name imageCount columnHeight (games: float list list list) = 

    use image = new MagickImage(MagickColor.FromRgb(byte 0, byte  0, byte  0), columnHeight, (10 * imageCount))
    image.Grayscale()

    image.GetPixels() 
    |> Seq.iter (fun pixel -> 
        let i = pixel.Y / 10
        let y = pixel.Y - (10 * i)
        let card =  games.[y].[pixel.X].[i] * 14. |> byte
        pixel.Set([| card; |]) 
        )

    image.Resize(50, 0)
    image.Quality <- 100

    let filename = sprintf "/Users/willsam100/Desktop/image-%s.png" name
    image.Write(filename)
    
    let startInfo = ProcessStartInfo("open")
    startInfo.RedirectStandardInput <- true
    startInfo.RedirectStandardOutput <- true
    startInfo.UseShellExecute <- false
    startInfo.Arguments <- filename
    startInfo.WindowStyle <- ProcessWindowStyle.Hidden
    Process.Start(startInfo) |> ignore


let createMnistImage name imageCount columnHeight rowSize (games: float list list list) = 

    // let rowSize = rowSize + 1

    use image = new MagickImage(MagickColor.FromRgb(byte 0, byte  0, byte  0), columnHeight, (rowSize * imageCount))

    image.Grayscale()

    image.GetPixels() 
    |> Seq.iter (fun pixel -> 
        let i = pixel.Y / rowSize
        let y = pixel.Y - (rowSize * i)
        let card =  games.[y].[pixel.X].[i] * 255. |> byte
        pixel.Set([| card; |]) 
        )

    image.Resize(50, 0)
    image.Quality <- 100

    let filename = sprintf "/Users/willsam100/Desktop/image-mnist-%s.png" name
    image.Write(filename)
    
    let startInfo = ProcessStartInfo("open")
    startInfo.RedirectStandardInput <- true
    startInfo.RedirectStandardOutput <- true
    startInfo.UseShellExecute <- false
    startInfo.Arguments <- filename
    startInfo.WindowStyle <- ProcessWindowStyle.Hidden
    Process.Start(startInfo) |> ignore


let visulizeColumnPrediction (brainsMover: MonteCarloTreeSearch.IBransMover) (nMove: MutableNode<Game, 'b>) = 

    let c = 
        brainsMover.GetColumnOfLongestRun nMove.Game
        |> List.sortByDescending snd
        |> List.take 2

    let tabCount = 10  // image width
    let cnnView = brainsMover.GetCnnView nMove.Game

    printfn "Have response: %d" cnnView.ProbsOneH

    let columnHeight = cnnView.ProbsOneH
    let count = cnnView.ProbsOneC
    cnnView.ProbsOne
    |> List.splitInto tabCount
    |> List.map (List.splitInto columnHeight)
    |> createImage "one" count columnHeight

    let columnHeight = cnnView.ProbsTwoH
    let count = cnnView.ProbsTwoC
    cnnView.ProbsTwo
    |> List.splitInto tabCount
    |> List.map (List.splitInto columnHeight)
    |> createImage "two" count columnHeight

    let columnHeight = cnnView.ProbsThreeH
    let count = cnnView.ProbsThreeC
    cnnView.ProbsThree
    |> List.splitInto tabCount
    |> List.map (List.splitInto columnHeight)
    |> createImage "three" count columnHeight

    // let columnHeight = cnnView.ProbsFourH
    // let count = cnnView.ProbsFourC
    // cnnView.ProbsFour
    // |> List.splitInto tabCount
    // |> List.map (List.splitInto columnHeight)
    // |> createImage "four" count columnHeight

    // let columnHeight = cnnView.ProbsFiveH
    // let count = cnnView.ProbsFiveC
    // cnnView.ProbsFive
    // |> List.splitInto tabCount
    // |> List.map (List.splitInto columnHeight)
    // |> createImage "five" count columnHeight

    printfn "Longest Run: %A %A <--------------------"  (((nMove.Game |> GameMover.getRuns |> List.maxBy (fun (c, cs) -> cs.Length) |> fst))) c.[0]
    printfn "Longest Run (2): %A" c.[1]
    // false, gameNumber, nMove.Game, movesMade,  history