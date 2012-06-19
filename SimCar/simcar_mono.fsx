#r "c:\\SimCar\SimCar\\bin\\Release\\SimCar.dll"

open SimCar

let simCar = new Sim.SimCar(96)

let test2 str = 
    printfn "%s" str |> ignore

simCar.RegisterProgressEvent test2
simCar.RegisterDebugEvent test2
simCar.Scheduler <- None
simCar.Contribution <- None
simCar.Method <- None
System.Threading.Thread.Sleep(5000)
simCar.ComputeDayahead()
simCar.Run(sprintf "%s" <| System.String.Format("{0:dd.MM.HH.mm}", System.DateTime.Now))