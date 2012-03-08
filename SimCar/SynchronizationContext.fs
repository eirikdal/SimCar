module SynchronizationContext

open Agent
open Models
open Message
open System.Threading

type SynchronizationContext with 
    /// A standard helper extension method to raise an event on the GUI thread
    member syncContext.RaiseDelegateEvent (event: DelegateEvent<_>) args = 
        syncContext.Post((fun _ -> event.Trigger([|box args; System.EventArgs.Empty|])),state=null)

    member syncContext.RaiseEvent (event: Event<_>) args = 
        syncContext.Post((fun _ -> event.Trigger args),state=null)
    /// A standard helper extension method to capture the current synchronization context.
    /// If none is present, use a context that executes work in the thread pool.
    static member CaptureCurrent () = 
        match SynchronizationContext.Current with 
        | null -> new SynchronizationContext()
        | ctxt -> ctxt

// Each of these lines declares an F# event that we can raise
let jobCompleted<'a> = new Event<Agent<Message> * string>()
//let allCompleted  = new Event<'T[]>()
let error         = new Event<System.Exception>()
let canceled      = new Event<System.OperationCanceledException>()
[<CLIEvent>]
let phevEvent     = new Event<string>()
let brpEvent      = new Event<string>()
let trfEvent      = new Event<string>()
let pnodeEvent    = new Event<string>()
let updateEvent   = new Event<float<kWh>[]>()
let syncContext = SynchronizationContext.CaptureCurrent()

// EventHandlers for tracking progress
let progressTotal       = new DelegateEvent<System.EventHandler>()
let progressPhev        = new DelegateEvent<System.EventHandler>()
let progressPnode       = new DelegateEvent<System.EventHandler>()

// EventHandlers for debugging Dayahead-algorithm
let dayaheadStep       = new DelegateEvent<System.EventHandler>()
let dayaheadProgress   = new DelegateEvent<System.EventHandler>()
let dayaheadInit       = new DelegateEvent<System.EventHandler>()

// EventHandlers for displaying cumulative pdf 
let probEvent           = new DelegateEvent<System.EventHandler>()
let probReset           = new DelegateEvent<System.EventHandler>()