module SynchronizationContext

open Agent
open Message
open System.Threading

type SynchronizationContext with 
    /// A standard helper extension method to raise an event on the GUI thread
    member syncContext.RaiseEvent (event: Event<_>) args = 
        syncContext.Post((fun _ -> event.Trigger args),state=null)

    /// A standard helper extension method to capture the current synchronization context.
    /// If none is present, use a context that executes work in the thread pool.
    static member CaptureCurrent () = 
        match SynchronizationContext.Current with 
        | null -> new SynchronizationContext()
        | ctxt -> ctxt

// Each of these lines declares an F# event that we can raise
let jobCompleted<'a> = new Event<Agent<'a Message> * string>()
//let allCompleted  = new Event<'T[]>()
let error         = new Event<System.Exception>()
let canceled      = new Event<System.OperationCanceledException>()
let progress      = new Event<string>()
let syncContext = SynchronizationContext.CaptureCurrent()
