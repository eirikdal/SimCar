module BRP

open Agent
open Message

let brp_agent brp = Agent.Start(fun agent ->
    let rec loop brp = async {
        let! msg = agent.Receive()

        return! loop brp
    }

    loop brp)