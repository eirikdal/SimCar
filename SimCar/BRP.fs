module BRP

open Agent
open Message

let brp_agent() = Agent.Start(fun agent ->
    let rec loop() = async {
        let! msg = agent.Receive()

        return! loop()
    }

    loop())