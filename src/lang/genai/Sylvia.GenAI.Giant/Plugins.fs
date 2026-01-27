namespace Sylvia.GenAI

open Microsoft.SemanticKernel

open Sylvia
open System.Collections.Generic

type GiantPlugin(name:string, ?id:string) =
    member val Name = name
    member val Id = defaultArg id (System.Guid.NewGuid().ToString())   
    member val SharedState = new Dictionary<string, Dictionary<string, obj>>() with get, set
    
    interface IPlugin with
        member x.Name with get () = name
        member x.SharedState with get() = x.SharedState and set(value) = x.SharedState <- value
        

type CASPlugin(?id:string) =
    inherit GiantPlugin("CAS", ?id=id)