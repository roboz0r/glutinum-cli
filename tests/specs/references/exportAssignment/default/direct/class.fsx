module rec Glutinum

open Fable.Core
open Fable.Core.JsInterop
open System

[<Erase>]
type Exports =
    [<ImportDefault("module")>]
    static member inline ChalkInstance: ChalkInstance = nativeOnly

[<AllowNullLiteral>]
[<Interface>]
type ChalkInstance =
    interface end

(***)
#r "nuget: Fable.Core"
(***)
