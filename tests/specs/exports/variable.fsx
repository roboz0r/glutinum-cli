module rec Glutinum

(***)
#r "nuget: Fable.Core"
(***)

open Fable.Core
open System

[<Erase>]
type Exports =
    [<Import("version", "module")>]
    static member version: string = nativeOnly
