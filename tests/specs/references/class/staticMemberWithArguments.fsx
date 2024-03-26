module rec Glutinum

open Fable.Core
open Fable.Core.JsInterop
open System

[<Erase>]
type Exports =
    interface end

[<AllowNullLiteral>]
[<Interface>]
type Hello =
    static member inline SayHelloTo (name: string): unit =
        emitJsExpr (name) $$"""
import { Class } from "module";
Hello.SayHelloTo($0)"""
    static member inline SayHelloTo2Persons (name1: string, name2: string): unit =
        emitJsExpr (name1, name2) $$"""
import { Class } from "module";
Hello.SayHelloTo2Persons($0, $1)"""
    static member inline SayHelloTo3Persons (name1: string, name2: string, name3: string): unit =
        emitJsExpr (name1, name2, name3) $$"""
import { Class } from "module";
Hello.SayHelloTo3Persons($0, $1, $2)"""

(***)
#r "nuget: Fable.Core"
(***)
