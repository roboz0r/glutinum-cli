module Glutinum.Converter.Reader.TypeQueryNode

open Glutinum.Converter.GlueAST
open Glutinum.Converter.Reader.Types
open TypeScript
open TypeScriptHelpers
open Fable.Core.JsInterop
open Glutinum.Converter.Reader.Utils
open FsToolkit.ErrorHandling

module Result =
    let ofOptionOrError error opt =
        match opt with
        | Some value -> Ok value
        | None -> Error error

let readTypeQueryNode

    (reader: ITypeScriptReader)
    (typeQueryNode: Ts.TypeQueryNode)
    =

    let checker = reader.checker
    let typ = checker.getTypeAtLocation !!typeQueryNode.exprName

    // This is safe as both cases have a `kind` field
    let exprNameKind: Ts.SyntaxKind = typeQueryNode.exprName?kind

    match typ.flags, typ.getSymbol (), exprNameKind with
    | HasTypeFlags Ts.TypeFlags.Object, None, Ts.SyntaxKind.Identifier ->

        let exprName: Ts.Identifier = !!typeQueryNode.exprName

        result {
            let! aliasSymbol =
                checker.getSymbolAtLocation exprName
                |> Result.ofOptionOrError (
                    generateReaderError
                        "type node (TypeQuery)"
                        "Missing symbol"
                        typeQueryNode
                )

            let! declarations =
                aliasSymbol.declarations
                |> Result.ofOptionOrError (
                    generateReaderError
                        "type node (TypeQuery)"
                        "Missing declarations"
                        typeQueryNode
                )

            let! declaration =

                if declarations.Count <> 1 then
                    Error(
                        generateReaderError
                            "type node (TypeQuery)"
                            "Expected exactly one declaration"
                            typeQueryNode
                    )

                else
                    Ok(declarations.[0])

            let! variableDeclaration =
                match declaration.kind with
                | Ts.SyntaxKind.VariableDeclaration ->
                    Ok(declaration :?> Ts.VariableDeclaration)
                | unsupported ->
                    Error(
                        generateReaderError
                            "type node (TypeQuery)"
                            $"Unsupported declaration kind {SyntaxKind.name unsupported}"
                            typeQueryNode
                    )

            let! typeNode =
                variableDeclaration.``type``
                |> Result.ofOptionOrError (
                    generateReaderError
                        "type node (TypeQuery)"
                        "Missing type"
                        typeQueryNode
                )

            match typeNode.kind with
            | Ts.SyntaxKind.TypeOperator ->
                let typeOperatorNode = typeNode :?> Ts.TypeOperatorNode

                return reader.ReadTypeOperatorNode typeOperatorNode

            | unsupported ->
                return!
                    generateReaderError
                        "type node (TypeQuery)"
                        $"Unsupported declaration kind {SyntaxKind.name unsupported}"
                        typeQueryNode
                    |> Error

        }
        |> function
            | Ok glueType -> glueType
            | Error error -> failwith error

    | HasTypeFlags Ts.TypeFlags.Object, Some symbol, _ ->
        // Try to find the declaration of the type, to get more information about it
        match symbol.declarations with
        | Some declarations ->
            let declaration = declarations.[0]

            match declaration.kind with
            | Ts.SyntaxKind.ClassDeclaration ->
                {
                    Name = symbol.name
                    Constructors = []
                    Members = []
                    TypeParameters = []
                    HeritageClauses = []
                }
                |> GlueType.ClassDeclaration

            // We don't support TypeQuery for ModuleDeclaration yet
            // See https://github.com/glutinum-org/cli/issues/70 for a possible solution
            | Ts.SyntaxKind.ModuleDeclaration -> GlueType.Discard
            | _ -> reader.ReadNode declaration

        | None -> GlueType.Primitive GluePrimitive.Any

    | HasTypeFlags Ts.TypeFlags.String, _, _ ->
        GlueType.Primitive GluePrimitive.String

    | HasTypeFlags Ts.TypeFlags.Number, _, _ ->
        GlueType.Primitive GluePrimitive.Number

    | HasTypeFlags Ts.TypeFlags.Boolean, _, _ ->
        GlueType.Primitive GluePrimitive.Bool

    | HasTypeFlags Ts.TypeFlags.Any, _, _ ->
        GlueType.Primitive GluePrimitive.Any

    | HasTypeFlags Ts.TypeFlags.Void, _, _ ->
        GlueType.Primitive GluePrimitive.Unit

    | _ -> GlueType.Primitive GluePrimitive.Any
