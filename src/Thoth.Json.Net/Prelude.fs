namespace Thoth.Json.Net

[<AutoOpen>]
module Polyfills =
    let isNull x = x = null

    let genericTypeArgument (t: System.Type) (index : int) =
#if NET40
        // https://stackoverflow.com/a/19504096/1397724
        t.GetGenericArguments()
        |> Array.filter (fun t -> not t.IsGenericParameter)
        |> Seq.nth index
#else
        t.GenericTypeArguments.[index]
#endif

    type Result<'a,'b> =
    | Ok of 'a
    | Error of 'b

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Result =
        let map f = function
        | Ok x -> Ok (f x)
        | Error y -> Error y

    module List =
        let tryLast xs =
            match xs with
            | [] -> None
            | xs -> List.nth xs (List.length xs - 1) |> Some

    module Option =
        let defaultValue def opt =
            match opt with
            | Some x -> x
            | None -> def

        let defaultWith defThunk option = match option with None -> defThunk () | Some v -> v

    module Seq =
        let private foldArraySubRight f (arr: 'T[]) start fin acc =
            let mutable state = acc
            for i = fin downto start do
                state <- f arr.[i] state
            state

        let foldBack<'T,'State> folder (source : seq<'T>) (state:'State) =
            let arr = Array.ofSeq source
            let len = arr.Length
            foldArraySubRight folder arr 0 (len - 1) state
