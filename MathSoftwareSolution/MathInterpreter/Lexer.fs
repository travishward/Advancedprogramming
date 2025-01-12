namespace MathInterpreter

open System

module Lexer =

    /// Define token types for the arithmetic expressions (K)
    type Token =
        | Number of float
        | Complex of float * float
        | Function of string
        | Ident of string       // for variables/functions (K)
        | Equals                // '=' assignment (K)
        | Plus | Minus | Multiply | Divide | Remainder | Power
        | LParen | RParen
        | EOF

    /// A token introduced to handle implied multiplication (e.g., "2x^2") (K)
    type TokenOrPending =
        | RealToken of Token
        | PendingMultiply of float

    type LexerState = {
        Text: string
        Position: int
        Length: int
    }

    // ----------------------------------
    // Basic utilities (K)
    // ----------------------------------
    let initLexer (input: string) = 
        { Text = input; Position = 0; Length = input.Length }

    let peekChar state =
        if state.Position < state.Length then
            Some(state.Text.[state.Position])
        else
            None

    let advance state = 
        { state with Position = state.Position + 1 }

    let rec skipWhitespace state =
        match peekChar state with
        | Some ch when Char.IsWhiteSpace(ch) ->
            skipWhitespace (advance state)
        | _ -> state

    let isDigit ch = Char.IsDigit(ch)

    // ----------------------------------
    // Number lexing (K)
    // ----------------------------------
    let lexNumber state =
        let rec collectNumber acc st =
            match peekChar st with
            | Some ch when isDigit(ch) || ch = '.' || ch = 'E' || ch = 'e' ->
                collectNumber (acc + ch.ToString()) (advance st)
            | _ ->
                acc, st
        let numberStr, newState = collectNumber "" state
        match Double.TryParse(numberStr) with
        | true, value -> Number value, newState
        | _ -> failwithf "Invalid number format: %s" numberStr

    // ----------------------------------
    // Complex number lexing (optional) (K)
    // ----------------------------------
    let lexComplex state =
        let rec collectComplex acc st =
            match peekChar st with
            | Some ch when isDigit(ch) || ch = '.' || ch = '+' || ch = '-' || ch = 'i' ->
                collectComplex (acc + ch.ToString()) (advance st)
            | _ ->
                acc, st
        let complexStr, newState = collectComplex "" state
        // Basic parse for e.g. "3+4i" (K)
        match complexStr.Split([|'+'; '-'|], StringSplitOptions.RemoveEmptyEntries) with
        | [| real; imaginary |] when complexStr.EndsWith("i") ->
            let sign = 
                if complexStr.Contains("-") && complexStr.IndexOf('-') > 0 then -1.0 
                else 1.0
            Complex(Double.Parse(real), sign * Double.Parse(imaginary.TrimEnd('i'))), newState
        | _ ->
            failwithf "Invalid complex number format: %s" complexStr

    // ----------------------------------
    // Function or identifier (variable) (K)
    // ----------------------------------
    let lexFunctionOrIdent state =
        let rec collectName acc st =
            match peekChar st with
            | Some ch when Char.IsLetter(ch) ->
                collectName (acc + ch.ToString()) (advance st)
            | _ -> acc, st
        let name, newState = collectName "" state
        match name with
        | "sin" | "cos" | "tan" | "log" | "exp" ->
            Function name, newState
        | _ ->
            Ident name, newState

    // ----------------------------------
    // Main "nextToken" logic, including implied multiplication (K)
    // ----------------------------------
    let rec nextToken state =
        let state = skipWhitespace state
        match peekChar state with
        | Some ch when isDigit(ch) ->
            // Lex a number (K)
            let (numTok, st1) = lexNumber state
            // Check for implied multiplication (e.g., "2x") (K)
            match peekChar st1 with
            | Some nxt when Char.IsLetter(nxt) ->
                numTok, st1, true  // true indicates pending multiplication
            | _ ->
                numTok, st1, false

        | Some '/' -> Divide, advance state, false
        | Some '+' ->
            let advanced = advance state
            match peekChar advanced with
            | Some 'i' -> Plus, advanced, false
            | _ -> Plus, advanced, false
        | Some '-' ->
            let advanced = advance state
            match peekChar advanced with
            | Some 'i' -> Minus, advanced, false
            | _ -> Minus, advanced, false
        | Some '*' -> Multiply, advance state, false
        | Some '%' -> Remainder, advance state, false
        | Some '^' -> Power, advance state, false
        | Some '(' -> LParen, advance state, false
        | Some ')' -> RParen, advance state, false
        | Some '=' -> Equals, advance state, false
        | Some ch when Char.IsLetter(ch) ->
            let (tok, st1) = lexFunctionOrIdent state
            tok, st1, false
        | Some ch ->
            failwithf "Invalid character: %c" ch
        | None ->
            EOF, state, false

    /// "tokenize" loops through the input to generate tokens while handling pending multiplication (K)
    let rec tokenize state tokens =
        let token, newState, pendingMultiply = nextToken state
        match token with
        | EOF ->
            List.rev (EOF :: tokens)
        | Number v ->
            if pendingMultiply then
                tokenize newState (Multiply :: Number v :: tokens)
            else
                tokenize newState (Number v :: tokens)
        | _ ->
            tokenize newState (token :: tokens)

    let lex input =
        try
            let initialState = initLexer input
            let tokens = tokenize initialState []
            Ok tokens
        with
        | ex -> Error ex.Message
