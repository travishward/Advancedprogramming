namespace MathInterpreter

open System
open MathInterpreter.Lexer
open MathInterpreter.SymbolTable

module Parser =

    // 1) Types & Exceptions (K)
    type ParseTree =
        | Node of string * ParseTree list
        | Leaf of string
        | ComplexNode of float * float

    exception ParserException of string

    // 2) Utility Functions (K)
    let applyFunction (name: string) (value: float) =
        match name with
        | "sin" -> Math.Sin(value)
        | "cos" -> Math.Cos(value)
        | "tan" -> Math.Tan(value)
        | "log" -> Math.Log(value)
        | "exp" -> Math.Exp(value)
        | _ -> raise (ParserException $"Unsupported function: {name}")

    let rec visualizeParseTree (tree: ParseTree) (indent: string) =
        match tree with
        | Node(op, children) ->
            let childrenStr = 
                children
                |> List.map (fun child -> visualizeParseTree child (indent + "    "))
                |> String.concat "\n"
            sprintf "%s|-- %s\n%s" indent op childrenStr
        | Leaf(value) ->
            sprintf "%s|-- %s" indent value
        | ComplexNode(r, i) ->
            sprintf "%s|-- Complex: %.2f + %.2fi" indent r i

    // 3) Mutually Recursive Parser Functions (K)
    let rec parseStatement tokens =
        match tokens with
        | Ident varName :: Equals :: rest ->
            let exprValue, exprTree, remaining = parseExpression rest
            symbolTable.[varName] <- exprValue
            let assignTree = Node("=", [Leaf varName; exprTree])
            exprValue, assignTree, remaining
        | _ ->
            parseExpression tokens

    and parseExpression tokens =
        parseAddSub tokens

    and parseAddSub tokens =
        let leftValue, leftTree, remaining = parseMulDiv tokens
        parseAddSub' leftValue leftTree remaining

    and parseAddSub' leftValue leftTree tokens =
        match tokens with
        | Plus :: rest ->
            let rightValue, rightTree, remaining = parseMulDiv rest
            let newTree = Node("+", [leftTree; rightTree])
            parseAddSub' (leftValue + rightValue) newTree remaining
        | Minus :: rest ->
            let rightValue, rightTree, remaining = parseMulDiv rest
            let newTree = Node("-", [leftTree; rightTree])
            parseAddSub' (leftValue - rightValue) newTree remaining
        | _ ->
            leftValue, leftTree, tokens

    and parseMulDiv tokens =
        let leftValue, leftTree, remaining = parsePrimary tokens
        parseMulDiv' leftValue leftTree remaining

    and parseMulDiv' leftValue leftTree tokens =
        match tokens with
        | Multiply :: rest ->
            let rightValue, rightTree, rem = parsePrimary rest
            let newTree = Node("*", [leftTree; rightTree])
            parseMulDiv' (leftValue * rightValue) newTree rem
        | Divide :: rest ->
            let rightValue, rightTree, rem = parsePrimary rest
            if rightValue = 0.0 then raise (ParserException "Division by zero")
            let newTree = Node("/", [leftTree; rightTree])
            parseMulDiv' (leftValue / rightValue) newTree rem
        | Remainder :: rest ->
            let rightValue, rightTree, rem = parsePrimary rest
            if rightValue = 0.0 then raise (ParserException "Division by zero in remainder")
            let newTree = Node("%", [leftTree; rightTree])
            parseMulDiv' (leftValue % rightValue) newTree rem
        | Power :: rest ->
            let rightValue, rightTree, rem = parsePrimary rest
            let newTree = Node("^", [leftTree; rightTree])
            parseMulDiv' (Math.Pow(leftValue, rightValue)) newTree rem
        | _ ->
            leftValue, leftTree, tokens

    and parsePrimary tokens =
        match tokens with
        | Minus :: rest ->
            let val', tree', remaining' = parsePrimary rest
            let negValue = -val'
            let negTree = Node("unary -", [tree'])
            negValue, negTree, remaining'
        | Ident varName :: rest ->
            if symbolTable.ContainsKey(varName) then
                let value = symbolTable.[varName]
                value, Leaf(varName), rest
            else
                raise (ParserException $"Undefined variable: {varName}")
        | Number value :: rest ->
            value, Leaf(string value), rest
        | Complex (r, i) :: rest ->
            let combinedVal = r + i
            combinedVal, ComplexNode(r, i), rest
        | Function fname :: LParen :: rest ->
            let exprValue, exprTree, remaining = parseExpression rest
            match remaining with
            | RParen :: rem ->
                let result = applyFunction fname exprValue
                result, Node(fname, [exprTree]), rem
            | _ ->
                raise (ParserException "Mismatched parentheses after function call")
        | LParen :: rest ->
            let exprValue, exprTree, remaining = parseExpression rest
            match remaining with
            | RParen :: rem ->
                exprValue, exprTree, rem
            | _ ->
                raise (ParserException "Mismatched parentheses")
        | [] ->
            raise (ParserException "Unexpected end of input")
        | _ ->
            raise (ParserException "Unexpected token")
