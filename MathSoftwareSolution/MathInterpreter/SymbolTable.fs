namespace MathInterpreter

open System.Collections.Generic

module SymbolTable =
    /// A global symbol table that stores variables (string) -> value (float). (K)
    let symbolTable = Dictionary<string, float>()

    /// Optionally, a function table to store parse trees. (K)
    // let functionTable = Dictionary<string, ParseTree>()
