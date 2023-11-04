# Toy Programming Language
- Welcome to a toy programming language (ToyPL), a minimal yet powerful scheme-based language designed for educational purposes and fun experimentation with programming language concepts. ToyPL features an interactive Read-Eval-Print Loop (REPL), a fully-functional lexer and parser, debugging modes, environments, and a store for managing references.

- With automatic garbage collection implemented via a straightforward trace-and-sweep algorithm, ToyPL manages memory efficiently.

# Features
- Interactive REPL: Immediate feedback on expressions evaluation.
- Lexer & Parser: A robust foundation for understanding the breakdown and analysis of code.
- Debugging Modes: Step through code with helpful debugging support. 
- Environments & Store: Scoped environments and a store for reference management that mimics a heap.
- Automatic Garbage Collection: Simplified memory management with a trace-and-sweep garbage collector.
- Extensible Grammar: Easily expandable grammar for adding new features and constructs.

# Core Language Constructs
- Basic data types such as numbers, booleans (true and false), and the empty list.
- Arithmetic expressions: addition (+), subtraction (-), multiplication (*), and division (/).
- Relational and equality operators: equal (=), less-than (<).
- Logical operators: and, or, not.
- Conditional expressions with if-then-else.
- Variable bindings with let and immutable global definitions with def!.
- First-class functions with proc, function calls, and recursive functions with letrec.
- Reference-related operations: newref!, setref!, deref.
- Mutable variable assignment with set!.
- Block expressions for sequencing with {}.

# Interacting with the Language
- Ensure you have a Scheme interpreter installed on your system. Clone or download the ToyPL source code to your local machine.

- Start the ToyPL REPL in scheme by loading the "interp.scm" file and sending the (start) command. 