# scheme-interpreter
Scheme interpreter written in Scheme

This repo contains the code for a code interpreter built in Scheme, and built for scheme. It obviously does not use Scheme's built-in eval procedure to avoid triviality. It can be run in two different ways after loading main.ss:

* rep -- Calling this procedure starts a REPL session.

* eval-one-exp -- Calling this procedure with a quoted scheme command as an argument evaluates the scheme command and outputs the result.

The final version of the interpreter is written entirely in continuation-passing style and provides the call/cc procedure, which calls a receiver procedure with the current continuation as an argument. This procedure can be used to implement features such as an escaper procedure.

This project was completed in CSSE304 - Programming Language Concepts at Rose-Hulman Institute of Technology.