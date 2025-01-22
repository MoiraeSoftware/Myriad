---
title: "About Myriad"
weight: 20
---

# Welcome to Myriad
Myriad is a code generator, it can take an arbitrary file and uses it to produce F# code.  The file can be anything from a plain txt file to an F# file to anything else.  Read on to find out more, or visit our [documentation](docs/) to get started!
 

## How does Myriad work, how is it different?  

Myriad is a code generator, in response to an input code can be generated, specifically F# code is generated which means all the various constructs available in F# such as records, discriminated unions, Active patterns etc, can all be generated in response to an input.  Myriad can be used via MSBuild as part of a your project file, or standalone by using the CLI tool.  If differs  from the way F# type provider works as it can produce F# types in response to other types or any arbitrary file or input, the code that is output can be examined in the development environment and can also be generated inline with your exiting code.  Myriad predates C# source generators but the way it work is quite similar, Myriad was influenced somewhat by Ocaml's Ppx code generation.    

## How does Myriad help?  

The idea behind Myriad is to un-complicate, as far as possible, the ability to generate code and do meta-programming in F#. By meta-programming in F# I mean generating actual F# types too, not just IL output or simple classes like type providers but outputting full F# types like discriminated unions and records.  

Myriad is an evolution of the ideas I developed while working with F#'s type providers and other meta-programming functionality like quotations and AST manipulation.  Myriad aims to make it easy to extend the compiler via Myriad plugins rather than modifying or adjusting Type Providers and other F# improvements that would take a long time to be developed and released.  

The way a Myriad plugin works is that it that works on a particular file type or input data, this could be a fragment of AST input or a proto file etc.  The plugin is supplied metadata such as file name and project information.  there are also various helpers in the library make it easier to produce the AST output with the final form being source code that is built into your project.  The advantages of this are the compiler can optimise the generated code and the tooling can operate effectively as you can navigate the generated code as normal.

## What's next for Myriad?  

Who knows, Im not activly writing any new features, I do check in occasionally and merge any PR's and answer questions. 
