# Peach

This is a (very, very) incomplete re-write of the Rust compiler. It follows some different principles than the original.

* It tried to be as lazy as possible. Only the code called from the main is fully-checked and codegen'd.
* It compiles to a flat, stack-based bytecode representation
* It can run files like a scripting language
* It has a REPL
* It outputs C and then compiles the C

There are huge swaths of Rust currently missing:

* There is no borrow-checking
* There are no macros
* Things like name-binding are very much simplified
* Visibility, mutability, etc all still need to be implemented
* Error messages (it currently uses syn which doesn't expose source locations)
* And lots of other things

You can get a sense for what's supported by looking through `peach/test_files`

