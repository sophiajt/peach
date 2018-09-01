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

You can get a sense for what's supported by looking through `peach/test_files`.  There's also an example of working with SDL.

## Building

You'll need to have your platform C compiler in your path. For Linux/macOS this assumes 'clang' is in the path. On Windows, it assumes 'cl' is in the path.

Note for Windows users: some equivalent of running `"C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\Common7\Tools\VsDevCmd.bat"` should do the trick.

If you want to build the optional SDL example, you can uncomment it in the root Cargo.toml file. From there, you'll need to download SDL. Grab the Development Libraries for your platform from the [2.0 downloads](https://libsdl.org/download-2.0.php). 
