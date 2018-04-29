//! Peachlib - the core Peach library
//!
//! Peach has the following capabilities:
//!   * "build" - builds given project to a binary (uses the system C compiler)
//!   * "run" - converts the project to bytecode, which it runs immediately
//!   * "repl" - creates a repl to interact with the code directly
#![feature(nll)]
#![feature(dyn_trait)]
extern crate proc_macro2;
extern crate syn;
extern crate time;

mod bytecode;
mod compile;
mod eval;
mod tests;

pub use bytecode::{builtin_type, Bytecode, BytecodeEngine, Fun, VarStack};
pub use compile::compile_bytecode;
pub use eval::{EvalEngine, Value};
