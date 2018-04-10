//! Peachlib - the core Peach library
//!
//! Peach has the following capabilities:
//!   * "build" - builds given project to a binary (uses the system C compiler)
//!   * "run" - converts the project to bytecode, which it runs immediately
//!   * "repl" - creates a repl to interact with the code directly

#![feature(match_default_bindings, nll)]
extern crate syn;
extern crate time;

mod bytecode;
mod bytecode_ast;
mod compile;
mod eval;
mod tests;
mod typecheck;

pub use bytecode::{Bytecode, BytecodeEngine, VarStack};
pub use compile::compile_bytecode;
pub use eval::{eval_block_bytecode, eval_engine, Value};
