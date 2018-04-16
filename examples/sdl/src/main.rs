extern crate peach;
use peach::{BytecodeEngine, EvalEngine};
use std::path::Path;

use std::os::raw::c_void;

extern "C" {
    fn SDL_Init(flags: i32) -> i32;
    fn SDL_CreateWindow(
        title: *const c_void,
        x: i32,
        y: i32,
        w: i32,
        h: i32,
        flags: i32,
    ) -> *const c_void;
    fn SDL_FillRect(dst: *const c_void, rect: *const c_void, color: i32) -> i32;
    fn SDL_GetWindowSurface(window: *const c_void) -> *const c_void;
    fn SDL_UpdateWindowSurface(window: *const c_void) -> i32;
    fn SDL_Delay(ms: i32);
    fn SDL_DestroyWindow(window: *const c_void);
    fn SDL_Quit();
    fn SDL_PumpEvents();
}

fn process(fname: &str, start_fn: &str) -> BytecodeEngine {
    let mut bc = BytecodeEngine::new();

    // Step 1: Load up the parsed file so that we can lazily convert it
    //TODO: FIXME: we should probably take &str or Path
    let path = Path::new(fname).canonicalize().unwrap();
    bc.set_project_root(path.parent().unwrap().to_str().unwrap());
    bc.load_file(path.file_name().unwrap().to_str().unwrap());

    // Step 2: Convert to bytecode from the given location
    // We assume the starting function is found in scope 0, the starting scope
    bc.process_fn(start_fn, 0);
    //println!("{:#?}", bc.processed_fns);

    bc
}

fn main() {
    use std::env;

    let mut args = env::args();
    let _ = args.next(); // executable name

    match args.next() {
        Some(fname) => {
            let bc = process(&fname, "main");
            let mut ee = EvalEngine::new();

            ee.register_extern_fn_1("SDL_Init", SDL_Init);
            ee.register_extern_fn_6("SDL_CreateWindow", SDL_CreateWindow);
            ee.register_extern_fn_3("SDL_FillRect", SDL_FillRect);
            ee.register_extern_fn_1("SDL_GetWindowSurface", SDL_GetWindowSurface);
            ee.register_extern_fn_1("SDL_UpdateWindowSurface", SDL_UpdateWindowSurface);
            ee.register_extern_fn_1("SDL_Delay", SDL_Delay);
            ee.register_extern_fn_1("SDL_DestroyWindow", SDL_DestroyWindow);
            ee.register_extern_fn_0("SDL_Quit", SDL_Quit);
            ee.register_extern_fn_0("SDL_PumpEvents", SDL_PumpEvents);

            println!("Eval result:");
            ee.eval_program(&bc, "main");
        }
        None => println!("Pass the filename to run in the interpreter with SDL support"),
    }
}
