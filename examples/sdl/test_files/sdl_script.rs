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

fn main() {
    let result = SDL_Init(32);
    println!("{}", result);
    let window = SDL_CreateWindow(NULL, 0, 0, 1000, 1000, 4);
    let screen_surface = SDL_GetWindowSurface(window);
    SDL_FillRect(screen_surface, NULL, 0x45954545);
    SDL_UpdateWindowSurface(window);
    SDL_Delay(5000);
    SDL_DestroyWindow(window);
    SDL_Quit();
}
