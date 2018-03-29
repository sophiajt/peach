# So many experiments

* What if type checking and normalise were the same pass?  And that this was a lazy expansion?

* What if name binding didn't happen first, and that it was part of lazy expansion?

* What if the output of this pass wasn't normalise but the bytecode?

v1: Let's avoid constant folding as part of this pass

# Byte code conversion

Who knows what will work.  
Step 1: take a Fun
Step 2: take a ___ and a lazy context?

# Timing

```
        let start = PreciseTime::now();
        let length = codegen_fun(&fn_hash["main"]).len();
        let end = PreciseTime::now();
```