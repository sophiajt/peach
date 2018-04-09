# Madness

You shouldn't be reading this part.  There isn't any interesting text here.  It's just a space for your eyes to keep searching for meaning when there is none.

# Experiments

This project is the playful playground of experiments. There are many paths ahead of us, but we have but two feet.

# TODO

Need a better intermediate format.  Preferably something we can convert to post function processing that will help us quickly do a type/lifetime test. (note: this is kinda sorta done now, but would will likely need a bit more work)

Some possible next steps:

* Add functions and function calls
  * (DONE) basic support
  * (DONE) params
  * TODO: Check types of each argument
* (DONE) if
* (DONE) while
* (DONE) Playing with the while codegen, I'm noticing we could codegen a bit smarter.  Expressions could come out with paren'd strings inside of other expressions.  Assignments and the like would cap off the codegen.
* (DONE) if expressions
* (IN PROGRESS) mod
*   TODO: mod visibility
*   DONE: correct mod scope traversal
*   DONE: leading colon
* (DONE) 'use' items
* (DONE) mod "file":
  * (DONE) Noticed that if this is added, you can load files froms of code from the repl

* All this new functionality feels like we want another refactor for cleanliness.  I'm starting to lose track of what functionality is where.
* TODO: mutability check
* TODO: consistent naming
* Add structs and member access
* Add type inference (we kinda already have it, but it's not like real yet)

We could also start a non-lazy mode that's parallel
  * basically: each Item would be done in parallel.  If you hit a definition you don't have (which is another Item), then
    you delay yourself with that as the Requirement.  Periodically we flush the enqueued Item tasks and look for ones whose
    Requirement was met.  
  * If we wanted, each one could take it all the way to codegen.

Where and how do macros fit in?




Non-next steps:

* Good error messages (would need better parsing library)




# Thoughts

I kinda think it makes sense to set up the lazy bytecode converter first.  It might be some upfront work to make this happen, but then I think we could use it for the next stage of things, like bringing 
on additional functions, structs, etc.

Basically, how I think it should work (with currently using syn) is to us syn first to parse the file.  Then, we'll go through the items we have, grab their name and then put their syn-parsed bodies into a holder.

Then we'll start converting from known correct locations.  Since we'll start with binaries, how about first starting with the main function.  Into the converter, we'll pass the lazy elements and the converted elements. (we can get fancy in the future with how this happens but first Make It Work(tm))