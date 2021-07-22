use groenlang::state::ThreadState;

/*
** Main body of stand-alone interpreter (to be called in protected mode).
** Reads the options and handles them all.
*/
fn pmain(state: &ThreadState) -> i32 {
    return 0;
}

fn main() {
    let state = ThreadState::new(); /* create state */
    //pushrfunction(state, pmain); /* to call 'pmain' in protected mode */
}
