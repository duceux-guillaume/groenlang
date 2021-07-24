pub struct Zio {
    p: usize, /* current position in buffer */
    buf: Vec<char>, /* buffer */
              //lua_Reader reader;	/* reader function */
              //void *data;			/* additional data */
              //lua_State *L;		    /* Lua state (for reader) */
}

impl Zio {
    pub fn new(input: String) -> Zio {
        return Zio {
            p: 0,
            buf: input.chars().collect(),
        };
    }

    pub fn next(&mut self) -> Option<char> {
        if self.p < self.buf.len() {
            let c = self.buf[self.p];
            self.p += 1;
            return Some(c);
        }
        return None;
    }
}
