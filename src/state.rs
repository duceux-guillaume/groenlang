use crate::object::Value;

pub struct GlobalState {}

/*
* 'per thread' state
*/
pub struct ThreadState {
    //CommonHeader;
//lu_byte status;
//lu_byte allowhook;
//unsigned short nci;  /* number of items in 'ci' list */
//top: usize,          /* first free slot in the stack */
//CallInfo *ci;  /* call info for current function */
//StkId stack_last;  /* end of stack (last element + 1) */
//stack: Vec<StackValue>, /* stack base */
//UpVal *openupval;  /* list of open upvalues in this stack */
//StkId tbclist;  /* list of to-be-closed variables */
//GCObject *gclist;
//struct lua_State *twups;  /* list of threads with open upvalues */
//struct lua_longjmp *errorJmp;  /* current error recover point */
//CallInfo base_ci;  /* CallInfo for first level (C calling Lua) */
//volatile lua_Hook hook;
//ptrdiff_t errfunc;  /* current error handling function (stack index) */
//l_uint32 nCcalls;  /* number of nested (non-yieldable | C)  calls */
//int oldpc;  /* last pc traced */
//int basehookcount;
//int hookcount;
//volatile l_signalT hookmask;
}

impl ThreadState {
    pub fn new() -> ThreadState {
        return ThreadState {
            //stack: vec![],
        };
    }

    pub fn push(&mut self, val: Value) {
        //self.stack.push(StackValue::Value(val));
    }

    pub fn pop(&mut self) -> Option<Value> {
        //if let Some(StackValue::Value(val)) = self.stack.pop() {
        //    return Some(val);
        //}
        return None;
    }
}
