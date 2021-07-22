/*
** Union of all Groenlang values
*/
pub enum Value {
    //gc: GCObject       /* collectable objects */
    //void *p;           /* light userdata */
    RFunction(), /* light C functions */
    Int(i32),             /* integer numbers */
    Number(f32),          /* float numbers */
}

impl Value {
    pub fn try_from(value: &Vec<char>) -> Option<Self> {
        let s: String = value.iter().collect();
        let i = s.parse::<i32>();
        if i.is_ok() {
            /* try as an integer */
            return Some(Value::Int(i.unwrap()));
        }
        let f = s.parse::<f32>();
        if f.is_ok() {
            /* else try as a float */
            return Some(Value::Number(f.unwrap()));
        }
        return None;
    }
}
