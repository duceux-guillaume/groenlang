/*
** Union of all Groenlang values
*/
pub enum Value {
    //gc: GCObject       /* collectable objects */
    //void *p;           /* light userdata */
    RFunction(),
    Int(i32),
    Number(f32),
    Bool(bool),
    Nil(),
    KString(String), /* literal string */
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
