/*
** Union of all Groenlang values
*/
#[derive(Debug, Clone)]
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
    pub fn try_from(value: String) -> Option<Self> {
        let i = value.parse::<i32>();
        if i.is_ok() {
            /* try as an integer */
            return Some(Value::Int(i.unwrap()));
        }
        let f = value.parse::<f32>();
        if f.is_ok() {
            /* else try as a float */
            return Some(Value::Number(f.unwrap()));
        }
        return None;
    }
}
