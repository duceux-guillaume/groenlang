use crate::code::{BinOpr, UnOpr};
use crate::error::{Error, GResult};
use crate::lexer::{LexState, Token};
use crate::object::Value;

/*
** Expression and variable descriptor.
** Code generation for variables and expressions can be delayed to allow
** optimizations; An 'expdesc' structure describes a potentially-delayed
** variable/expression. It has a description of its "main" value plus a
** list of conditional jumps that can also produce its value (generated
** by short-circuit operators 'and'/'or').
*/

/* kinds of variables/expressions */
#[derive(Debug, PartialEq)]
enum ExpKind {
    Void,     /* when 'expdesc' describes the last expression of a list,
              this kind means an empty list (so, no expression) */
    Nil,      /* constant nil */
    Bool,     /* constant true or false */
    ConstFlt, /* floating constant; nval = numerical float value */
    ConstInt, /* integer constant; ival = numerical integer value */
    Vararg,   /* vararg expression; info = instruction pc */
}

/* expressions description */
#[derive(Debug)]
struct ExpDesc {
    literal_value: Option<Value>, /* value when a literal expression */
    kind: ExpKind,
    var: Option<String>,
}

impl ExpDesc {
    fn new() -> ExpDesc {
        return ExpDesc {
            literal_value: None,
            kind: ExpKind::Void,
            var: None,
        };
    }

    fn init(&mut self, k: ExpKind, v: Option<Value>) {
        self.kind = k;
        self.literal_value = v;
    }

    fn init_var(&mut self, name: String) {
        self.kind = ExpKind::Vararg;
        self.literal_value = None;
        self.var = Some(name);
    }

    fn has_jumps(&self) -> bool {
        //TODO
        return false;
    }

    /*
     ** If expression is a constant, return its value
     */
    fn try_into_value(&self) -> Option<Value> {
        if self.has_jumps() {
            return None;
        }
        use ExpKind::*;
        return match self.kind {
            Nil => Some(Value::Nil()),
            Bool | ConstInt | ConstFlt => self.literal_value.clone(),
            _ => return None,
        };
    }

    fn is_true(&self) -> bool {
        return match (&self.kind, &self.literal_value) {
            (ExpKind::Nil, _) => false,
            (ExpKind::Bool, Some(Value::Bool(b))) => *b,
            (_, _) => false,
        };
    }
}

/* description of pending goto statements and label statements */
//struct Labeldesc {
//name: String, /* label identifier */
//pc: i32,      /* position in code */
//line: i32,    /* line where it appeared */
//nactvar: i32, /* number of active variables in that position */
//close: i32,   /* goto that escapes upvalues */
//}

#[derive(Debug, Clone)]
struct VarDesc {
    value: Value,
    name: String, /* variable name */
}

impl VarDesc {
    fn new(name: String) -> VarDesc {
        return VarDesc {
            value: Value::Nil(),
            name: name,
        };
    }
}

/*
** nodes for block list (list of active blocks)
*/
struct Block {
    actvar: Vec<VarDesc>,
}

impl Block {
    fn get_mut(&mut self, name: String) -> Option<&mut VarDesc> {
        for v in self.actvar.iter_mut() {
            if v.name == name {
                return Some(v);
            }
        }
        return None;
    }

    fn get(&self, name: String) -> Option<VarDesc> {
        for v in self.actvar.iter() {
            if v.name == name {
                return Some(v.clone());
            }
        }
        return None;
    }

    fn register(&mut self, name: String) {
        self.actvar.push(VarDesc::new(name));
    }
}

/* state needed to generate code for a given function */
pub struct FuncState {
    blocks: Vec<Block>, /* active local variables */
}

impl FuncState {
    fn new() -> FuncState {
        return FuncState { blocks: vec![] };
    }

    fn write(&mut self, name: String, value: Value) -> GResult<()> {
        for bl in self.blocks.iter_mut() {
            if let Some(var) = bl.get_mut(name.clone()) {
                var.value = value;
                return Ok(());
            }
        }
        return Err(Error::semantic(0, name, "unknown".to_owned()));
    }

    fn value(&self, name: String) -> GResult<Value> {
        for bl in self.blocks.iter() {
            if let Some(var) = bl.get(name.clone()) {
                return Ok(var.value);
            }
        }
        return Err(Error::semantic(0, name, "unknown".to_owned()));
    }

    fn register(&mut self, name: String) {
        if self.blocks.len() == 0 {
            self.enter_block();
        }
        self.blocks.last_mut().unwrap().register(name);
    }

    fn enter_block(&mut self) {
        let bl = Block { actvar: vec![] };
        self.blocks.push(bl);
    }

    fn leave_block(&mut self) {
        self.blocks.pop();
        // Clean vars
    }
}

pub struct Parser {
    ls: LexState,
    fs: FuncState,
}

impl Parser {
    pub fn eval(s: String) -> GResult<()> {
        let mut p = Parser::new(s);
        p.ls.next()?;
        return p.statlist();
    }

    pub fn new(input: String) -> Parser {
        return Parser {
            ls: LexState::new(input),
            fs: FuncState::new(),
        };
    }

    fn statlist(&mut self) -> GResult<()> {
        /* statlist -> { stat [';'] } */
        loop {
            match self.ls.current() {
                Token::Return => return self.statement(),
                Token::Char('}') => return Ok(()),
                Token::Eos => return Ok(()),
                _ => self.statement()?,
            }
        }
    }

    fn statement(&mut self) -> GResult<()> {
        use Token::*;
        //enterlevel(ls); /* increment stack */
        match self.ls.current() {
            Char(';') => {
                /* stat -> ';' (empty statement) */
                self.ls.next()?; /* skip ';' */
            }
            If => {
                /* stat -> ifstat */
                self.ifstat()?;
            }
            While => { /* stat -> whilestat */
                //whilestat(ls, line);
            }
            Do => {
                /* stat -> DO block END */
                self.ls.next()?; /* skip DO */
                self.block()?;
                self.expect_next(Char('}'))?;
            }
            For => { /* stat -> forstat */
                //forstat(ls, line);
            }
            Repeat => { /* stat -> repeatstat */
                //repeatstat(ls, line);
            }
            Function => {
                self.function_statement()?;
            }
            Let => {
                /* stat -> localstat */
                self.ls.next()?; /* skip LOCAL */
                //if (testnext(ls, TK_FUNCTION))  /* local function? */
                //  localfunc(ls);
                //else
                self.letstat()?;
            }
            Dbcolon => {
                /* stat -> label */
                self.ls.next()?; /* skip double colon */
                //labelstat(ls, str_checkname(ls), line);
            }
            Return => {
                /* stat -> retstat */
                self.ls.next()?; /* skip RETURN */
                //retstat(ls);
            }
            Break => { /* stat -> breakstat */
                //breakstat(ls);
            }
            Goto => {
                /* stat -> 'goto' NAME */
                self.ls.next()?; /* skip 'goto' */
                //gotostat(ls);
            }
            _ => {
                /* stat -> func | assignment */
                self.exprstat();
            }
        }
        //lua_assert(ls->fs->f->maxstacksize >= ls->fs->freereg &&
        //           ls->fs->freereg >= luaY_nvarstack(ls->fs));
        //ls->fs->freereg = luaY_nvarstack(ls->fs);  /* free registers */
        //leavelevel(ls);
        return Ok(());
    }

    /* funcstat -> FN funcname body */
    fn function_statement(&mut self) -> GResult<()> {
        self.ls.next()?;
        //let mut fname = self.ls.current();
        //FIXME: use name
        self.ls.next()?;
        let mut fbody = ExpDesc::new();
        return self.body(&mut fbody, true);
    }

    fn body(&mut self, fbody: &mut ExpDesc, ismethod: bool) -> GResult<()> {
        let mut new_fs = FuncState::new();
        //new_fs.f = self.add_prototype();
        //new_fs.f.linedefined = self.ls.linenumber();
        new_fs.enter_block();
        //self.fs = new_fs;
        self.parlist()?;
        self.block()?;
        return Ok(());
    }

    /* parlist -> [ {NAME ','} (NAME | '...') ] */
    fn parlist(&mut self) -> GResult<()> {
        self.ls.check_next_is_char('(')?;
        return self.ls.check_next_is_char(')');
    }

    /* stat -> LOCAL NAME ATTRIB { ',' NAME ATTRIB } ['=' explist] */
    fn letstat(&mut self) -> GResult<()> {
        let new_var_name = self.new_localvar()?;
        let mut exp = ExpDesc::new();
        self.expect_next(Token::Char('='))?;
        self.expression(&mut exp)?;
        if let Some(v) = exp.try_into_value() {
            self.fs.write(new_var_name.clone(), v.clone())?;
            println!("new var => {}={:?}", new_var_name, v);
        } else if exp.kind == ExpKind::Vararg {
            let v = self.fs.value(exp.var.unwrap())?;
            self.fs.write(new_var_name.clone(), v.clone())?;
            println!("new var => {}={:?}", new_var_name, v);
        } else {
            return Err(Error::semantic(
                self.ls.linenumber(),
                "value".to_string(),
                "something".to_owned(),
            ));
        }
        return Ok(());
    }

    /*
     ** Create a new local variable with the given 'name' and return it
     */
    fn new_localvar(&mut self) -> GResult<String> {
        if let Token::Name(name) = self.ls.current() {
            self.fs.register(name.clone());
            self.ls.next()?;
            return Ok(name);
        } else {
            return Err(Error::syntactical(
                self.ls.linenumber(),
                "<name>".to_owned(),
                self.ls.current().to_string(),
            ));
        }
    }

    fn expression(&mut self, exp: &mut ExpDesc) -> GResult<()> {
        self.subexpression(0, exp)?;
        return Ok(());
    }

    /*
     ** subexpression -> (simpleexp | unop subexpression) { binop subexpression }
     ** where 'binop' is any binary operator with a priority higher than 'limit'
     */
    fn subexpression(&mut self, limit: u8, exp: &mut ExpDesc) -> GResult<Option<BinOpr>> {
        //self.enterlevel(); // incre recursive calls to prevent stack overflow ?
        if let Some(uop) = UnOpr::try_from(&self.ls.current()) {
            /* prefix (unary) operator? */
            self.ls.next()?; /* skip operator */
            self.subexpression(uop.priority(), exp)?;
            self.apply_uop(uop, exp)?;
        } else {
            self.simpleexp(exp)?;
        }
        /* expand while operators have priorities higher than 'limit' */
        let mut opt = BinOpr::try_from(&self.ls.current());
        while opt.is_some() {
            let op = opt.take().unwrap();
            if op.left_priority() <= limit {
                break;
            }
            self.ls.next()?; /* skip operator */
            /* read sub-expression with higher priority */
            let mut right_exp = ExpDesc::new();
            opt = self.subexpression(op.right_priority(), &mut right_exp)?;
            /* Apply the operator */
            self.apply_binop(op, exp, right_exp)?;
        }
        //leavelevel(ls);
        return Ok(opt); /* return first untreated operator */
    }

    fn apply_binop(
        &mut self,
        op: BinOpr,
        left_exp: &mut ExpDesc,
        right_exp: ExpDesc,
    ) -> GResult<()> {
        if left_exp.literal_value.is_none() || right_exp.literal_value.is_none() {
            return Err(Error::semantic(
                self.ls.linenumber(),
                "two litteral values".to_owned(),
                "something else".to_owned(),
            ));
        }
        let leftv = left_exp.literal_value.take().unwrap();
        left_exp.literal_value = op.apply(&leftv, &right_exp.literal_value.unwrap());
        return Ok(());
    }

    fn apply_uop(&self, op: UnOpr, exp: &mut ExpDesc) -> GResult<()> {
        if let Some(value) = &exp.literal_value {
            exp.literal_value = op.apply(&value);
        } else {
            return Err(Error::semantic(
                self.ls.linenumber(),
                "value".to_owned(),
                "not a literal value".to_owned(),
            ));
        }
        return Ok(());
    }

    /* simpleexp -> FLT | INT | STRING | NIL | TRUE | FALSE | ... | constructor | FUNCTION body | suffixedexp */
    fn simpleexp(&mut self, exp: &mut ExpDesc) -> GResult<()> {
        use ExpKind::*;
        match self.ls.current() {
            Token::Flt(literal) => exp.init(ConstFlt, Value::try_from(literal)),
            Token::Int(literal) => exp.init(ConstInt, Value::try_from(literal)),
            Token::Nil => exp.init(Nil, None),
            Token::True => exp.init(Bool, Some(Value::Bool(true))),
            Token::False => exp.init(Bool, Some(Value::Bool(false))),
            Token::Name(name) => exp.init_var(name),
            _ => return self.suffixed_exp(exp),
        }
        return self.ls.next();
    }

    /* suffixedexp -> primaryexp { '.' NAME | '[' exp ']' | ':' NAME funcargs | funcargs } */
    fn suffixed_exp(&mut self, exp: &mut ExpDesc) -> GResult<()> {
        self.primary_exp(exp)?;
        return Ok(());
    }

    /* primaryexp -> NAME | '(' expr ')' */
    fn primary_exp(&mut self, exp: &mut ExpDesc) -> GResult<()> {
        match self.ls.current() {
            Token::Char('(') => {
                self.ls.next()?;
                self.expression(exp)?;
                if !self.ls.next_if_char(')')? && !self.ls.next_if_char('(')? {
                    return Err(Error::syntactical(
                        self.ls.linenumber(),
                        "expected: '(' or ')'".to_owned(),
                        self.ls.current().to_string(),
                    ));
                }
            }
            Token::Name(name) => {
                //TODO find var
                exp.kind = ExpKind::Vararg;
                exp.var = Some(name);
            }
            _ => {
                return Err(Error::syntactical(
                    self.ls.linenumber(),
                    "expected: <name> or '(' expr ')'".to_owned(),
                    self.ls.current().to_string(),
                ))
            }
        }
        return Ok(());
    }

    /* stat -> func | assignment */
    fn exprstat(&mut self) {
        //FuncState *fs = ls->fs;
        //struct LHS_assign v;
        //suffixedexp(ls, &v.v);
        //if (ls->t.token == '=' || ls->t.token == ',') { /* stat -> assignment ? */
        //  v.prev = NULL;
        //  restassign(ls, &v, 1);
        //}
        //else {  /* stat -> func */
        //  Instruction *inst;
        //  check_condition(ls, v.v.k == VCALL, "syntax error");
        //  inst = &getinstruction(fs, &v.v);
        //  SETARG_C(*inst, 1);  /* call statement uses no results */
        //}
    }

    /* ifstat -> IF cond block {ELSEIF cond block} [ELSE block] */
    fn ifstat(&mut self) -> GResult<()> {
        self.ls.check_next_is_token(Token::If)?;

        // if statement
        let mut exp = ExpDesc::new();
        self.expression(&mut exp)?;
        if exp.is_true() {
            self.block()?;
        } else {
            //skip block
            while !matches!(self.ls.current(), Token::Char('}') | Token::Eos) {
                self.ls.next()?;
            }
            self.ls.check_next_is_char('}')?;
        }

        while self.ls.next_if_token(Token::ElseIf)? {
            let mut cond = ExpDesc::new();
            self.expression(&mut cond)?;
            if cond.is_true() {
                self.block()?;
            } else {
                //skip block
                while !matches!(self.ls.current(), Token::Char('}') | Token::Eos) {
                    self.ls.next()?;
                }
                self.ls.check_next_is_char('}')?;
            }
        }

        if self.ls.next_if_token(Token::Else)? {
            self.block()?;
        }

        return Ok(());
    }

    fn expect_next(&mut self, t: Token) -> GResult<()> {
        if !self.ls.next_if_token(t.clone())? {
            return Err(Error::syntactical(
                self.ls.linenumber(),
                t.to_string(),
                self.ls.current().to_string(),
            ));
        }
        return Ok(());
    }

    /* block -> { statlist } */
    fn block(&mut self) -> GResult<()> {
        self.ls.check_next_is_char('{')?;
        self.fs.enter_block();
        self.statlist()?;
        self.fs.leave_block();
        return self.ls.check_next_is_char('}');
    }
}
