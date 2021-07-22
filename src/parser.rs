use crate::code::{BinOpr, UnOpr};
use crate::lexer::{LexState, Token};
use crate::object::Value;

const UNARY_PRIORITY: u8 = 12; /* priority for unary operators */

/*
** Expression and variable descriptor.
** Code generation for variables and expressions can be delayed to allow
** optimizations; An 'expdesc' structure describes a potentially-delayed
** variable/expression. It has a description of its "main" value plus a
** list of conditional jumps that can also produce its value (generated
** by short-circuit operators 'and'/'or').
*/

/* kinds of variables/expressions */
#[derive(PartialEq)]
enum ExpKind {
    VVoid,  /* when 'expdesc' describes the last expression of a list,
            this kind means an empty list (so, no expression) */
    VNil,   /* constant nil */
    VTrue,  /* constant true */
    VFalse, /* constant false */
    VKk,    /* constant in 'k'; info = index of constant in 'k' */
    VKFlt,  /* floating constant; nval = numerical float value */
    VKInt,  /* integer constant; ival = numerical integer value */
    VKStr,  /* string constant; strval = TString address;
            (string is fixed by the lexer) */
    VNonReloc, /* expression has its value in a fixed register;
               info = result register */
    VLocal, /* local variable; var.ridx = register index;
            var.vidx = relative index in 'actvar.arr'  */
    VUpVal, /* upvalue variable; info = index of upvalue in 'upvalues' */
    VConst, /* compile-time <const> variable;
            info = absolute index in 'actvar.arr'  */
    VIndexed, /* indexed variable;
              ind.t = table register;
              ind.idx = key's R index */
    VIndexUp, /* indexed upvalue;
              ind.t = table upvalue;
              ind.idx = key's K index */
    VIndexI, /* indexed variable with constant integer;
             ind.t = table register;
             ind.idx = key's value */
    VIndexStr, /* indexed variable with literal string;
               ind.t = table register;
               ind.idx = key's K index */
    VJmp, /* expression is a test/comparison;
          info = pc of corresponding jump instruction */
    VReloc,  /* expression can put result in any register;
             info = instruction pc */
    VCall,   /* expression is a function call; info = instruction pc */
    VVararg, /* vararg expression; info = instruction pc */
}

/* expressions description */
struct ExpDesc {
    const_value: Option<Value>, /* value when a const expression */
    kind: ExpKind,
}

impl ExpDesc {
    fn new() -> ExpDesc {
        return ExpDesc {
            const_value: None,
            kind: ExpKind::VVoid,
        };
    }

    fn init(&mut self, k: ExpKind) {
        self.kind = k;
    }

    fn has_multiple_return(&self) -> bool {
        use ExpKind::*;
        return match self.kind {
            VCall | VVararg => true,
            _ => false,
        };
    }

    fn is_void(&self) -> bool {
        return self.kind == ExpKind::VVoid;
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
            VFalse => Some(Value::Bool(false)),
            VTrue => Some(Value::Bool(true)),
            VNil => Some(Value::Nil()),
            VKStr => Some(Value::KString(String::new())), //TODO
            VConst => None,                               //TODO
            //_ => return tonumeral(e, v); //TODO
            _ => return None,
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

#[derive(PartialEq)]
enum VarKind {
    Regular = 0,
    Const = 1,
    ToBeClosed = 2,
    CompileTimeConstant = 3,
}

struct VarDesc {
    value: Value,
    name: String, /* variable name */
    kind: VarKind,
    ridx: u8, /* register holding the variable */
}

impl VarDesc {
    fn new(name: String, kind: VarKind) -> VarDesc {
        return VarDesc {
            value: Value::Nil(),
            name: name,
            kind: kind,
            ridx: 0,
        };
    }

    fn in_register(&self) -> bool {
        return self.kind != VarKind::CompileTimeConstant;
    }

    fn ridx(&self) -> u8 {
        return self.ridx;
    }
}

/*
** nodes for block list (list of active blocks)
*/
//struct BlockCnt {}

/* state needed to generate code for a given function */
pub struct FuncState {
    actvar: Vec<VarDesc>, /* active local variables */
                          //f: Weak<Proto>,        /* current function header */
                          //prev: Weak<FuncState>, /* enclosing function */
                          //ls: Weak<LexState>,    /* lexical state */
                          //bl: Weak<BlockCnt>,    /* chain of current blocks */
                          //pc: i32,               /* next position to code (equivalent to 'ncode') */
                          //lasttarget: i32,       /* 'label' of last 'jump label' */
                          //previousline: i32,     /* last line that was saved in 'lineinfo' */
                          //nk: i32,               /* number of elements in 'k' */
                          //np: i32,               /* number of elements in 'p' */
                          //nabslineinfo: i32,     /* number of elements in 'abslineinfo' */
                          //firstlocal: i32,       /* index of first local var (in Dyndata array) */
                          //firstlabel: i32,       /* index of first label (in 'dyd->label->arr') */
                          //ndebugvars: i16,       /* number of elements in 'f->locvars' */
                          //nactvar: u8,           /* number of active local variables */
                          //nups: u8,              /* number of upvalues */
                          //freereg: u8,           /* first free register */
                          //iwthabs: u8,           /* instructions issued since last absolute line info */
                          //needclose: u8,         /* function needs to close upvalues when returning */
}

impl FuncState {
    fn new() -> FuncState {
        return FuncState { actvar: vec![] };
    }

    fn register(&mut self, v: VarDesc) {
        self.actvar.push(v);
    }

    fn last(&mut self) -> Option<&mut VarDesc> {
        return self.actvar.last_mut();
    }

    fn reglevel(&self, mut nvar: usize) -> usize {
        while nvar > 0 {
            nvar -= 1;
            let vd = self.getlocalvardesc(nvar); /* get previous variable */
            if vd.is_some() && vd.unwrap().in_register() {
                /* is in a register? */
                return vd.unwrap().ridx() as usize + 1;
            }
        }
        return 0; /* no variables in registers */
    }

    /*
     ** Return the "variable description" (VarDesc) of a given variable.
     ** (Unless noted otherwise, all variables are referred to by their
     ** compiler indices.)
     */
    fn getlocalvardesc(&self, vidx: usize) -> Option<&VarDesc> {
        return self.actvar.get(vidx);
    }
}

pub struct Parser {
    ls: LexState,
    fs: FuncState,
}

impl Parser {
    pub fn eval(s: String) {
        let mut p = Parser::new(s);
        p.ls.next();
        println!("{:?}", p.ls.current());
        p.statlist();
    }

    pub fn new(input: String) -> Parser {
        return Parser {
            ls: LexState::new(input),
            fs: FuncState::new(),
        };
    }

    fn statlist(&mut self) {
        /* statlist -> { stat [';'] } */
        println!("parser:statlist");
        while !self.block_follow(true) {
            if self.ls.current() == Token::TkReturn {
                self.statement();
                return; /* 'return' must be last statement */
            }
            self.statement();
            return;
        }
    }

    /*
     ** check whether current token is in the follow set of a block.
     ** 'until' closes syntactical blocks, but do not close scope,
     ** so it is handled in separate.
     */
    fn block_follow(&mut self, withuntil: bool) -> bool {
        use Token::*;
        println!("parser:block_follow");
        return match self.ls.current() {
            TkElse => true,
            TkElseIf => true,
            TkEnd => true,
            TkEos => true,
            TkUntil => withuntil,
            _ => false,
        };
    }

    fn statement(&mut self) {
        use Token::*;
        println!("parser:statement");
        println!("parser:current:{:?}", self.ls.current());
        //int line = ls->linenumber;  /* may be needed for error messages */
        //enterlevel(ls); /* increment stack */
        match self.ls.current() {
            TkChar(';') => {
                /* stat -> ';' (empty statement) */
                self.ls.next(); /* skip ';' */
            }
            TkIf => { /* stat -> ifstat */
                //ifstat(line);
            }
            TkWhile => { /* stat -> whilestat */
                //whilestat(ls, line);
            }
            TkDo => { /* stat -> DO block END */
                //ls.next();  /* skip DO */
                //block(ls);
                //check_match(ls, TK_END, TK_DO, line);
            }
            TkFor => { /* stat -> forstat */
                //forstat(ls, line);
            }
            TkRepeat => { /* stat -> repeatstat */
                //repeatstat(ls, line);
            }
            TkFunction => { /* stat -> funcstat */
                //funcstat(ls, line);
            }
            TkLocal => {
                /* stat -> localstat */
                println!("let");
                self.ls.next(); /* skip LOCAL */
                //if (testnext(ls, TK_FUNCTION))  /* local function? */
                //  localfunc(ls);
                //else
                self.localstat();
            }
            TkDbcolon => { /* stat -> label */
                //luaX_next(ls);  /* skip double colon */
                //labelstat(ls, str_checkname(ls), line);
            }
            TkReturn => { /* stat -> retstat */
                //luaX_next(ls);  /* skip RETURN */
                //retstat(ls);
            }
            TkBreak => { /* stat -> breakstat */
                //breakstat(ls);
            }
            TkGoto => { /* stat -> 'goto' NAME */
                //luaX_next(ls);  /* skip 'goto' */
                //gotostat(ls);
            }
            _ => {
                /* stat -> func | assignment */
                println!("func | assignment");
                self.exprstat();
            }
        }
        //lua_assert(ls->fs->f->maxstacksize >= ls->fs->freereg &&
        //           ls->fs->freereg >= luaY_nvarstack(ls->fs));
        //ls->fs->freereg = luaY_nvarstack(ls->fs);  /* free registers */
        //leavelevel(ls);
    }

    /* stat -> LOCAL NAME ATTRIB { ',' NAME ATTRIB } ['=' explist] */
    fn localstat(&mut self) {
        println!("parser:localstat");
        let mut toclose: isize = -1; /* index of to-be-closed variable (if any) */
        //VarDesc *var;  /* last variable */
        //int vidx, kind;  /* index and kind of last variable */
        let mut nvars = 1;
        loop {
            let var = self.new_localvar();
            if var.kind == VarKind::ToBeClosed {
                /* to-be-closed? */
                if toclose != -1 {
                    /* one already present? */
                    self.error("multiple to-be-closed variables in local list");
                }
                toclose = self.fs.actvar.len() as isize + nvars;
            }
            nvars += 1;
            if !self.testnext(Token::TkChar(',')) {
                break;
            }
        }
        let mut nexps = 1;
        let mut exp = ExpDesc::new();
        if self.testnext(Token::TkChar('=')) {
            nexps = self.explist(&mut exp);
        } else {
            exp.init(ExpKind::VVoid);
        }
        let var = self.fs.last().unwrap(); /* get last variable */
        if nvars == nexps as isize &&  /* no adjustments? */
            var.kind == VarKind::Const
        {
            /* last variable is const? */
            if let Some(value) = exp.try_into_value() {
                /* compile-time constant? */
                var.kind = VarKind::CompileTimeConstant; /* variable is a compile-time constant */
                var.value = value;
                self.adjustlocalvars((nvars - 1) as u8); /* exclude last variable */
                //  fs->nactvar++;  /* but count it */
            }
        } else {
            self.adjust_assign(nvars as u8, nexps, &exp);
            self.adjustlocalvars(nvars as u8);
        }
        //checktoclose(fs, toclose);
    }

    fn testnext(&self, c: Token) -> bool {
        println!(
            "testnext {:?}=={:?}->{}",
            self.ls.current(),
            c,
            self.ls.current() == c
        );
        return self.ls.current() == c;
    }

    /*
     ** Create a new local variable with the given 'name'
     */
    fn new_localvar(&mut self) -> &VarDesc {
        println!("new_localvar");
        if let Token::TkName(name) = self.ls.current().clone() {
            let kind = self.getlocalattribute();
            self.fs.register(VarDesc::new(name, kind));
        } else {
            self.error("wanted a name");
        }
        self.ls.next();
        return self.fs.last().unwrap();
    }

    fn error(&self, msg: &str) {
        panic!("byebye error: {}", msg);
    }

    /* Parse list of expression, return the number of expression in the list */
    /* explist -> expr { ',' expr } */
    fn explist(&mut self, exp: &mut ExpDesc) -> u8 {
        println!("explist");
        let mut n = 1; /* at least one expression */
        self.expr(exp);
        while self.testnext(Token::TkChar(',')) {
            //luaK_exp2nextreg(ls->fs, v);
            self.expr(exp);
            n += 1;
        }
        return n;
    }

    fn expr(&mut self, exp: &mut ExpDesc) {
        println!("expr");
        self.subexpr(0, exp);
    }

    /*
     ** subexpr -> (simpleexp | unop subexpr) { binop subexpr }
     ** where 'binop' is any binary operator with a priority higher than 'limit'
     */
    fn subexpr(&mut self, limit: u8, exp: &mut ExpDesc) -> Option<BinOpr> {
        println!("subexpr");
        //self.enterlevel(); // incre recursive calls to prevent stack overflow ?
        if let Some(uop) = UnOpr::try_from(&self.ls.current()) {
            /* prefix (unary) operator? */
            //  int line = ls->linenumber;
            self.ls.next(); /* skip operator */
            self.subexpr(UNARY_PRIORITY, exp);
        //  luaK_prefix(ls->fs, uop, v, line);
        } else {
            self.simpleexp(exp);
        }
        /* expand while operators have priorities higher than 'limit' */
        let mut op = BinOpr::try_from(&self.ls.current());
        while op.is_some() && op.take().unwrap().left_priority() > limit {
            //  expdesc v2;
            //  BinOpr nextop;
            //  int line = ls->linenumber;
            self.ls.next(); /* skip operator */
            //  luaK_infix(ls->fs, op, v);
            //  /* read sub-expression with higher priority */
            //  nextop = subexpr(ls, &v2, priority[op].right);
            //  luaK_posfix(ls->fs, op, v, &v2, line);
            //  op = nextop;
        }
        //leavelevel(ls);
        return op; /* return first untreated operator */
    }

    /* simpleexp -> FLT | INT | STRING | NIL | TRUE | FALSE | ... | constructor | FUNCTION body | suffixedexp */
    fn simpleexp(&mut self, exp: &mut ExpDesc) {
        use ExpKind::*;
        use Token::*;
        match self.ls.current() {
            TkFlt(_) => exp.init(VKFlt),
            TkInt(_) => exp.init(VKInt),
            TkNil => exp.init(VNil),
            TkTrue => exp.init(VTrue),
            TkFalse => exp.init(VFalse),
            _ => {}
        }
        self.ls.next();
    }

    fn adjust_assign(&mut self, nvars: u8, nexps: u8, exp: &ExpDesc) {
        let needed = nvars as i16 - nexps as i16; /* extra values needed */
        if exp.has_multiple_return() {
            /* last expression has multiple returns? */
            let mut extra = needed + 1; /* discount last expression itself */
            if extra < 0 {
                extra = 0;
            }
            //self.set_returns(); /* last exp. provides the difference */
        } else {
            if !exp.is_void() { /* at least one expression? */
                //luaK_exp2nextreg(fs, e);  /* close last expression */
            }
            if needed > 0 { /* missing values? */
                //luaK_nil(fs, fs->freereg, needed);  /* complete with nils */
            }
        }
        if needed > 0 {
            //luaK_reserveregs(fs, needed);  /* registers for extra values */
        } else {
            //fs->freereg += needed;  /* remove extra values */
        }
    }

    /*
     ** Start the scope for the last 'nvars' created variables.
     */
    fn adjustlocalvars(&mut self, nvars: u8) {
        //FuncState *fs = ls->fs;
        //int reglevel = luaY_nvarstack(fs);
        //int i;
        //for (i = 0; i < nvars; i++) {
        //    int vidx = fs->nactvar++;
        //    VarDesc *var = getlocalvardesc(fs, vidx);
        //    var->vd.ridx = reglevel++;
        //    var->vd.pidx = registerlocalvar(ls, fs, var->vd.name);
        //}
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

    /* ATTRIB -> ['<' Name '>'] */
    fn getlocalattribute(&mut self) -> VarKind {
        let mut attr: String = String::from("anonymous");
        if self.testnext(Token::TkChar('<')) {
            if let Token::TkName(name) = self.ls.current() {
                attr = name.clone();
            }
            //TODO: raise error
            self.ls.next(); /* move to '>' */
            //TODO: check >
            self.ls.next(); /* skip '>' */
            if attr == "const" {
                return VarKind::Const;
            } else if attr == "close" {
                return VarKind::ToBeClosed;
            } else {
                self.error("invalid attribute name");
            }
        }
        return VarKind::Regular;
    }
}
