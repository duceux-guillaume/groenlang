use crate::code::{BinOpr, UnOpr};
use crate::lexer::{LexState, Token};

const UNARY_PRIORITY: u8 = 12; /* priority for unary operators */

/* description of pending goto statements and label statements */
//struct Labeldesc {
//name: String, /* label identifier */
//pc: i32,      /* position in code */
//line: i32,    /* line where it appeared */
//nactvar: i32, /* number of active variables in that position */
//close: i32,   /* goto that escapes upvalues */
//}

/* description of an active local variable */
struct Vardesc {
    name: String,
}

impl Vardesc {
    fn new(name: String) -> Vardesc {
        return Vardesc { name: name };
    }
}

/* dynamic structures used by the parser */
pub struct Dyndata {
    /* list of all active local variables */
//lvar: Vec<Vardesc>,
//gt: Vec<Labeldesc>,    /* list of pending gotos */
//label: Vec<Labeldesc>, /* list of active labels */
}

impl Dyndata {
    pub fn new() -> Dyndata {
        return Dyndata {
            //lvar: vec![],
            //gt: vec![],
            //label: vec![],
        };
    }
}

/*
** nodes for block list (list of active blocks)
*/
//struct BlockCnt {}

/* state needed to generate code for a given function */
pub struct FuncState {
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
    /*
    fn new() -> FuncState {
        return FuncState {
            //f: Weak::new(),
            //prev: Weak::new(),
            //ls: Weak::new(),
            //bl: Weak::new(),
            //pc: 0,
            //lasttarget: 0,
            //previousline: 0,
            //nk: 0,
            //np: 0,
            //nabslineinfo: 0,
            //firstlocal: 0,
            //firstlabel: 0,
            //ndebugvars: 0,
            //nactvar: 0,
            //nups: 0,
            //freereg: 0,
            //iwthabs: 0,
            //needclose: 0,
        };
    }
    */
}

pub struct Parser {
    ls: LexState,
    lvars: Vec<Vardesc>,
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
            lvars: vec![],
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
        println!("parser:block_follow");
        return match self.ls.current() {
            Token::TkElse => true,
            Token::TkElseIf => true,
            Token::TkEnd => true,
            Token::TkEos => true,
            Token::TkUntil => withuntil,
            _ => false,
        };
    }

    fn statement(&mut self) {
        println!("parser:statement");
        println!("parser:current:{:?}", self.ls.current());
        //int line = ls->linenumber;  /* may be needed for error messages */
        //enterlevel(ls); /* increment stack */
        match self.ls.current() {
            Token::TkChar(';') => {
                /* stat -> ';' (empty statement) */
                self.ls.next(); /* skip ';' */
            }
            Token::TkIf => { /* stat -> ifstat */
                //ifstat(line);
            }
            Token::TkWhile => { /* stat -> whilestat */
                //whilestat(ls, line);
            }
            Token::TkDo => { /* stat -> DO block END */
                //ls.next();  /* skip DO */
                //block(ls);
                //check_match(ls, TK_END, TK_DO, line);
            }
            Token::TkFor => { /* stat -> forstat */
                //forstat(ls, line);
            }
            Token::TkRepeat => { /* stat -> repeatstat */
                //repeatstat(ls, line);
            }
            Token::TkFunction => { /* stat -> funcstat */
                //funcstat(ls, line);
            }
            Token::TkLocal => {
                /* stat -> localstat */
                println!("let");
                self.ls.next(); /* skip LOCAL */
                //if (testnext(ls, TK_FUNCTION))  /* local function? */
                //  localfunc(ls);
                //else
                self.localstat();
            }
            Token::TkDbcolon => { /* stat -> label */
                //luaX_next(ls);  /* skip double colon */
                //labelstat(ls, str_checkname(ls), line);
            }
            Token::TkReturn => { /* stat -> retstat */
                //luaX_next(ls);  /* skip RETURN */
                //retstat(ls);
            }
            Token::TkBreak => { /* stat -> breakstat */
                //breakstat(ls);
            }
            Token::TkGoto => { /* stat -> 'goto' NAME */
                //luaX_next(ls);  /* skip 'goto' */
                //gotostat(ls);
            }
            _ => {
                /* stat -> func | assignment */
                println!("func | assignment");
                //self.exprstat();
            }
        }
        //lua_assert(ls->fs->f->maxstacksize >= ls->fs->freereg &&
        //           ls->fs->freereg >= luaY_nvarstack(ls->fs));
        //ls->fs->freereg = luaY_nvarstack(ls->fs);  /* free registers */
        //leavelevel(ls);
    }

    fn localstat(&mut self) {
        println!("parser:localstat");
        /* stat -> LOCAL NAME ATTRIB { ',' NAME ATTRIB } ['=' explist] */
        //FuncState *fs = ls->fs;
        //int toclose = -1;  /* index of to-be-closed variable (if any) */
        //Vardesc *var;  /* last variable */
        //int vidx, kind;  /* index and kind of last variable */
        //int nvars = 0;
        //int nexps;
        //expdesc e;
        //do {
        self.new_localvar();
        //  kind = getlocalattribute(ls);
        //  getlocalvardesc(fs, vidx)->vd.kind = kind;
        //  if (kind == RDKTOCLOSE) {  /* to-be-closed? */
        //    if (toclose != -1)  /* one already present? */
        //      luaK_semerror(ls, "multiple to-be-closed variables in local list");
        //    toclose = fs->nactvar + nvars;
        //  }
        //  nvars++;
        //} while (testnext(ls, ','));
        if self.testnext(Token::TkChar('=')) {
            self.explist();
        }
        //else {
        //  e.k = VVOID;
        //  nexps = 0;
        //}
        //var = getlocalvardesc(fs, vidx);  /* get last variable */
        //if (nvars == nexps &&  /* no adjustments? */
        //    var->vd.kind == RDKCONST &&  /* last variable is const? */
        //    luaK_exp2const(fs, &e, &var->k)) {  /* compile-time constant? */
        //  var->vd.kind = RDKCTC;  /* variable is a compile-time constant */
        //  adjustlocalvars(ls, nvars - 1);  /* exclude last variable */
        //  fs->nactvar++;  /* but count it */
        //}
        //else {
        //  adjust_assign(ls, nvars, nexps, &e);
        //  adjustlocalvars(ls, nvars);
        //}
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
    fn new_localvar(&mut self) {
        println!("new_localvar");
        if let Token::TkName(name) = self.ls.current().clone() {
            self.lvars.push(Vardesc::new(name));
        } else {
            self.error("wanted a name");
        }
        self.ls.next();
    }

    fn error(&self, msg: &str) {
        panic!("byebye error: {}", msg);
    }

    /* Parse list of expression, return the number of expression in the list */
    fn explist(&mut self) -> usize {
        /* explist -> expr { ',' expr } */
        println!("explist");
        let mut n = 1; /* at least one expression */
        self.expr();
        while self.testnext(Token::TkChar(',')) {
            //luaK_exp2nextreg(ls->fs, v);
            self.expr();
            n += 1;
        }
        return n;
    }

    fn expr(&mut self) {
        println!("expr");
        self.subexpr(0);
    }

    /*
     ** subexpr -> (simpleexp | unop subexpr) { binop subexpr }
     ** where 'binop' is any binary operator with a priority higher than 'limit'
     */
    fn subexpr(&mut self, limit: u8) {
        println!("subexpr");
        //BinOpr op;
        //UnOpr uop;
        //self.enterlevel(); // incre recursive calls to prevent stack overflow ?

        if let Some(uop) = UnOpr::try_from(&self.ls.current()) {
            /* prefix (unary) operator? */
            //  int line = ls->linenumber;
            self.ls.next(); /* skip operator */
            self.subexpr(UNARY_PRIORITY);
        //  luaK_prefix(ls->fs, uop, v, line);
        } else {
            self.simpleexp();
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
        //return op;  /* return first untreated operator */
    }

    /* simpleexp -> FLT | INT | STRING | NIL | TRUE | FALSE | ... | constructor | FUNCTION body | suffixedexp */
    fn simpleexp(&mut self) {
        use Token::*;
        match self.ls.current() {
            _ => {
                //init_exp
            }
        }
        self.ls.next();
    }
}
