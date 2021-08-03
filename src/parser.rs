use crate::code::{BinOpr, UnOpr};
use crate::lexer::{LexState, Token};
use crate::object::Value;

type ParserResult<T> = std::result::Result<T, ParsingError>;
#[derive(Debug, Clone)]
pub struct ParsingError {
    line: usize,
    expected_token: Token,
    current_token: Token,
}

impl ParsingError {
    fn new(l: usize, e: Token, c: Token) -> ParsingError {
        return ParsingError {
            line: l,
            expected_token: e,
            current_token: c,
        };
    }
}

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
    True,     /* constant true */
    False,    /* constant false */
    ConstK,   /* constant in 'k'; info = index of constant in 'k' */
    ConstFlt, /* floating constant; nval = numerical float value */
    ConstInt, /* integer constant; ival = numerical integer value */
    ConstStr, /* string constant; strval = TString address;
              (string is fixed by the lexer) */
    NonReloc, /* expression has its value in a fixed register;
              info = result register */
    Local, /* local variable; var.ridx = register index;
           var.vidx = relative index in 'actvar.arr'  */
    UpVal, /* upvalue variable; info = index of upvalue in 'upvalues' */
    Const, /* compile-time <const> variable;
           info = absolute index in 'actvar.arr'  */
    Indexed, /* indexed variable;
             ind.t = table register;
             ind.idx = key's R index */
    IndexUp, /* indexed upvalue;
             ind.t = table upvalue;
             ind.idx = key's K index */
    IndexI, /* indexed variable with constant integer;
            ind.t = table register;
            ind.idx = key's value */
    IndexStr, /* indexed variable with literal string;
              ind.t = table register;
              ind.idx = key's K index */
    Jump, /* expression is a test/comparison;
          info = pc of corresponding jump instruction */
    Reloc,  /* expression can put result in any register;
            info = instruction pc */
    Call,   /* expression is a function call; info = instruction pc */
    Vararg, /* vararg expression; info = instruction pc */
}

/* expressions description */
#[derive(Debug)]
struct ExpDesc {
    literal_value: Option<Value>, /* value when a literal expression */
    kind: ExpKind,
}

impl ExpDesc {
    fn new() -> ExpDesc {
        return ExpDesc {
            literal_value: None,
            kind: ExpKind::Void,
        };
    }

    fn init(&mut self, k: ExpKind, v: Option<Value>) {
        self.kind = k;
        self.literal_value = v;
    }

    fn has_multiple_return(&self) -> bool {
        use ExpKind::*;
        return match self.kind {
            Call | Vararg => true,
            _ => false,
        };
    }

    fn is_void(&self) -> bool {
        return self.kind == ExpKind::Void;
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
            False => Some(Value::Bool(false)),
            True => Some(Value::Bool(true)),
            Nil => Some(Value::Nil()),
            Const => None,
            ConstInt | ConstFlt | ConstStr => self.literal_value.clone(),
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

#[derive(Debug, PartialEq, Clone)]
enum VarKind {
    Regular = 0,
    Const = 1,
    ToBeClosed = 2,
    CompileTimeConstant = 3,
}

#[derive(Debug, Clone)]
struct VarDesc {
    value: Value,
    name: String, /* variable name */
    kind: VarKind,
    ridx: usize, /* register holding the variable */
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

    fn ridx(&self) -> usize {
        return self.ridx;
    }

    fn set_ridx(&mut self, i: usize) {
        self.ridx = i;
    }
}

/*
** nodes for block list (list of active blocks)
*/
struct Block {
    isloop: bool,
    actvar: Vec<VarDesc>,
}

impl Block {
    fn isloop(&self) -> bool {
        return self.isloop;
    }
}

/* state needed to generate code for a given function */
pub struct FuncState {
    actvar: Vec<VarDesc>, /* active local variables */
    blocks: Vec<Block>,
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
        return FuncState {
            actvar: vec![],
            blocks: vec![],
        };
    }

    fn register(&mut self, mut v: VarDesc) {
        v.set_ridx(self.actvar.len());
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
     */
    fn getlocalvardesc(&self, vidx: usize) -> Option<&VarDesc> {
        return self.actvar.get(vidx);
    }

    fn goiftrue(&mut self, exp: &ExpDesc) {
        self.dischargevars(exp);
        match &exp.literal_value {
            VJmp => {
                /* condition? */
                self.negatecondition(exp); /* jump when it is false */
                //pc = e->u.info;  /* save jump position */
            }
            _ => { //TODO}
            }
        }
    }

    fn dischargevars(&mut self, exp: &ExpDesc) {}

    fn negatecondition(&mut self, exp: &ExpDesc) {}

    fn enter_block(&mut self, isloop: bool) {
        let bl = Block {
            isloop: isloop,
            actvar: self.actvar.clone(),
        };
        //bl->firstlabel = fs->ls->dyd->label.n;
        //bl->firstgoto = fs->ls->dyd->gt.n;
        //bl->upval = 0;
        //bl->insidetbc = (fs->bl != NULL && fs->bl->insidetbc);
        //bl->previous = fs->bl;
        self.blocks.push(bl);
        //lua_assert(fs->freereg == luaY_nvarstack(fs));
    }

    fn leave_block(&mut self) {
        let hasblock = self.blocks.pop();
        if hasblock.is_none() {
            return;
        }
        let bl = hasblock.unwrap();
        let hasclose = false;
        //int stklevel = reglevel(fs, bl->nactvar);  /* level outside the block */
        if bl.isloop() { /* fix pending breaks? */
            //hasclose = createlabel(ls, luaS_newliteral(ls->L, "break"), 0, 0);
        }
        //if (!hasclose && bl->previous && bl->upval) {
        //  luaK_codeABC(fs, OP_CLOSE, stklevel, 0, 0);
        //}
        //removevars(fs, bl->nactvar);
        //lua_assert(bl->nactvar == fs->nactvar);
        //fs->freereg = stklevel;  /* free registers */
        //ls->dyd->label.n = bl->firstlabel;  /* remove local labels */
        //if (bl->previous) { /* inner block? */
        //  movegotosout(fs, bl);  /* update pending gotos to outer block */
        //}
        //else {
        //  if (bl->firstgoto < ls->dyd->gt.n)  /* pending gotos in outer block? */
        //    undefgoto(ls, &ls->dyd->gt.arr[bl->firstgoto]);  /* error */
        //}
    }
}

pub struct Parser {
    ls: LexState,
    fs: FuncState,
}

impl Parser {
    pub fn eval(s: String) -> ParserResult<()> {
        let mut p = Parser::new(s);
        p.ls.next();
        println!("eval: {:?}", p.ls.current());
        return p.statlist();
    }

    pub fn new(input: String) -> Parser {
        return Parser {
            ls: LexState::new(input),
            fs: FuncState::new(),
        };
    }

    fn statlist(&mut self) -> ParserResult<()> {
        /* statlist -> { stat [';'] } */
        while !self.block_follow(true) {
            if self.ls.current() == Token::Return {
                /* 'return' must be last statement */
                return self.statement();
            }
            self.statement()?;
        }
        println!("statlist => {:?}", self.ls.current());
        return Ok(());
    }

    /*
     ** check whether current token is in the follow set of a block.
     ** 'until' closes syntactical blocks, but do not close scope,
     ** so it is handled in separate.
     */
    fn block_follow(&mut self, withuntil: bool) -> bool {
        use Token::*;
        return match self.ls.current() {
            Else => true,
            ElseIf => true,
            Eos => true,
            Until => withuntil,
            _ => false,
        };
    }

    fn statement(&mut self) -> ParserResult<()> {
        use Token::*;
        println!("statement => {:?}", self.ls.current());
        //int line = ls->linenumber;  /* may be needed for error messages */
        //enterlevel(ls); /* increment stack */
        match self.ls.current() {
            Char(';') => {
                /* stat -> ';' (empty statement) */
                self.ls.next(); /* skip ';' */
            }
            If => {
                /* stat -> ifstat */
                self.ifstat(self.ls.linenumber())?;
            }
            While => { /* stat -> whilestat */
                //whilestat(ls, line);
            }
            Do => {
                /* stat -> DO block END */
                self.ls.next(); /* skip DO */
                self.block();
                self.expect_next(Char('}'))?;
            }
            For => { /* stat -> forstat */
                //forstat(ls, line);
            }
            Repeat => { /* stat -> repeatstat */
                //repeatstat(ls, line);
            }
            Function => { /* stat -> funcstat */
                //funcstat(ls, line);
            }
            Let => {
                /* stat -> localstat */
                self.ls.next(); /* skip LOCAL */
                //if (testnext(ls, TK_FUNCTION))  /* local function? */
                //  localfunc(ls);
                //else
                self.letstat()?;
            }
            Dbcolon => {
                /* stat -> label */
                self.ls.next(); /* skip double colon */
                //labelstat(ls, str_checkname(ls), line);
            }
            Return => {
                /* stat -> retstat */
                self.ls.next(); /* skip RETURN */
                //retstat(ls);
            }
            Break => { /* stat -> breakstat */
                //breakstat(ls);
            }
            Goto => {
                /* stat -> 'goto' NAME */
                self.ls.next(); /* skip 'goto' */
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

    /* stat -> LOCAL NAME ATTRIB { ',' NAME ATTRIB } ['=' explist] */
    fn letstat(&mut self) -> ParserResult<()> {
        self.new_localvar()?;
        let mut exp = ExpDesc::new();
        self.expect_next(Token::Char('='))?;
        self.expression(&mut exp);
        println!("let expr => {:?}", exp);
        let const_var = exp.try_into_value();
        if let Some(v) = const_var {
            let var = self.fs.last().unwrap();
            var.value = v;
            println!("new var => {:?}", var);
        }
        return Ok(());
    }

    /*
     ** Create a new local variable with the given 'name'
     */
    fn new_localvar(&mut self) -> ParserResult<()> {
        if let Token::Name(name) = self.ls.current() {
            self.fs.register(VarDesc::new(name, VarKind::Regular));
        } else {
            return Err(ParsingError::new(
                self.ls.linenumber(),
                Token::Name("<name>".to_owned()),
                self.ls.current(),
            ));
        }
        self.ls.next();
        return Ok(());
    }

    /* Parse list of expression, return the number of expression in the list */
    /* explist -> expression { ',' expression } */
    fn explist(&mut self, exp: &mut ExpDesc) -> u8 {
        let mut n = 1; /* at least one expression */
        self.expression(exp);
        while self.ls.next_if_char(',') {
            //luaK_exp2nextreg(ls->fs, v);
            self.expression(exp);
            n += 1;
        }
        return n;
    }

    fn expression(&mut self, exp: &mut ExpDesc) {
        self.subexpression(0, exp);
    }

    /*
     ** subexpression -> (simpleexp | unop subexpression) { binop subexpression }
     ** where 'binop' is any binary operator with a priority higher than 'limit'
     */
    fn subexpression(&mut self, limit: u8, exp: &mut ExpDesc) -> ParserResult<Option<BinOpr>> {
        //self.enterlevel(); // incre recursive calls to prevent stack overflow ?
        if let Some(uop) = UnOpr::try_from(&self.ls.current()) {
            /* prefix (unary) operator? */
            self.ls.next(); /* skip operator */
            self.subexpression(uop.priority(), exp)?;
            self.apply_uop(uop, exp)?;
        } else {
            self.simpleexp(exp);
        }
        /* expand while operators have priorities higher than 'limit' */
        println!("current: {:?}", self.ls.current());
        let mut opt = BinOpr::try_from(&self.ls.current());
        println!("binop: {:?}", opt);
        while opt.is_some() {
            let op = opt.take().unwrap();
            if op.left_priority() <= limit {
                break;
            }
            println!("binop: {:?}", op);
            self.ls.next(); /* skip operator */
            /* read sub-expression with higher priority */
            let mut right_exp = ExpDesc::new();
            opt = self.subexpression(op.right_priority(), &mut right_exp)?;
            /* Apply the operator */
            self.apply_binop(op, exp, right_exp);
        }
        //leavelevel(ls);
        return Ok(opt); /* return first untreated operator */
    }

    fn apply_binop(
        &mut self,
        op: BinOpr,
        left_exp: &mut ExpDesc,
        right_exp: ExpDesc,
    ) -> ParserResult<()> {
        return Ok(());
    }

    fn apply_uop(&mut self, op: UnOpr, exp: &mut ExpDesc) -> ParserResult<()> {
        return match exp.literal_value {
            Some(Value::Int(i)) => {
                exp.literal_value = Some(Value::Int(-i));
                Ok(())
            }
            Some(Value::Number(n)) => {
                exp.literal_value = Some(Value::Number(-n));
                Ok(())
            }
            _ => Err(ParsingError::new(
                self.ls.linenumber(),
                self.ls.current(),
                self.ls.current(),
            )),
        };
    }

    /* simpleexp -> FLT | INT | STRING | NIL | TRUE | FALSE | ... | constructor | FUNCTION body | suffixedexp */
    fn simpleexp(&mut self, exp: &mut ExpDesc) {
        use ExpKind::*;
        match self.ls.current() {
            Token::Flt(literal) => exp.init(ConstFlt, Value::try_from(literal)),
            Token::Int(literal) => exp.init(ConstInt, Value::try_from(literal)),
            Token::Nil => exp.init(Nil, None),
            Token::True => exp.init(True, None),
            Token::False => exp.init(False, None),
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

    /* ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END */
    fn ifstat(&mut self, line: usize) -> ParserResult<()> {
        //let escapelist = NO_JUMP;  /* exit list for finished parts */
        self.test_then_block()?; /* IF cond THEN block */
        while self.ls.current() == Token::ElseIf {
            self.test_then_block()?; /* ELSEIF cond THEN block */
        }
        if self.ls.next_if_token(Token::Else) {
            self.block(); /* 'else' part */
        }
        self.expect_next(Token::Char('}'))?;
        //luaK_patchtohere(fs, escapelist);  /* patch escape list to 'if' end */
        return Ok(());
    }

    /* test_then_block -> [IF | ELSEIF] cond THEN block */
    fn test_then_block(&mut self) -> ParserResult<()> {
        //expdesc v;
        //int jf;  /* instruction to skip 'then' code (if condition is false) */
        self.ls.next(); /* skip IF or ELSEIF */
        let mut cond_expr = ExpDesc::new();
        self.expression(&mut cond_expr); /* read condition */
        self.expect_next(Token::Char('{'))?;
        if self.ls.current() == Token::Break {
            /* 'if x then break' ? */
            //let line = self.ls.linenumber();
            //luaK_goiffalse(ls->fs, &v);  /* will jump if condition is true */
            self.ls.next(); /* skip 'break' */
            self.fs.enter_block(false); /* must enter block before 'goto' */
            //newgotoentry(ls, luaS_newliteral(ls->L, "break"), line, v.t);
            while self.ls.next_if_char(';') {} /* skip semicolons */
            if self.block_follow(false) {
                /* jump is the entire block? */
                self.fs.leave_block();
                return Ok(()); /* and that is it */
            } else { /* must skip over 'then' part if condition is false */
                //jf = luaK_jump(fs);
            }
        } else {
            /* regular case (not a break) */
            //luaK_goiftrue(ls->fs, &v);  /* skip over block if condition is false */
            self.fs.enter_block(false);
            //jf = v.f;
        }
        self.statlist(); /* 'then' part */
        self.fs.leave_block();
        if self.ls.current() == Token::Else || self.ls.current() == Token::ElseIf { /* followed by 'else'/'elseif'? */
            //luaK_concat(fs, escapelist, luaK_jump(fs));  /* must jump over it */
        }
        //luaK_patchtohere(fs, jf);
        return Ok(());
    }

    fn expect_next(&mut self, t: Token) -> ParserResult<()> {
        if !self.ls.next_if_token(t.clone()) {
            return Err(ParsingError::new(
                self.ls.linenumber(),
                t,
                self.ls.current(),
            ));
        }
        return Ok(());
    }

    fn expect_current(&mut self, t: Token) -> ParserResult<()> {
        if std::mem::discriminant(&self.ls.current()) != std::mem::discriminant(&t) {
            return Err(ParsingError::new(
                self.ls.linenumber(),
                t,
                self.ls.current(),
            ));
        }
        return Ok(());
    }

    fn block(&mut self) {
        /* block -> statlist */
        //FuncState *fs = ls->fs;
        //BlockCnt bl;
        //enterblock(fs, &bl, 0);
        self.statlist();
        //leaveblock(fs);
    }
}
