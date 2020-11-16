use super::*;
use crate::location;
use std::fmt;

type Span = location::Span<location::ParserLoc>;

impl Debug for Module {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self { decls } = self;
        writer.node("MODULE", |writer| {
            for decl in decls {
                writer.child("decl", decl)?;
            }
            Ok(())
        })
    }
}

impl Debug for Decl {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        use Decl::*;
        match self {
            Type(decl) => decl.write(writer),
            Func(decl) => decl.write(writer),
        }
    }
}

impl Debug for TypeDecl {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self { name, params, body } = self;
        writer.node("TYPEDECL", |writer| {
            writer.child("name", name)?;
            for param in params {
                writer.child("type_param", param)?;
            }
            writer.child("type", body)
        })
    }
}

impl Debug for FuncDecl {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self {
            name,
            type_params,
            expr_params,
            return_type,
            body,
        } = self;
        writer.node("FUNCDECL", |writer| {
            writer.child("name", name)?;
            for type_param in type_params {
                writer.child("type_param", type_param)?;
            }
            for (param, typ) in expr_params {
                writer.child("param", param)?;
                writer.child("type", typ)?;
            }
            writer.child("result", return_type)?;
            writer.child("body", body)
        })
    }
}

impl Debug for TypeVar {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        self.with_name(|name| writer.leaf(name))
    }
}

impl Debug for Type {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        use Type::*;
        match self {
            Error => writer.leaf("ERROR"),
            Var(name) => name.write(writer),
            SynApp(syn, args) => writer.node("APP", |writer| {
                writer.child("syn", syn)?;
                for arg in args {
                    writer.child("type_arg", arg)?;
                }
                Ok(())
            }),
            Int => writer.leaf("INT"),
            Bool => writer.leaf("BOOL"),
            Fun(params, result) => writer.node("FUN", |writer| {
                for param in params {
                    writer.child("param", param)?;
                }
                writer.child("result", result)
            }),
            Record(fields) => writer.node("RECORD", |writer| {
                for (field, typ) in fields {
                    writer.child("field", field)?;
                    writer.child("type", typ)?;
                }
                Ok(())
            }),
            Variant(constrs) => writer.node("VARIANT", |writer| {
                for (constr, opt_typ) in constrs {
                    writer.child("constr", constr)?;
                    if let Some(typ) = opt_typ {
                        writer.child("type", typ)?;
                    }
                }
                Ok(())
            }),
        }
    }
}

impl Debug for ExprVar {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        self.with_name(|name| writer.leaf(name))
    }
}

impl Debug for ExprCon {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        self.with_name(|name| writer.leaf(name))
    }
}

impl Debug for Expr {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        use Expr::*;
        match self {
            Error => writer.leaf("ERROR"),
            Var(name) => name.write(writer),
            Num(n) => writer.leaf(&n.to_string()),
            Bool(b) => writer.leaf(&b.to_string()),
            Lam(params, body) => writer.node("LAM", |writer| {
                for (param, opt_typ) in params {
                    writer.child("param", param)?;
                    if let Some(typ) = opt_typ {
                        writer.child("type", typ)?;
                    }
                }
                writer.child("body", body)
            }),
            App(fun, args) => writer.node("APP", |writer| {
                writer.child("fun", fun)?;
                for arg in args {
                    writer.child("arg", arg)?;
                }
                Ok(())
            }),
            BinOp(lhs, op, rhs) => writer.node("BINOP", |writer| {
                writer.child("lhs", lhs)?;
                writer.child("op", op)?;
                writer.child("rhs", rhs)
            }),
            FuncInst(fun, types) => writer.node("FUNCINST", |writer| {
                writer.child("fun", fun)?;
                for typ in types {
                    writer.child("type_arg", typ)?;
                }
                Ok(())
            }),
            Let(binder, opt_typ, bindee, body) => writer.node("LET", |writer| {
                writer.child("binder", binder)?;
                if let Some(typ) = opt_typ {
                    writer.child("type", typ)?;
                }
                writer.child("bindee", bindee)?;
                writer.child("body", body)
            }),
            If(cond, then, elze) => writer.node("IF", |writer| {
                writer.child("cond", cond)?;
                writer.child("then", then)?;
                writer.child("else", elze)
            }),
            Record(fields) => writer.node("RECORD", |writer| {
                for (field, value) in fields {
                    writer.child("field", field)?;
                    writer.child("value", value)?;
                }
                Ok(())
            }),
            Proj(record, field) => writer.node("PROJ", |writer| {
                writer.child("record", record)?;
                writer.child("field", field)
            }),
            Variant(constr, opt_payload) => writer.node("VARIANT", |writer| {
                writer.child("constr", constr)?;
                if let Some(payload) = opt_payload {
                    writer.child("payload", payload)?;
                }
                Ok(())
            }),
            Match(scrut, branches) => writer.node("MATCH", |writer| {
                writer.child("scrut", scrut)?;
                for branch in branches {
                    writer.child("branch", branch)?;
                }
                Ok(())
            }),
        }
    }
}

impl Debug for Branch {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self { pattern, body: rhs } = self;
        writer.node("BRANCH", |writer| {
            writer.child("pattern", pattern)?;
            writer.child("body", rhs)
        })
    }
}

impl Debug for Pattern {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self { constr, binder } = self;
        writer.node("PATTERN", |writer| {
            writer.child("constr", constr)?;
            if let Some(binder) = binder {
                writer.child("binder", binder)?;
            }
            Ok(())
        })
    }
}

impl Debug for OpCode {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        use OpCode::*;
        match self {
            Add => writer.leaf("ADD"),
            Sub => writer.leaf("SUB"),
            Mul => writer.leaf("MUL"),
            Div => writer.leaf("DIV"),
            Equals => writer.leaf("EQUALS"),
            NotEq => writer.leaf("NOTEQ"),
            Less => writer.leaf("LESS"),
            LessEq => writer.leaf("LESSEQ"),
            Greater => writer.leaf("GREATER"),
            GreaterEq => writer.leaf("GREATEREQ"),
        }
    }
}

pub trait Debug {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result;
}

impl<T: Debug> Debug for Located<T> {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        writer.set_next_span(self.span);
        self.locatee.write(writer)
    }
}

impl<T: Debug> Debug for Box<T> {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        self.as_ref().write(writer)
    }
}

pub struct DebugWriter<'a> {
    writer: &'a mut dyn fmt::Write,
    indent_level: usize,
    next_span: Option<Span>,
}

impl<'a> DebugWriter<'a> {
    const INDENT_SIZE: usize = 4;

    pub fn new(writer: &'a mut dyn fmt::Write) -> Self {
        Self {
            writer,
            indent_level: 0,
            next_span: None,
        }
    }

    pub fn fmt(debug: &dyn Debug, writer: &'a mut dyn fmt::Write) -> fmt::Result {
        debug.write(&mut Self::new(writer))
    }

    pub fn set_next_span(&mut self, span: Span) {
        self.next_span = Some(span)
    }

    pub fn leaf(&mut self, label: &str) -> fmt::Result {
        if let Some(span) = self.next_span.take() {
            write!(self.writer, "{} @ {:?}...{:?}", label, span.start, span.end)
        } else {
            self.writer.write_str(label)
        }
    }

    pub fn node<F>(&mut self, label: &str, f: F) -> fmt::Result
    where
        F: Fn(&mut Self) -> fmt::Result,
    {
        self.leaf(label)?;
        self.indent_level += 1;
        f(self)?;
        self.indent_level -= 1;
        Ok(())
    }

    pub fn child(&mut self, label: &str, item: &impl Debug) -> fmt::Result {
        self.writer.write_char('\n')?;
        self.indent()?;
        write!(self.writer, "{}: ", label)?;
        item.write(self)
    }

    fn indent(&mut self) -> fmt::Result {
        self.writer
            .write_str(&" ".repeat(Self::INDENT_SIZE * self.indent_level))
    }
}
