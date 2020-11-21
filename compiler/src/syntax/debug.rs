use super::*;
use crate::location;
use std::fmt;

type Span = location::Span<location::ParserLoc>;

impl Debug for Module {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self { decls } = self;
        writer.node("MODULE", |writer| writer.children("decl", decls))
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
            writer.children("type_param", params)?;
            writer.child("body", body)
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
            writer.children("type_param", type_params)?;
            writer.children_pair("param", "type", expr_params)?;
            writer.child("result", return_type)?;
            writer.child("body", body)
        })
    }
}

impl Debug for TypeVar {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        writer.leaf(self.as_str())
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
                writer.children("type_arg", args)
            }),
            Int => writer.leaf("INT"),
            Bool => writer.leaf("BOOL"),
            Fun(params, result) => writer.node("FUN", |writer| {
                writer.children("param", params)?;
                writer.child("result", result)
            }),
            Record(fields) => writer.node("RECORD", |writer| {
                writer.children_pair("field", "type", fields)
            }),
            Variant(constrs) => writer.node("VARIANT", |writer| {
                for (constr, opt_typ) in constrs {
                    writer.child("constr", constr)?;
                    writer.child_if_some("type", opt_typ)?;
                }
                Ok(())
            }),
        }
    }
}

impl Debug for ExprVar {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        writer.leaf(self.as_str())
    }
}

impl Debug for (ExprVar, u32) {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        writer.leaf(&format!("{}/{}", self.0.as_str(), self.1))
    }
}

impl Debug for (ExprVar, Option<u32>) {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        match self.1 {
            None => self.0.write(writer),
            Some(n) => (self.0, n).write(writer),
        }
    }
}

impl Debug for ExprCon {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        writer.leaf(self.as_str())
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
                    writer.child_if_some("type", opt_typ)?;
                }
                writer.child("body", body)
            }),
            App(fun, args) => writer.node("APP", |writer| {
                writer.child("fun", fun)?;
                writer.children("arg", args)
            }),
            BinOp(lhs, op, rhs) => writer.node("BINOP", |writer| {
                writer.child("lhs", lhs)?;
                writer.child("op", op)?;
                writer.child("rhs", rhs)
            }),
            FuncInst(fun, types) => writer.node("FUNCINST", |writer| {
                writer.child("fun", fun)?;
                writer.children("type_arg", types)
            }),
            Let(binder, opt_typ, bindee, tail) => writer.node("LET", |writer| {
                writer.child("binder", binder)?;
                writer.child_if_some("type", opt_typ)?;
                writer.child("bindee", bindee)?;
                writer.child("tail", tail)
            }),
            If(cond, then, elze) => writer.node("IF", |writer| {
                writer.child("cond", cond)?;
                writer.child("then", then)?;
                writer.child("else", elze)
            }),
            Record(fields) => writer.node("RECORD", |writer| {
                writer.children_pair("field", "value", fields)
            }),
            Proj(record, field, index) => writer.node("PROJ", |writer| {
                writer.child("record", record)?;
                writer.child("field", &field.map(|f| (f, *index)))
            }),
            Variant(constr, opt_payload) => writer.node("VARIANT", |writer| {
                writer.child("constr", constr)?;
                writer.child_if_some("payload", opt_payload)?;
                Ok(())
            }),
            Match(scrut, branches) => writer.node("MATCH", |writer| {
                writer.child("scrut", scrut)?;
                writer.children("branch", branches)
            }),
        }
    }
}

impl Debug for Branch {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        let Self { pattern, rhs } = self;
        writer.node("BRANCH", |writer| {
            writer.child("pattern", pattern)?;
            writer.child("rhs", rhs)
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

impl<T: Debug> Debug for &T {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        (*self).write(writer)
    }
}

impl Debug for String {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        write!(writer.writer, "{}", self)
    }
}

impl Debug for u32 {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        write!(writer.writer, "{}", self)
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

    pub fn child<T: Debug>(&mut self, label: &str, item: &T) -> fmt::Result {
        self.writer.write_char('\n')?;
        self.indent()?;
        write!(self.writer, "{}: ", label)?;
        item.write(self)
    }

    pub fn child_if_some<T: Debug>(&mut self, label: &str, opt_item: &Option<T>) -> fmt::Result {
        if let Some(item) = opt_item {
            self.child(label, item)?;
        }
        Ok(())
    }

    pub fn children<T: Debug, I: IntoIterator<Item = T>>(&mut self, label: &str, items: I) -> fmt::Result {
        for item in items {
            self.child(label, &item)?;
        }
        Ok(())
    }

    pub fn children_pair<T: Debug, U: Debug>(
        &mut self,
        label1: &str,
        label2: &str,
        items: &[(T, U)],
    ) -> fmt::Result {
        for (item1, item2) in items {
            self.child(label1, item1)?;
            self.child(label2, item2)?;
        }
        Ok(())
    }

    fn indent(&mut self) -> fmt::Result {
        self.writer
            .write_str(&" ".repeat(Self::INDENT_SIZE * self.indent_level))
    }
}

#[macro_export]
macro_rules! derive_fmt_debug {
    ($type_name:ident) => {
        impl fmt::Debug for $type_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                DebugWriter::fmt(self, f)
            }
        }
    };
}
