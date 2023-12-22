use crate::location::{Located, SourceSpan};
use std::fmt;

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

impl<T: Debug> Debug for std::rc::Rc<T> {
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

impl Debug for SourceSpan {
    fn write(&self, writer: &mut DebugWriter) -> fmt::Result {
        write!(writer.writer, "{:?}-{:?}", self.start, self.end)
    }
}

pub struct DebugWriter<'a> {
    writer: &'a mut dyn fmt::Write,
    indent_level: usize,
    next_span: Option<SourceSpan>,
}

impl<'a> DebugWriter<'a> {
    const INDENT_SIZE: usize = 4;

    pub fn new(writer: &'a mut dyn fmt::Write) -> Self {
        Self { writer, indent_level: 0, next_span: None }
    }

    pub fn fmt(debug: &dyn Debug, writer: &'a mut dyn fmt::Write) -> fmt::Result {
        debug.write(&mut Self::new(writer))
    }

    pub fn set_next_span(&mut self, span: SourceSpan) {
        self.next_span = Some(span)
    }

    pub fn leaf(&mut self, label: &str) -> fmt::Result {
        match self.next_span.take() {
            None => self.writer.write_str(label),
            Some(span) => write!(self.writer, "{} @ {:?}-{:?}", label, span.start, span.end),
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

    pub fn children<T, I>(&mut self, label: &str, items: I) -> fmt::Result
    where
        T: Debug,
        I: IntoIterator<Item = T>,
    {
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
        self.writer.write_str(&" ".repeat(Self::INDENT_SIZE * self.indent_level))
    }
}

#[macro_export]
macro_rules! derive_fmt_debug {
    ($type_name:ident) => {
        impl std::fmt::Debug for $type_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                $crate::ast::DebugWriter::fmt(self, f)
            }
        }
    };
}
