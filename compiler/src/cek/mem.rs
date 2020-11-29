use crate::anf;
use crate::util::in_parens_if_some;
use std::fmt;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u16)]
pub enum Tag {
    Null = 0,
    Int,
    Bool,
    Record,
    Variant,
    Closure,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Header {
    pub tag: Tag,
    pub rank: u16,
    pub size: u32,
}

#[derive(Clone, Copy)]
pub struct Addr(usize);

#[derive(Clone, Copy)]
pub union Data<'a> {
    header: Header,
    addr: Addr,
    int: i64,
    make_closure: &'a anf::MakeClosure,
}

assert_eq_size!(Header, usize);
assert_eq_size!(Addr, usize);
assert_eq_size!(i64, usize);
assert_eq_size!(&'_ anf::Expr, usize);
assert_eq_size!(Data<'_>, usize);

#[derive(Clone)]
pub struct Memory<'a> {
    data: Vec<Data<'a>>,
    phantom: std::marker::PhantomData<&'a anf::Expr>,
}

impl Header {
    const NULL: Self = Self::new(Tag::Null, 0, 0);

    const fn new(tag: Tag, rank: u16, size: u32) -> Self {
        Self { tag, rank, size }
    }
}

impl Addr {
    pub const NULL: Self = Self(0);
}

impl<'a> Data<'a> {
    const NULL: Self = Self { header: Header::NULL };

    pub fn from_addr(addr: Addr) -> Self {
        Self { addr }
    }

    pub fn from_int(int: i64) -> Self {
        Self { int }
    }

    pub fn from_make_closure(make_closure: &'a anf::MakeClosure) -> Self {
        Self { make_closure }
    }

    pub fn into_header(self) -> Header {
        unsafe { self.header }
    }

    pub fn into_addr(self) -> Addr {
        unsafe { self.addr }
    }

    pub fn into_int(self) -> i64 {
        unsafe { self.int }
    }

    pub fn into_make_closure(self) -> &'a anf::MakeClosure {
        unsafe { self.make_closure }
    }
}

impl<'a> Memory<'a> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self { data: vec![Data::NULL], phantom: std::marker::PhantomData }
    }

    // #[inline(never)]
    pub fn alloc(&mut self, tag: Tag, rank: u16, size: u32) -> Addr {
        let len = self.data.len();
        self.data.resize(len + 1 + size as usize, Data::NULL);
        let addr = Addr(len);
        self[addr] = Data { header: Header::new(tag, rank, size) };
        addr
    }

    pub fn value_at(&'a self, addr: Addr) -> Value<'a> {
        Value { memory: self, addr }
    }
}

impl std::ops::Add<u32> for Addr {
    type Output = Self;

    fn add(self, offset: u32) -> Self {
        Addr(self.0 + offset as usize)
    }
}

impl<'a> std::ops::Index<Addr> for Memory<'a> {
    type Output = Data<'a>;

    fn index(&self, addr: Addr) -> &Data<'a> {
        &self.data[addr.0]
    }
}

impl<'a> std::ops::IndexMut<Addr> for Memory<'a> {
    fn index_mut(&mut self, addr: Addr) -> &mut Data<'a> {
        &mut self.data[addr.0]
    }
}

#[derive(Clone, Copy)]
pub struct Value<'a> {
    memory: &'a Memory<'a>,
    addr: Addr,
}

impl<'a> Value<'a> {
    fn deref_at_offset(&self, offset: u32) -> Self {
        let Self { memory, addr } = *self;
        Self { memory, addr: memory[addr + offset].into_addr() }
    }
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use join_lazy_fmt::*;
        let Self { memory, addr } = *self;
        let header = memory[addr].into_header();
        match header.tag {
            Tag::Null => panic!("uninitalized memory"),
            Tag::Int => write!(f, "{}", memory[addr + 1].into_int()),
            Tag::Bool => write!(f, "{}", header.rank != 0),
            Tag::Record => write!(
                f,
                "{{{}}}",
                ", ".join((0..header.size).map(|index| {
                    lazy_format!("_{} = {}", index, self.deref_at_offset(index + 1))
                }))
            ),
            Tag::Variant => {
                let payload = if header.size == 0 { None } else { Some(self.deref_at_offset(1)) };
                write!(f, "#{}{}", header.rank, in_parens_if_some(&payload))
            }
            Tag::Closure => {
                let anf::MakeClosure { captured, params, body: _ } =
                    memory[addr + header.size].into_make_closure();
                write!(
                    f,
                    "[{}; fn ({}) {{ ... }}]",
                    ", ".join(captured.iter().zip(0..).map(|(var, index)| {
                        lazy_format!("{} = {}", var.1, self.deref_at_offset(index + 1))
                    })),
                    ", ".join(params),
                )
            }
        }
    }
}
