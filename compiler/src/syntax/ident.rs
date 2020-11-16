macro_rules! ident_type {
    ($type_name:ident) => {
        #[derive(Clone, Copy, Eq, Hash, PartialEq)]
        pub struct $type_name(lalrpop_intern::InternedString);

        impl $type_name {
            pub fn new(x: &str) -> Self {
                Self(lalrpop_intern::intern(x))
            }

            pub fn with_name<R, F>(&self, f: F) -> R
            where
                F: FnOnce(&str) -> R,
            {
                lalrpop_intern::read(|interner| f(interner.data(self.0)))
            }
        }

        impl std::fmt::Display for $type_name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        impl std::fmt::Debug for $type_name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_fmt(format_args!("{}({:?})", stringify!($type_name), self.0))
            }
        }
    };
}
