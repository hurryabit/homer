macro_rules! ident_type {
    ($type_name:ident) => {
        #[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct $type_name(lasso::Spur);

        impl $type_name {
            pub fn new(x: &str) -> Self {
                Self(crate::INTERNER.get_or_intern(x))
            }

            pub fn as_str(&self) -> &'static str {
                crate::INTERNER.resolve(&self.0)
            }
        }

        impl std::fmt::Display for $type_name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", self.as_str())
            }
        }

        impl std::fmt::Debug for $type_name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_fmt(format_args!(
                    "{}({:?})",
                    stringify!($type_name),
                    self.as_str(),
                ))
            }
        }
    };
}
