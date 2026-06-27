mod debug;

pub use debug::Debug;
pub use debug::DebugWriter;
pub(crate) use debug::derive_fmt_debug;

// TODO: Inline the debug module and move this up as ast.rs.
