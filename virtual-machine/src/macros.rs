// Pulled from https://stackoverflow.com/a/54431057/15827495
#[cfg(debug_assertions)]
#[macro_export]
macro_rules! debug {
    ($x:expr) => {
        dbg!($x)
    };
}
