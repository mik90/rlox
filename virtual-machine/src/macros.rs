// Only print the given value in debug mode
#[cfg(debug_assertions)]
#[macro_export]
macro_rules! debug {
    ($($arg: tt)*) => {
        print!("[{}:{}] {}", file!(), line!(), format!($($arg)*));
    }
}
