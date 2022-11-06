#[macro_export]
macro_rules! trace {
    ($($arg: tt)*) => {
        println!("{}:{} {}", file!(), line!(), format!($($arg)*));
    }
}
