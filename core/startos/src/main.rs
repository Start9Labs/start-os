fn main() {
    #[cfg(feature = "backtrace-on-stack-overflow")]
    unsafe {
        backtrace_on_stack_overflow::enable()
    };
    startos::bins::startbox()
}
