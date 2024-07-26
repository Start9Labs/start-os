pub struct SyncMutex<T>(std::sync::Mutex<T>);
impl<T> SyncMutex<T> {
    pub fn new(t: T) -> Self {
        Self(std::sync::Mutex::new(t))
    }
    pub fn mutate<F: FnOnce(&mut T) -> U, U>(&self, f: F) -> U {
        f(&mut *self.0.lock().unwrap())
    }
}
