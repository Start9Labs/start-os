pub trait TransposeResultIterExt: Sized {
    type Ok: IntoIterator;
    type Err;
    fn transpose(self) -> TransposeResult<Self::Ok, Self::Err>;
}
impl<T: IntoIterator, E> TransposeResultIterExt for Result<T, E> {
    type Ok = T;
    type Err = E;
    fn transpose(self) -> TransposeResult<T, E> {
        TransposeResult(Some(self.map(|t| t.into_iter())))
    }
}

pub struct TransposeResult<T: IntoIterator, E>(Option<Result<T::IntoIter, E>>);
impl<T, E> Iterator for TransposeResult<T, E>
where
    T: IntoIterator,
{
    type Item = Result<T::Item, E>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.0.as_ref().map_or(true, |r| r.is_err()) {
            self.0
                .take()
                .map(|e| Err(e.map_or_else(|e| e, |_| unreachable!())))
        } else if let Some(Ok(res)) = &mut self.0 {
            res.next().map(Ok)
        } else {
            None
        }
    }
}
