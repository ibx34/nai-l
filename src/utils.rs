#[derive(Debug)]
pub struct Cursor<T> {
    pub idx: usize,
    pub len: usize,
    pub interior: Vec<T>,
}

impl<T> Cursor<T> {
    pub fn init(interior: Vec<T>) -> Self {
        Self {
            idx: 0,
            len: interior.len(),
            interior,
        }
    }
    pub fn current(&self) -> Option<&T> {
        self.interior.get(self.idx)
    }
    pub fn next(&mut self) -> Option<&T> {
        self.idx += 1;
        if self.idx == 0 {
            return self.interior.first();
        }
        return self.interior.get(self.idx);
    }
    pub fn advance_by(&mut self, n: usize) -> bool {
        self.idx += n;
        return self.interior.get(self.idx).is_some();
    }
    pub fn peek(&mut self) -> Option<&T> {
        // TODO: Fix why this is how we peek funky
        return self.interior.get(self.idx + 1);
    }
}
