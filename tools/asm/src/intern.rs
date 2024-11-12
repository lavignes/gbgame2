use std::{ffi::OsStr, ops::Range, path::Path, slice, str};

pub struct SliceInt<T> {
    pub storages: Vec<Vec<T>>,
}

impl<'a, T> SliceInt<T>
where
    T: PartialEq + Eq + Clone,
{
    pub fn new() -> Self {
        Self {
            storages: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.storages
            .iter()
            .fold(0, |accum, storage| accum + storage.len())
    }

    pub fn iter(&'a self) -> impl Iterator<Item = &'a [T]> {
        self.storages.iter().map(|storage| storage.as_slice())
    }

    pub fn offset(&self, values: &[T]) -> Option<usize> {
        let mut global_index = 0;
        for storage in self.storages.iter() {
            if let Some(index) = storage.windows(values.len()).position(|win| win == values) {
                return Some(global_index + index);
            }
            global_index += storage.len();
        }
        None
    }

    pub fn slice(&'a self, range: Range<usize>) -> Option<&'a [T]> {
        let mut global_index = 0;
        for storage in self.storages.iter() {
            let storage_range = global_index..(global_index + storage.len());
            if storage_range.contains(&range.start) && storage_range.contains(&(range.end - 1)) {
                return Some(&storage[range]);
            }
            global_index += storage.len();
        }
        None
    }

    pub fn intern(&mut self, values: &[T]) -> &'a [T] {
        let mut has_space = None;
        for (i, storage) in self.storages.iter().enumerate() {
            // we can always fit an empty slice
            if values.is_empty() {
                // SAFETY: the assumption is that we never re-allocate storages
                unsafe {
                    return slice::from_raw_parts(storage.as_ptr(), 0);
                }
            }
            // pre-check if we have space for the items in case we have a cache miss
            if has_space.is_none() && ((storage.capacity() - storage.len()) >= values.len()) {
                has_space = Some(i);
            }
            if let Some(index) = storage.windows(values.len()).position(|win| win == values) {
                // SAFETY: the assumption is that we never re-allocate storages
                unsafe {
                    return slice::from_raw_parts(storage.as_ptr().add(index), values.len());
                }
            }
        }
        // cache miss, add to a storage if possible
        let storage = if let Some(index) = has_space {
            &mut self.storages[index]
        } else {
            self.storages
                .push(Vec::with_capacity(values.len().max(256)));
            self.storages.last_mut().unwrap()
        };
        let index = storage.len();
        storage.extend_from_slice(values);
        // SAFETY: the assumption is that we never re-allocate storages
        unsafe { slice::from_raw_parts(storage.as_ptr().add(index), values.len()) }
    }
}

pub struct StrInt(SliceInt<u8>);

impl<'a> StrInt {
    pub fn new() -> Self {
        Self(SliceInt::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&'a self) -> impl Iterator<Item = &'a str> {
        self.0
            .iter()
            .map(|bytes| unsafe { str::from_utf8_unchecked(bytes) })
    }

    pub fn offset<S: AsRef<str>>(&self, string: S) -> Option<usize> {
        self.0.offset(string.as_ref().as_bytes())
    }

    pub fn slice(&'a self, range: Range<usize>) -> Option<&'a str> {
        self.0
            .slice(range)
            // SAFETY: what goes in, goes out
            .map(|bytes| unsafe { str::from_utf8_unchecked(bytes) })
    }

    pub fn intern<S: AsRef<str>>(&mut self, string: S) -> &'a str {
        // SAFETY: what goes in, goes out
        unsafe { str::from_utf8_unchecked(self.0.intern(string.as_ref().as_bytes())) }
    }
}

pub struct PathInt(SliceInt<u8>);

impl<'a> PathInt {
    pub fn new() -> Self {
        Self(SliceInt::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&'a self) -> impl Iterator<Item = &'a OsStr> {
        self.0
            .iter()
            .map(|bytes| unsafe { OsStr::from_encoded_bytes_unchecked(bytes) })
    }

    pub fn offset<P: AsRef<Path>>(&self, path: P) -> Option<usize> {
        self.0.offset(path.as_ref().as_os_str().as_encoded_bytes())
    }

    pub fn slice(&'a self, range: Range<usize>) -> Option<&'a Path> {
        self.0
            .slice(range)
            // SAFETY: what goes in, goes out
            .map(|bytes| Path::new(unsafe { OsStr::from_encoded_bytes_unchecked(bytes) }))
    }

    pub fn intern<P: AsRef<Path>>(&mut self, path: P) -> &'a Path {
        // SAFETY: what goes in, goes out
        Path::new(unsafe {
            OsStr::from_encoded_bytes_unchecked(
                self.0.intern(path.as_ref().as_os_str().as_encoded_bytes()),
            )
        })
    }
}
