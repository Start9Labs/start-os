use std::borrow::Borrow;
use std::fmt;

pub struct EqMap<K: Eq, V>(Vec<(K, V)>);
impl<K: Eq, V> Default for EqMap<K, V> {
    fn default() -> Self {
        Self(Default::default())
    }
}
impl<K: Eq, V> EqMap<K, V> {
    pub fn clear(&mut self) {
        self.0.clear()
    }

    /// Returns the key-value pair corresponding to the supplied key as a borrowed tuple.
    ///
    /// The supplied key may be any borrowed form of the map's key type, but the equality
    /// on the borrowed form *must* match the equality on the key type.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map = EqMap::new();
    /// map.insert(1, "a");
    /// assert_eq!(map.get_key_value(&1), Some((&1, &"a")));
    /// assert_eq!(map.get_key_value(&2), None);
    /// ```
    pub fn get_key_value_ref<Q: ?Sized>(&self, key: &Q) -> Option<&(K, V)>
    where
        K: Borrow<Q> + Eq,
        Q: Eq,
    {
        self.0.iter().find(|(k, _)| k.borrow() == key)
    }

    /// Returns the key-value pair corresponding to the supplied key as a mutably borrowed tuple.
    ///
    /// The supplied key may be any borrowed form of the map's key type, but the equality
    /// on the borrowed form *must* match the equality on the key type.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map = EqMap::new();
    /// map.insert(1, "a");
    /// assert_eq!(map.get_key_value(&1), Some((&1, &"a")));
    /// assert_eq!(map.get_key_value(&2), None);
    /// ```
    pub fn get_key_value_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut (K, V)>
    where
        K: Borrow<Q> + Eq,
        Q: Eq,
    {
        self.0.iter_mut().find(|(k, _)| k.borrow() == key)
    }

    /// Returns a reference to the value corresponding to the key.
    ///
    /// The key may be any borrowed form of the map's key type, but the equality
    /// on the borrowed form *must* match the equality on the key type.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map = EqMap::new();
    /// map.insert(1, "a");
    /// assert_eq!(map.get(&1), Some(&"a"));
    /// assert_eq!(map.get(&2), None);
    /// ```
    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q> + Eq,
        Q: Eq,
    {
        self.get_key_value_ref(key).map(|(_, v)| v)
    }

    /// Returns the key-value pair corresponding to the supplied key.
    ///
    /// The supplied key may be any borrowed form of the map's key type, but the equality
    /// on the borrowed form *must* match the equality on the key type.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map = EqMap::new();
    /// map.insert(1, "a");
    /// assert_eq!(map.get_key_value(&1), Some((&1, &"a")));
    /// assert_eq!(map.get_key_value(&2), None);
    /// ```
    pub fn get_key_value<Q: ?Sized>(&self, key: &Q) -> Option<(&K, &V)>
    where
        K: Borrow<Q> + Eq,
        Q: Eq,
    {
        self.get_key_value_ref(key).map(|(k, v)| (k, v))
    }

    /// Removes and returns an element in the map.
    /// There is no guarantee about which element this might be
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map = EqMap::new();
    /// map.insert(1, "a");
    /// map.insert(2, "b");
    /// while let Some((_key, _val)) = map.pop() { }
    /// assert!(map.is_empty());
    /// ```
    pub fn pop(&mut self) -> Option<(K, V)>
    where
        K: Eq,
    {
        self.0.pop()
    }

    /// Returns `true` if the map contains a value for the specified key.
    ///
    /// The key may be any borrowed form of the map's key type, but the equality
    /// on the borrowed form *must* match the equality on the key type.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map = EqMap::new();
    /// map.insert(1, "a");
    /// assert_eq!(map.contains_key(&1), true);
    /// assert_eq!(map.contains_key(&2), false);
    /// ```
    pub fn contains_key<Q: ?Sized>(&self, key: &Q) -> bool
    where
        K: Borrow<Q> + Eq,
        Q: Eq,
    {
        self.get(key).is_some()
    }

    /// Returns a mutable reference to the value corresponding to the key.
    ///
    /// The key may be any borrowed form of the map's key type, but the equality
    /// on the borrowed form *must* match the equality on the key type.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map = EqMap::new();
    /// map.insert(1, "a");
    /// if let Some(x) = map.get_mut(&1) {
    ///     *x = "b";
    /// }
    /// assert_eq!(map[&1], "b");
    /// ```
    // See `get` for implementation notes, this is basically a copy-paste with mut's added
    pub fn get_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q> + Eq,
        Q: Eq,
    {
        self.get_key_value_mut(key).map(|(_, v)| v)
    }

    /// Inserts a key-value pair into the map.
    ///
    /// If the map did not have this key present, `None` is returned.
    ///
    /// If the map did have this key present, the value is updated, and the old
    /// value is returned. The key is not updated, though; this matters for
    /// types that can be `==` without being identical.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map = EqMap::new();
    /// assert_eq!(map.insert(37, "a"), None);
    /// assert_eq!(map.is_empty(), false);
    ///
    /// map.insert(37, "b");
    /// assert_eq!(map.insert(37, "c"), Some("b"));
    /// assert_eq!(map[&37], "c");
    /// ```
    pub fn insert(&mut self, key: K, value: V) -> Option<V>
    where
        K: Eq,
    {
        match self.entry(key) {
            Occupied(mut entry) => Some(entry.insert(value)),
            Vacant(entry) => {
                entry.insert(value);
                None
            }
        }
    }

    /// Tries to insert a key-value pair into the map, and returns
    /// a mutable reference to the value in the entry.
    ///
    /// If the map already had this key present, nothing is updated, and
    /// an error containing the occupied entry and the value is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map = EqMap::new();
    /// assert_eq!(map.try_insert(37, "a").unwrap(), &"a");
    ///
    /// let err = map.try_insert(37, "b").unwrap_err();
    /// assert_eq!(err.entry.key(), &37);
    /// assert_eq!(err.entry.get(), &"a");
    /// assert_eq!(err.value, "b");
    /// ```
    pub fn try_insert(&mut self, key: K, value: V) -> Result<&mut V, OccupiedError<'_, K, V>>
    where
        K: Eq,
    {
        match self.entry(key) {
            Occupied(entry) => Err(OccupiedError { entry, value }),
            Vacant(entry) => Ok(entry.insert(value)),
        }
    }

    /// Removes a key from the map, returning the value at the key if the key
    /// was previously in the map.
    ///
    /// The key may be any borrowed form of the map's key type, but the equality
    /// on the borrowed form *must* match the equality on the key type.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map = EqMap::new();
    /// map.insert(1, "a");
    /// assert_eq!(map.remove(&1), Some("a"));
    /// assert_eq!(map.remove(&1), None);
    /// ```
    pub fn remove<Q: ?Sized>(&mut self, key: &Q) -> Option<V>
    where
        K: Borrow<Q> + Eq,
        Q: Eq,
    {
        self.remove_entry(key).map(|(_, v)| v)
    }

    /// Removes a key from the map, returning the stored key and value if the key
    /// was previously in the map.
    ///
    /// The key may be any borrowed form of the map's key type, but the equality
    /// on the borrowed form *must* match the equality on the key type.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map = EqMap::new();
    /// map.insert(1, "a");
    /// assert_eq!(map.remove_entry(&1), Some((1, "a")));
    /// assert_eq!(map.remove_entry(&1), None);
    /// ```
    pub fn remove_entry<Q: ?Sized>(&mut self, key: &Q) -> Option<(K, V)>
    where
        K: Borrow<Q> + Eq,
        Q: Eq,
    {
        self.0
            .iter()
            .enumerate()
            .find(|(_, (k, _))| k.borrow() == key)
            .map(|(idx, _)| idx)
            .map(|idx| self.0.swap_remove(idx))
    }

    /// Retains only the elements specified by the predicate.
    ///
    /// In other words, remove all pairs `(k, v)` for which `f(&k, &mut v)` returns `false`.
    /// The elements are visited in ascending key order.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map: EqMap<i32, i32> = (0..8).map(|x| (x, x*10)).collect();
    /// // Keep only the elements with even-numbered keys.
    /// map.retain(|&k, _| k % 2 == 0);
    /// assert!(map.into_iter().eq(vec![(0, 0), (2, 20), (4, 40), (6, 60)]));
    /// ```
    #[inline]
    pub fn retain<F>(&mut self, mut f: F)
    where
        K: Eq,
        F: FnMut(&K, &mut V) -> bool,
    {
        self.0.retain_mut(|(k, v)| f(k, v))
    }

    /// Moves all elements from `other` into `self`, leaving `other` empty.
    ///
    /// If a key from `other` is already present in `self`, the respective
    /// value from `self` will be overwritten with the respective value from `other`.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut a = EqMap::new();
    /// a.insert(1, "a");
    /// a.insert(2, "b");
    /// a.insert(3, "c"); // Note: Key (3) also present in b.
    ///
    /// let mut b = EqMap::new();
    /// b.insert(3, "d"); // Note: Key (3) also present in a.
    /// b.insert(4, "e");
    /// b.insert(5, "f");
    ///
    /// a.append(&mut b);
    ///
    /// assert_eq!(a.len(), 5);
    /// assert_eq!(b.len(), 0);
    ///
    /// assert_eq!(a[&1], "a");
    /// assert_eq!(a[&2], "b");
    /// assert_eq!(a[&3], "d"); // Note: "c" has been overwritten.
    /// assert_eq!(a[&4], "e");
    /// assert_eq!(a[&5], "f");
    /// ```
    pub fn append(&mut self, other: &mut Self)
    where
        K: Eq,
    {
        for k in other.keys() {
            self.remove(k);
        }
        self.0.append(&mut other.0)
    }

    /// Gets the given key's corresponding entry in the map for in-place manipulation.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut count: EqMap<&str, usize> = EqMap::new();
    ///
    /// // count the number of occurrences of letters in the vec
    /// for x in ["a", "b", "a", "c", "a", "b"] {
    ///     count.entry(x).and_modify(|curr| *curr += 1).or_insert(1);
    /// }
    ///
    /// assert_eq!(count["a"], 3);
    /// assert_eq!(count["b"], 2);
    /// assert_eq!(count["c"], 1);
    /// ```
    pub fn entry(&mut self, key: K) -> Entry<'_, K, V>
    where
        K: Eq,
    {
        match self.0.iter().enumerate().find(|(_, (k, _))| k == &key) {
            Some((idx, _)) => Occupied(OccupiedEntry { map: self, idx }),
            None => Vacant(VacantEntry { key, map: self }),
        }
    }

    /// Creates an iterator that visits all elements (key-value pairs) and
    /// uses a closure to determine if an element should be removed. If the
    /// closure returns `true`, the element is removed from the map and yielded.
    /// If the closure returns `false`, or panics, the element remains in the map
    /// and will not be yielded.
    ///
    /// The iterator also lets you mutate the value of each element in the
    /// closure, regardless of whether you choose to keep or remove it.
    ///
    /// If the returned `ExtractIf` is not exhausted, e.g. because it is dropped without iterating
    /// or the iteration short-circuits, then the remaining elements will be retained.
    /// Use [`retain`] with a negated predicate if you do not need the returned iterator.
    ///
    /// [`retain`]: EqMap::retain
    ///
    /// # Examples
    ///
    /// Splitting a map into even and odd keys, reusing the original map:
    ///
    /// ```
    /// #![feature(btree_extract_if)]
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map: EqMap<i32, i32> = (0..8).map(|x| (x, x)).collect();
    /// let evens: EqMap<_, _> = map.extract_if(|k, _v| k % 2 == 0).collect();
    /// let odds = map;
    /// assert_eq!(evens.keys().copied().collect::<Vec<_>>(), [0, 2, 4, 6]);
    /// assert_eq!(odds.keys().copied().collect::<Vec<_>>(), [1, 3, 5, 7]);
    /// ```
    // pub fn extract_if<F>(&mut self, pred: F) -> ExtractIf<'_, K, V, F>
    // where
    //     K: Eq,
    //     F: FnMut(&K, &mut V) -> bool,
    // {
    //     let (inner, alloc) = self.extract_if_inner();
    //     ExtractIf { pred, inner, alloc }
    // }

    // pub(super) fn extract_if_inner(&mut self) -> (ExtractIfInner<'_, K, V>)
    // where
    //     K: Eq,
    // {
    //     if let Some(root) = self.root.as_mut() {
    //         let (root, dormant_root) = DormantMutRef::new(root);
    //         let front = root.borrow_mut().first_leaf_edge();
    //         (
    //             ExtractIfInner {
    //                 length: &mut self.length,
    //                 dormant_root: Some(dormant_root),
    //                 cur_leaf_edge: Some(front),
    //             },
    //             (*self.alloc).clone(),
    //         )
    //     } else {
    //         (
    //             ExtractIfInner {
    //                 length: &mut self.length,
    //                 dormant_root: None,
    //                 cur_leaf_edge: None,
    //             },
    //             (*self.alloc).clone(),
    //         )
    //     }
    // }

    /// Creates a consuming iterator visiting all the keys, in sorted order.
    /// The map cannot be used after calling this.
    /// The iterator element type is `K`.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut a = EqMap::new();
    /// a.insert(2, "b");
    /// a.insert(1, "a");
    ///
    /// let keys: Vec<i32> = a.into_keys().collect();
    /// assert_eq!(keys, [1, 2]);
    /// ```
    #[inline]
    pub fn into_keys(self) -> IntoKeys<K, V> {
        IntoKeys(self.0.into_iter())
    }

    /// Creates a consuming iterator visiting all the values, in order by key.
    /// The map cannot be used after calling this.
    /// The iterator element type is `V`.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut a = EqMap::new();
    /// a.insert(1, "hello");
    /// a.insert(2, "goodbye");
    ///
    /// let values: Vec<&str> = a.into_values().collect();
    /// assert_eq!(values, ["hello", "goodbye"]);
    /// ```
    #[inline]
    pub fn into_values(self) -> IntoValues<K, V> {
        IntoValues(self.0.into_iter())
    }

    /// Gets an iterator over the entries of the map, sorted by key.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map = EqMap::new();
    /// map.insert(3, "c");
    /// map.insert(2, "b");
    /// map.insert(1, "a");
    ///
    /// for (key, value) in map.iter() {
    ///     println!("{key}: {value}");
    /// }
    ///
    /// let (first_key, first_value) = map.iter().next().unwrap();
    /// assert_eq!((*first_key, *first_value), (1, "a"));
    /// ```
    pub fn iter(&self) -> std::slice::Iter<'_, (K, V)> {
        self.0.iter()
    }

    /// Gets a mutable iterator over the entries of the map, sorted by key.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map = EqMap::from([
    ///    ("a", 1),
    ///    ("b", 2),
    ///    ("c", 3),
    /// ]);
    ///
    /// // add 10 to the value if the key isn't "a"
    /// for (key, value) in map.iter_mut() {
    ///     if key != &"a" {
    ///         *value += 10;
    ///     }
    /// }
    /// ```
    pub fn iter_mut(
        &mut self,
    ) -> std::iter::Map<std::slice::IterMut<'_, (K, V)>, fn(&mut (K, V)) -> (&K, &mut V)> {
        self.0.iter_mut().map(|(k, v)| (&*k, v))
    }

    /// Gets an iterator over the keys of the map, in sorted order.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut a = EqMap::new();
    /// a.insert(2, "b");
    /// a.insert(1, "a");
    ///
    /// let keys: Vec<_> = a.keys().cloned().collect();
    /// assert_eq!(keys, [1, 2]);
    /// ```
    pub fn keys(&self) -> std::iter::Map<std::slice::Iter<'_, (K, V)>, fn(&(K, V)) -> &K> {
        self.0.iter().map(|(k, _)| k)
    }

    /// Gets an iterator over the values of the map, in order by key.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut a = EqMap::new();
    /// a.insert(1, "hello");
    /// a.insert(2, "goodbye");
    ///
    /// let values: Vec<&str> = a.values().cloned().collect();
    /// assert_eq!(values, ["hello", "goodbye"]);
    /// ```
    pub fn values(&self) -> std::iter::Map<std::slice::Iter<'_, (K, V)>, fn(&(K, V)) -> &V> {
        self.0.iter().map(|(_, v)| v)
    }

    /// Gets a mutable iterator over the values of the map, in order by key.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut a = EqMap::new();
    /// a.insert(1, String::from("hello"));
    /// a.insert(2, String::from("goodbye"));
    ///
    /// for value in a.values_mut() {
    ///     value.push_str("!");
    /// }
    ///
    /// let values: Vec<String> = a.values().cloned().collect();
    /// assert_eq!(values, [String::from("hello!"),
    ///                     String::from("goodbye!")]);
    /// ```
    pub fn values_mut(
        &mut self,
    ) -> std::iter::Map<std::slice::IterMut<'_, (K, V)>, fn(&mut (K, V)) -> &mut V> {
        self.0.iter_mut().map(|(_, v)| v)
    }

    /// Returns the number of elements in the map.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut a = EqMap::new();
    /// assert_eq!(a.len(), 0);
    /// a.insert(1, "a");
    /// assert_eq!(a.len(), 1);
    /// ```
    #[must_use]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if the map contains no elements.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut a = EqMap::new();
    /// assert!(a.is_empty());
    /// a.insert(1, "a");
    /// assert!(!a.is_empty());
    /// ```
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

use Entry::*;

/// A view into a single entry in a map, which may either be vacant or occupied.
///
/// This `enum` is constructed from the [`entry`] method on [`EqMap`].
///
/// [`entry`]: EqMap::entry
pub enum Entry<'a, K: Eq + 'a, V: 'a> {
    Vacant(VacantEntry<'a, K, V>),

    /// An occupied entry.
    Occupied(OccupiedEntry<'a, K, V>),
}

impl<K: fmt::Debug + Eq, V: fmt::Debug> fmt::Debug for Entry<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Vacant(ref v) => f.debug_tuple("Entry").field(v).finish(),
            Occupied(ref o) => f.debug_tuple("Entry").field(o).finish(),
        }
    }
}

/// A view into a vacant entry in a `EqMap`.
/// It is part of the [`Entry`] enum.
pub struct VacantEntry<'a, K: Eq, V> {
    key: K,
    map: &'a mut EqMap<K, V>,
}

impl<K: fmt::Debug + Eq, V> fmt::Debug for VacantEntry<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("VacantEntry").field(self.key()).finish()
    }
}

/// A view into an occupied entry in a `EqMap`.
/// It is part of the [`Entry`] enum.
pub struct OccupiedEntry<'a, K: Eq, V> {
    map: &'a mut EqMap<K, V>,
    idx: usize,
}

impl<K: fmt::Debug + Eq, V: fmt::Debug> fmt::Debug for OccupiedEntry<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("OccupiedEntry")
            .field("key", self.key())
            .field("value", self.get())
            .finish()
    }
}

/// The error returned by [`try_insert`](EqMap::try_insert) when the key already exists.
///
/// Contains the occupied entry, and the value that was not inserted.
pub struct OccupiedError<'a, K: Eq + 'a, V: 'a> {
    /// The entry in the map that was already occupied.
    pub entry: OccupiedEntry<'a, K, V>,
    /// The value which was not inserted, because the entry was already occupied.
    pub value: V,
}

impl<K: fmt::Debug + Eq, V: fmt::Debug> fmt::Debug for OccupiedError<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("OccupiedError")
            .field("key", self.entry.key())
            .field("old_value", self.entry.get())
            .field("new_value", &self.value)
            .finish()
    }
}

impl<'a, K: fmt::Debug + Eq, V: fmt::Debug> fmt::Display for OccupiedError<'a, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "failed to insert {:?}, key {:?} already exists with value {:?}",
            self.value,
            self.entry.key(),
            self.entry.get(),
        )
    }
}

impl<'a, K: fmt::Debug + Eq, V: fmt::Debug> std::error::Error for OccupiedError<'a, K, V> {
    fn description(&self) -> &str {
        "key already exists"
    }
}

impl<'a, K: Eq, V> Entry<'a, K, V> {
    /// Ensures a value is in the entry by inserting the default if empty, and returns
    /// a mutable reference to the value in the entry.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map: EqMap<&str, usize> = EqMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// assert_eq!(map["poneyland"], 12);
    /// ```
    pub fn or_insert(self, default: V) -> &'a mut V {
        match self {
            Occupied(entry) => entry.into_mut(),
            Vacant(entry) => entry.insert(default),
        }
    }

    /// Ensures a value is in the entry by inserting the result of the default function if empty,
    /// and returns a mutable reference to the value in the entry.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map: EqMap<&str, String> = EqMap::new();
    /// let s = "hoho".to_string();
    ///
    /// map.entry("poneyland").or_insert_with(|| s);
    ///
    /// assert_eq!(map["poneyland"], "hoho".to_string());
    /// ```
    pub fn or_insert_with<F: FnOnce() -> V>(self, default: F) -> &'a mut V {
        match self {
            Occupied(entry) => entry.into_mut(),
            Vacant(entry) => entry.insert(default()),
        }
    }

    /// Ensures a value is in the entry by inserting, if empty, the result of the default function.
    /// This method allows for generating key-derived values for insertion by providing the default
    /// function a reference to the key that was moved during the `.entry(key)` method call.
    ///
    /// The reference to the moved key is provided so that cloning or copying the key is
    /// unnecessary, unlike with `.or_insert_with(|| ... )`.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map: EqMap<&str, usize> = EqMap::new();
    ///
    /// map.entry("poneyland").or_insert_with_key(|key| key.chars().count());
    ///
    /// assert_eq!(map["poneyland"], 9);
    /// ```
    #[inline]
    pub fn or_insert_with_key<F: FnOnce(&K) -> V>(self, default: F) -> &'a mut V {
        match self {
            Occupied(entry) => entry.into_mut(),
            Vacant(entry) => {
                let value = default(entry.key());
                entry.insert(value)
            }
        }
    }

    /// Returns a reference to this entry's key.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map: EqMap<&str, usize> = EqMap::new();
    /// assert_eq!(map.entry("poneyland").key(), &"poneyland");
    /// ```
    pub fn key(&self) -> &K {
        match *self {
            Occupied(ref entry) => entry.key(),
            Vacant(ref entry) => entry.key(),
        }
    }

    /// Provides in-place mutable access to an occupied entry before any
    /// potential inserts into the map.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map: EqMap<&str, usize> = EqMap::new();
    ///
    /// map.entry("poneyland")
    ///    .and_modify(|e| { *e += 1 })
    ///    .or_insert(42);
    /// assert_eq!(map["poneyland"], 42);
    ///
    /// map.entry("poneyland")
    ///    .and_modify(|e| { *e += 1 })
    ///    .or_insert(42);
    /// assert_eq!(map["poneyland"], 43);
    /// ```
    pub fn and_modify<F>(self, f: F) -> Self
    where
        F: FnOnce(&mut V),
    {
        match self {
            Occupied(mut entry) => {
                f(entry.get_mut());
                Occupied(entry)
            }
            Vacant(entry) => Vacant(entry),
        }
    }
}

impl<'a, K: Eq, V: Default> Entry<'a, K, V> {
    /// Ensures a value is in the entry by inserting the default value if empty,
    /// and returns a mutable reference to the value in the entry.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map: EqMap<&str, Option<usize>> = EqMap::new();
    /// map.entry("poneyland").or_default();
    ///
    /// assert_eq!(map["poneyland"], None);
    /// ```
    pub fn or_default(self) -> &'a mut V {
        match self {
            Occupied(entry) => entry.into_mut(),
            Vacant(entry) => entry.insert(Default::default()),
        }
    }
}

impl<'a, K: Eq, V> VacantEntry<'a, K, V> {
    /// Gets a reference to the key that would be used when inserting a value
    /// through the VacantEntry.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map: EqMap<&str, usize> = EqMap::new();
    /// assert_eq!(map.entry("poneyland").key(), &"poneyland");
    /// ```
    pub fn key(&self) -> &K {
        &self.key
    }

    /// Take ownership of the key.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    /// use crate::util::collections::eq_map::Entry;
    ///
    /// let mut map: EqMap<&str, usize> = EqMap::new();
    ///
    /// if let Entry::Vacant(v) = map.entry("poneyland") {
    ///     v.into_key();
    /// }
    /// ```
    pub fn into_key(self) -> K {
        self.key
    }

    /// Sets the value of the entry with the `VacantEntry`'s key,
    /// and returns a mutable reference to it.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    /// use crate::util::collections::eq_map::Entry;
    ///
    /// let mut map: EqMap<&str, u32> = EqMap::new();
    ///
    /// if let Entry::Vacant(o) = map.entry("poneyland") {
    ///     o.insert(37);
    /// }
    /// assert_eq!(map["poneyland"], 37);
    /// ```
    pub fn insert(self, value: V) -> &'a mut V {
        self.map.0.push((self.key, value));
        self.map.0.last_mut().map(|(_, v)| v).unwrap()
    }
}

impl<'a, K: Eq, V> OccupiedEntry<'a, K, V> {
    /// Gets a reference to the key in the entry.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    ///
    /// let mut map: EqMap<&str, usize> = EqMap::new();
    /// map.entry("poneyland").or_insert(12);
    /// assert_eq!(map.entry("poneyland").key(), &"poneyland");
    /// ```
    #[must_use]
    pub fn key(&self) -> &K {
        &self.map.0[self.idx].0
    }

    /// Take ownership of the key and value from the map.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    /// use crate::util::collections::eq_map::Entry;
    ///
    /// let mut map: EqMap<&str, usize> = EqMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// if let Entry::Occupied(o) = map.entry("poneyland") {
    ///     // We delete the entry from the map.
    ///     o.remove_entry();
    /// }
    ///
    /// // If now try to get the value, it will panic:
    /// // println!("{}", map["poneyland"]);
    /// ```
    pub fn remove_entry(self) -> (K, V) {
        self.map.0.swap_remove(self.idx)
    }

    /// Gets a reference to the value in the entry.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    /// use crate::util::collections::eq_map::Entry;
    ///
    /// let mut map: EqMap<&str, usize> = EqMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// if let Entry::Occupied(o) = map.entry("poneyland") {
    ///     assert_eq!(o.get(), &12);
    /// }
    /// ```
    #[must_use]
    pub fn get(&self) -> &V {
        &self.map.0[self.idx].1
    }

    /// Gets a mutable reference to the value in the entry.
    ///
    /// If you need a reference to the `OccupiedEntry` that may outlive the
    /// destruction of the `Entry` value, see [`into_mut`].
    ///
    /// [`into_mut`]: OccupiedEntry::into_mut
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    /// use crate::util::collections::eq_map::Entry;
    ///
    /// let mut map: EqMap<&str, usize> = EqMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// assert_eq!(map["poneyland"], 12);
    /// if let Entry::Occupied(mut o) = map.entry("poneyland") {
    ///     *o.get_mut() += 10;
    ///     assert_eq!(*o.get(), 22);
    ///
    ///     // We can use the same Entry multiple times.
    ///     *o.get_mut() += 2;
    /// }
    /// assert_eq!(map["poneyland"], 24);
    /// ```
    pub fn get_mut(&mut self) -> &mut V {
        &mut self.map.0[self.idx].1
    }

    /// Converts the entry into a mutable reference to its value.
    ///
    /// If you need multiple references to the `OccupiedEntry`, see [`get_mut`].
    ///
    /// [`get_mut`]: OccupiedEntry::get_mut
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    /// use crate::util::collections::eq_map::Entry;
    ///
    /// let mut map: EqMap<&str, usize> = EqMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// assert_eq!(map["poneyland"], 12);
    /// if let Entry::Occupied(o) = map.entry("poneyland") {
    ///     *o.into_mut() += 10;
    /// }
    /// assert_eq!(map["poneyland"], 22);
    /// ```
    #[must_use = "`self` will be dropped if the result is not used"]
    pub fn into_mut(self) -> &'a mut V {
        &mut self.map.0[self.idx].1
    }

    /// Sets the value of the entry with the `OccupiedEntry`'s key,
    /// and returns the entry's old value.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    /// use crate::util::collections::eq_map::Entry;
    ///
    /// let mut map: EqMap<&str, usize> = EqMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// if let Entry::Occupied(mut o) = map.entry("poneyland") {
    ///     assert_eq!(o.insert(15), 12);
    /// }
    /// assert_eq!(map["poneyland"], 15);
    /// ```
    pub fn insert(&mut self, value: V) -> V {
        std::mem::replace(self.get_mut(), value)
    }

    /// Takes the value of the entry out of the map, and returns it.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::util::collections::EqMap;
    /// use crate::util::collections::eq_map::Entry;
    ///
    /// let mut map: EqMap<&str, usize> = EqMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// if let Entry::Occupied(o) = map.entry("poneyland") {
    ///     assert_eq!(o.remove(), 12);
    /// }
    /// // If we try to get "poneyland"'s value, it'll panic:
    /// // println!("{}", map["poneyland"]);
    /// ```
    pub fn remove(self) -> V {
        self.remove_entry().1
    }
}

pub struct IntoValues<K: Eq, V>(std::vec::IntoIter<(K, V)>);
impl<'a, K: Eq, V> From<IntoValues<K, V>> for std::vec::IntoIter<(K, V)> {
    fn from(value: IntoValues<K, V>) -> Self {
        value.0
    }
}
impl<K: Eq, V> Iterator for IntoValues<K, V> {
    type Item = V;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(_, v)| v)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
    fn count(self) -> usize
    where
        Self: Sized,
    {
        self.0.count()
    }
}
impl<K: Eq, V> DoubleEndedIterator for IntoValues<K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|(_, v)| v)
    }
}
impl<K: Eq, V> ExactSizeIterator for IntoValues<K, V> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

pub struct IntoKeys<K: Eq, V>(std::vec::IntoIter<(K, V)>);
impl<'a, K: Eq, V> From<IntoKeys<K, V>> for std::vec::IntoIter<(K, V)> {
    fn from(value: IntoKeys<K, V>) -> Self {
        value.0
    }
}
impl<K: Eq, V> Iterator for IntoKeys<K, V> {
    type Item = K;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(k, _)| k)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
    fn count(self) -> usize
    where
        Self: Sized,
    {
        self.0.count()
    }
}
impl<K: Eq, V> DoubleEndedIterator for IntoKeys<K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|(k, _)| k)
    }
}
impl<K: Eq, V> ExactSizeIterator for IntoKeys<K, V> {
    fn len(&self) -> usize {
        self.0.len()
    }
}
