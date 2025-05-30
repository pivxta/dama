macro_rules! mapped_enum_u8 {
    {
        $(#[$attr:meta])*
        $vis:vis enum $name:ident [all: $all_name:ident] {
            $($variant:ident),*$(,)?
        }

        $(
            $(#[$map_attr:meta])*
            $map_vis:vis map $map_name:ident {
                $($map_variant:ident => $mapped_name:ident),*$(,)?
            }
        )?
    } => {
        $(#[$attr])*
        #[repr(u8)]
        $vis enum $name {
            $($variant),*
        }

        #[derive(Debug, Clone)]
        pub struct $all_name(core::ops::Range<usize>);

        impl $name {
            pub const COUNT: usize = [$(Self::$variant),*].len();

            #[inline]
            pub const fn from_index(index: usize) -> Self {
                assert!(index < Self::COUNT);
                unsafe { Self::from_index_unchecked(index) }
            }

            #[inline]
            pub const fn try_from_index(index: usize) -> Option<Self> {
                if index < Self::COUNT {
                    unsafe { Some(Self::from_index_unchecked(index)) }
                } else {
                    None
                }
            }

            #[inline]
            pub const unsafe fn from_index_unchecked(index: usize) -> Self {
                unsafe { core::mem::transmute(index as u8) }
            }

            #[inline]
            pub fn all() -> $all_name {
                $all_name(0..Self::COUNT)
            }
        }

        impl Iterator for $all_name {
            type Item = $name;

            #[inline]
            fn next(&mut self) -> Option<$name> {
                self.0.next().map(|idx| unsafe { $name::from_index_unchecked(idx) })
            }

            #[inline]
            fn nth(&mut self, n: usize) -> Option<$name> {
                self.0.nth(n).map(|idx| unsafe { $name::from_index_unchecked(idx) })
            }

            #[inline]
            fn last(self) -> Option<$name> {
                self.0.last().map(|idx| unsafe { $name::from_index_unchecked(idx) })
            }

            #[inline]
            fn count(self) -> usize {
                self.0.count()
            }

            #[inline]
            fn size_hint(&self) -> (usize, Option<usize>) {
                self.0.size_hint()
            }
        }

        impl ExactSizeIterator for $all_name {}

        impl DoubleEndedIterator for $all_name {
            #[inline]
            fn next_back(&mut self) -> Option<$name> {
                self.0.next_back().map(|idx| unsafe { $name::from_index_unchecked(idx) })
            }

            #[inline]
            fn nth_back(&mut self, n: usize) -> Option<$name> {
                self.0.nth_back(n).map(|idx| unsafe { $name::from_index_unchecked(idx) })
            }
        }

        $(
            $(#[$map_attr])*
            #[repr(C)]
            $map_vis struct $map_name<T> {
                $(pub $mapped_name: T),*
            }

            #[derive(Clone, Debug)]
            $map_vis struct IntoIter<T>(core::iter::Zip<$all_name, IntoValues<T>>);

            #[derive(Clone, Debug)]
            $map_vis struct Iter<'a, T>(core::iter::Zip<$all_name, Values<'a, T>>);

            #[derive(Debug)]
            $map_vis struct IterMut<'a, T>(core::iter::Zip<$all_name, ValuesMut<'a, T>>);

            #[derive(Clone, Debug)]
            $map_vis struct IntoValues<T>(core::array::IntoIter<T, {$name::COUNT}>);

            #[derive(Clone, Debug)]
            $map_vis struct Values<'a, T>(core::array::IntoIter<&'a T, {$name::COUNT}>);

            #[derive(Debug)]
            $map_vis struct ValuesMut<'a, T>(core::array::IntoIter<&'a mut T, {$name::COUNT}>);

            impl<T> $map_name<T> {
                #[inline]
                pub fn from_fn(mut f: impl FnMut($name) -> T) -> Self {
                    Self {
                        $($mapped_name: f($name::$map_variant)),*
                    }
                }

                #[inline]
                pub fn iter(&self) -> Iter<T> {
                    Iter($name::all().zip(self.values()))
                }

                #[inline]
                pub fn iter_mut(&mut self) -> IterMut<T> {
                    IterMut($name::all().zip(self.values_mut()))
                }

                #[inline]
                pub fn into_values(self) -> IntoValues<T> {
                    IntoValues([$(self.$mapped_name),*].into_iter())
                }

                #[inline]
                pub fn values(&self) -> Values<T> {
                    Values([$(&self.$mapped_name),*].into_iter())
                }

                #[inline]
                pub fn values_mut(&mut self) -> ValuesMut<T> {
                    ValuesMut([$(&mut self.$mapped_name),*].into_iter())
                }
            }

            impl<T> IntoIterator for $map_name<T> {
                type Item = ($name, T);
                type IntoIter = IntoIter<T>;

                fn into_iter(self) -> IntoIter<T> {
                    IntoIter($name::all().zip(self.into_values()))
                }
            }

            impl<'a, T> IntoIterator for &'a $map_name<T> {
                type Item = ($name, &'a T);
                type IntoIter = Iter<'a, T>;

                fn into_iter(self) -> Iter<'a, T> {
                    self.iter()
                }
            }

            impl<'a, T> IntoIterator for &'a mut $map_name<T> {
                type Item = ($name, &'a mut T);
                type IntoIter = IterMut<'a, T>;

                fn into_iter(self) -> IterMut<'a, T> {
                    self.iter_mut()
                }
            }

            impl<T> Iterator for IntoIter<T> {
                type Item = ($name, T);

                #[inline]
                fn next(&mut self) -> Option<Self::Item> {
                    self.0.next()
                }
            }

            impl<'a, T> Iterator for Iter<'a, T> {
                type Item = ($name, &'a T);

                #[inline]
                fn next(&mut self) -> Option<Self::Item> {
                    self.0.next()
                }
            }

            impl<'a, T> Iterator for IterMut<'a, T> {
                type Item = ($name, &'a mut T);

                #[inline]
                fn next(&mut self) -> Option<Self::Item> {
                    self.0.next()
                }
            }

            impl<T> Iterator for IntoValues<T> {
                type Item = T;

                #[inline]
                fn next(&mut self) -> Option<T> {
                    self.0.next()
                }
            }

            impl<'a, T> Iterator for Values<'a, T> {
                type Item = &'a T;

                #[inline]
                fn next(&mut self) -> Option<&'a T> {
                    self.0.next()
                }
            }

            impl<'a, T> Iterator for ValuesMut<'a, T> {
                type Item = &'a mut T;

                #[inline]
                fn next(&mut self) -> Option<&'a mut T> {
                    self.0.next()
                }
            }

            impl<T> core::ops::Index<$name> for $map_name<T> {
                type Output = T;

                #[inline]
                fn index(&self, idx: $name) -> &T {
                    match idx {
                        $($name::$map_variant => &self.$mapped_name),*
                    }
                }
            }

            impl<T> core::ops::IndexMut<$name> for $map_name<T> {
                #[inline]
                fn index_mut(&mut self, idx: $name) -> &mut T {
                    match idx {
                        $($name::$map_variant => &mut self.$mapped_name),*
                    }
                }
            }
        )?
    };
}

pub(crate) use mapped_enum_u8;
