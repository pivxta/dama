macro_rules! mapped_enum {
    {
        $(#[$attr:meta])*
        $vis:vis enum $name:ident {
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
        $vis enum $name {
            $($variant),*
        }

        impl $name {
            pub const COUNT: usize = [$(Self::$variant),*].len();
            pub const ALL: [Self; Self::COUNT] = [$(Self::$variant),*];
        }

        $(
            $(#[$map_attr])*
            $map_vis struct $map_name<T> {
                $(pub $mapped_name: T),*
            }

            #[derive(Clone, Debug)]
            $map_vis struct IntoIter<T>(core::array::IntoIter<($name, T), {$name::COUNT}>);

            #[derive(Clone, Debug)]
            $map_vis struct Iter<'a, T>(core::array::IntoIter<($name, &'a T), {$name::COUNT}>);

            #[derive(Debug)]
            $map_vis struct IterMut<'a, T>(core::array::IntoIter<($name, &'a mut T), {$name::COUNT}>);

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
                    Iter([$(($name::$map_variant, &self.$mapped_name)),*].into_iter())
                }

                #[inline]
                pub fn iter_mut(&mut self) -> IterMut<T> {
                    IterMut([$(($name::$map_variant, &mut self.$mapped_name)),*].into_iter())
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
                    IntoIter([$(($name::$map_variant, self.$mapped_name)),*].into_iter())
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

pub(crate) use mapped_enum;
