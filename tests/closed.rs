// Copyright © 2023 Microsoft Corporation
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

extern crate open_enum;
use open_enum::*;

#[open_enum(with_closed)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, ToRepr, FromRepr)]
#[open_enum_closed_attr(derive(Clone, Copy, Debug, PartialEq, Eq))]
#[repr(u32)]
pub enum Color {
    Red = 1,
    Blue = 2,
}

#[open_enum(with_closed)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, ToRepr, FromRepr)]
#[open_enum_closed_attr(derive(Clone, Copy, Debug, PartialEq, Eq))]
#[repr(u32)]
pub enum ColorWithFeatures {
    Red = 1,
    Blue = 2,
    /// Test doc
    #[cfg(feature = "std")]
    Orange = 3,
}

#[test]
fn try_from_open() {

    let repr: u32 = ColorWithFeatures::Red.into();
    let open = ColorWithFeatures::from(repr);
    let res = ColorWithFeaturesClosed::try_from(open);
    assert!(matches!(res, Ok(ColorWithFeaturesClosed::Red)));

    let repr: u32 = ColorWithFeatures::Blue.into();
    let open = ColorWithFeatures::from(repr);
    let res =  ColorWithFeaturesClosed::try_from(open);
    assert!(matches!(res, Ok(ColorWithFeaturesClosed::Blue)));

    #[cfg(not(feature = "std"))]
    {
        let repr: u32 = 3; // unknown orange on std
        let open = ColorWithFeatures::from(repr);
        let res = ColorWithFeaturesClosed::try_from(open);
        assert!(matches!(res, Err(UnknownVariantError)));
    }
}
