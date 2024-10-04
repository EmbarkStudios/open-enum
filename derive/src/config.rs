// Copyright 2022 Google LLC
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

use std::collections::HashSet;

use proc_macro2::Span;
use syn::{parse::Parse, Error, Token, Visibility};

pub struct Config {
    pub allow_alias: bool,
    pub repr_visibility: Visibility,
    pub with_closed: bool,
}

mod kw {
    syn::custom_keyword!(allow_alias);
    syn::custom_keyword!(with_closed);
    syn::custom_keyword!(inner_vis);
}

fn parse_optional_eq_bool(input: syn::parse::ParseStream) -> syn::Result<bool> {
    Ok(if input.peek(Token![=]) {
        let _eq_token: Token![=] = input.parse()?;
        let out: syn::LitBool = input.parse()?;
        out.value
    } else {
        true
    })
}

impl Parse for Config {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut out = Self {
            allow_alias: false,
            with_closed: false,
            repr_visibility: Visibility::Public(Token![pub](Span::call_site())),
        };
        let mut seen_kws = HashSet::new();
        while !input.is_empty() {
            let lookahead = input.lookahead1();
            if lookahead.peek(kw::allow_alias) {
                let allow_alias = input.parse::<kw::allow_alias>().unwrap();
                if !seen_kws.insert("allow_alias") {
                    return Err(Error::new_spanned(&allow_alias, "`allow_alias` config specified twice"));
                }

                out.allow_alias = parse_optional_eq_bool(input)?;
            } else if lookahead.peek(kw::with_closed) {
                let with_closed = input.parse::<kw::with_closed>().unwrap();
                if !seen_kws.insert("with_closed") {
                    return Err(Error::new_spanned(&with_closed, "`with_closed` config specified twice"));
                }

                out.with_closed = parse_optional_eq_bool(input)?;
            } else if lookahead.peek(kw::inner_vis) {
                let inner_vis = input.parse::<kw::inner_vis>().unwrap();
                if !seen_kws.insert("inner_vis") {
                    return Err(Error::new_spanned(&inner_vis, "`inner_vis` attribute specified twice"));
                }

                let _eq_token: Token![=] = input.parse()?;

                out.repr_visibility = input.parse()?;
                if matches!(out.repr_visibility, syn::Visibility::Inherited) {
                    return Err(input.error("Expected visibility"));
                }
            } else {
                return Err(lookahead.error())
            }
            if !input.is_empty() {
                let _comma: Token![,] = input.parse()?;
            }
        }
        if out.allow_alias && out.with_closed {
            return Err(Error::new(Span::call_site(), "`with_closed` and `allow_alias` cannot be used together"));
        }
        Ok(out)
    }
}
