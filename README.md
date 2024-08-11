# Logos Display
A set of derive macros that automatically implement `Display` and `Debug` for an enum, based on the [Logos](https://github.com/maciejhirsz/logos) `token` and `regex` attributes. Usable in `no_std` contexts.

## How To Use
Simply `use logos_display::Display` and/or `use logos_display::Debug` and add it to your derives, like so:

```rust
use logos_display::{Display, Debug}
#[derive(Display, Debug, Logos)]
enum A {
	#[token("{")]
	LCur,

	#[regex("[a-z]")]
	Lower
}

|
V

impl core::fmt::Display for A {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use core::fmt::Write;
        match &self {
            A::LCur => write!(f, "{}", "{"),
            A::Lower => write!(f, "{}", "[a-z]"),
        }
    }
}
```

## Difference between `Display` and `Debug`
If the enum variant is a unit type, there is no difference. But in the case where the variant is a tuple or struct variant, the `Debug` version will also show the inner value held by the instance, whereas the `Display` version will only output the name of the outer layer. Like so:

```rust
use logos_display::{Display, Debug}
#[derive(Display, Debug, Logos)]
enum A {
	#[token("{")]
	LCur,

	#[regex("[a-z]", |lex| some_func(lex.slice()))]
	Lower(TypeOne, TypeTwo)
}

|
V

impl core::fmt::Debug for A {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
		use core::fmt::Write;
        match &self {
            A::LCur => write!(f, "{}", "{"),
            A::Lower(_arg1, _arg2) => write!(f, "{}{:?}", "[a-z]", vec![_arg1, _arg2]),
        }
    }
}
```

This does of course require the inner types to implement `Debug` in some form as well.

### Dealing with non-tokens
In the case that a variant is not a token or regex, the name of the variant will be used for the Display method (so variant `B` will have `"B"` as it's string representation). If you want to override any of this functionality, you can add an `display_override("string")` attribute to the variant as follows:

```rust
use logos_display::Display
#[derive(Display, Logos)]
enum A {
	#[display_override("fancy curly thing")]
	#[token("{")]
	LCur,

	#[regex("[a-z]")]
	Lower
}

|
V

impl core::fmt::Display for A {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
		use core::fmt::Write;
        match &self {
            A::LCur => write!(f, "{}", "fancy curly thing"),
            A::Lower => write!(f, "{}", "[a-z]"),
        }
    }
}
```

### Multiple tokens
When a variant accepts multiple tokens, by default, they will be concatenated using `/` in the string representation, like so:

```rust
use logos_display::Display
#[derive(Display, Logos)]
enum A {
	#[token("{")]
	#[token("}")]
	Cur
}

|
V

impl core::fmt::Display for A {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
		use core::fmt::Write;
        match &self {
            A::LCur => write!(f, "{}", "{/}"),
        }
    }
}
```

This functionality can be overwritten using the `display_concat("string")` attribute on the original enum:
```rust
use logos_display::Display
#[derive(Display, Logos)]
#[display_concat(" or ")]
enum A {
	#[token("{")]
	#[token("}")]
	Cur
}

|
V

impl core::fmt::Display for A {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
		use core::fmt::Write;
        match &self {
            A::LCur => write!(f, "{}", "{ or }"),
        }
    }
}
```

Additionally, you can pass `None` to this attribute in order to disable concatonation. In this case, the token that is encountered last will be used:
```rust
use logos_display::Display
#[derive(Display, Logos)]
#[display_concat(None)]
enum A {
	#[token("{")]
	#[token("}")]
	Cur
}

|
V

impl core::fmt::Display for A {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
		use core::fmt::Write;
        match &self {
            A::LCur => write!(f, "{}", "}"),
        }
    }
}
```
