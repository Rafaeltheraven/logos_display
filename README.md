# Logos Display
A derive macro that automatically implements `Display` for an enum based on the [Logos](https://github.com/maciejhirsz/logos) `token` and `regex` attributes.

## How To Use
Simply `use logos_display::Display` and add it to your derives, like so:

```rust
use logos_display::Display
#[derive(Display, Logos)]
enum A {
	#[token("{")]
	LCur,

	#[regex("[a-z]")]
	Lower
}

|
V

impl std::fmt::Display for A {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret = match &self {
            A::LCur => "{",
            A::Lower => "[a-z]",
        };
        write!(f, "{}", ret)
    }
}
```

### Dealing with non-tokens
In the case that a variant is not a token or regex, the name of the variant will be used for the Display method (so variant `B` will have `"B"` as it's string representation). If you want to override any of this functionality, you can add an `override_display("string")` attribute to the variant as follows:

```rust
use logos_display::Display
#[derive(Display, Logos)]
enum A {
	#[override_display("fancy curly thing")]
	#[token("{")]
	LCur,

	#[regex("[a-z]")]
	Lower
}

|
V

impl std::fmt::Display for A {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret = match &self {
            A::LCur => "fancy curly thing",
            A::Lower => "[a-z]",
        };
        write!(f, "{}", ret)
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

impl std::fmt::Display for A {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret = match &self {
            A::LCur => "{/}",
        };
        write!(f, "{}", ret)
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

impl std::fmt::Display for A {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret = match &self {
            A::LCur => "{ or }",
        };
        write!(f, "{}", ret)
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

impl std::fmt::Display for A {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret = match &self {
            A::LCur => "}",
        };
        write!(f, "{}", ret)
    }
}
```