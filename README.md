# Logos Display
A small derive macro that automatically implements `Display` for an enum based on the `Logos` `token` attribute.

## How To Use
Simply `use logos_display::Display` and add it to your derives, like so:

```rust
use logos_display::Display
#[derive(Display, Logos)]
enum A {
	#[token("{")]
	LCur,

	#[token("}")]
	RCur
}

|
V

impl Display for A {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret = match &self {
            LCur => "{",
            RCur => "}",
        };
        write!(f, "{}", ret)
    }
}
```

### Dealing with non-tokens
In the case that a variant is not a token (but, for example, a regex), the name of the variant will be used for the Display method (so variant `B` will have `"B"` as it's string representation). If you want to override any of this functionality, you can add an `override_display("string")` attribute to the variant as follows:

```rust
use logos_display::Display
#[derive(Display, Logos)]
enum A {
	#[override_display("fancy curly thing")]
	#[token("{")]
	LCur,

	#[token("}")]
	RCur
}

|
V

impl Display for A {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret = match &self {
            LCur => "fancy curly thing",
            RCur => "}",
        };
        write!(f, "{}", ret)
    }
}
```

### Multiple tokens
When a variant accepts multiple tokens, they will be concatenated using `/` in the string representation, like so:

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

impl Display for A {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret = match &self {
            LCur => "{/}",
        };
        write!(f, "{}", ret)
    }
}
```