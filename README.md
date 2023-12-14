<div align="center">
  <!-- Version -->
  <a href="https://crates.io/crates/debugify">
    <img alt="Crates.io version" src="https://img.shields.io/crates/v/debugify.svg?style=flat-square"/>
  </a>

  <!-- Docs -->
  <a href="https://docs.rs/debugify/latest/debugify/">
    <img alt="docs.rs" src="https://img.shields.io/docsrs/debugify?style=flat-square"/>
  </a>
  
  <!-- Dependencies -->
  <a href="https://deps.rs/repo/github/LouisGariepy/debugify">
    <img alt="Crates.io version" src="https://deps.rs/repo/github/LouisGariepy/debugify/status.svg?style=flat-square"/>
  </a>

 
  <!-- License -->
  <a href="https://github.com/LouisGariepy/debugify#License">
    <img alt="License" src="https://img.shields.io/badge/License-APACHE--2.0%2FMIT-blue?style=flat-square"/>
  </a>
</div>

<br/>

# debugify

Derive macro for `std::fmt::Debug` focused on reducing boilerplate.Supports both format strings and formatter functions.

## Formats

Formats can be specified either as a format string or as the path to a formatter function.
A formatter function must adhere to the following signature: `fn(&T) -> String`.

## Attributes

In case of a conflict between attributes, the order of precedence is

1. field attribute
2. field name
3. field type

If no format is specified, the default format is used.

### Item attributes

These attributes are applied to a struct or enum.

#### `field_name`

Applies to the formatting of all fields with the given names inside the item.

```rust
use debugify::Debugify;

#[derive(Debugify)]
#[debugify(field_name(
    [bar, biz] = "foobar{:?}",
    baz = "foobaz{:?}",
))]
struct Foo {
    bar: i32,
    baz: String,
    biz: &'static str,
    qux: i64,

}

let foo = Foo {
    bar: 123,
    baz: "hello".to_string(),
    biz: "world",
    qux: 456,
};

let foo_debug = format!("{:?}", foo);
assert_eq!(foo_debug, "Foo { bar: foobar123, baz: foobaz\"hello\", biz: foobar\"world\", qux: 456 }");
```

#### `field_type`

Applies to the formatting of all fields with the given types inside the item.

```rust
use debugify::Debugify;

#[derive(Debugify)]
#[debugify(field_type(
    [i32, &'static str] = "foobar{:?}",
    String = "foobaz{:?}",
))]
struct Foo {
    bar: i32,
    baz: String,
    biz: &'static str,
    qux: i64,
}

let foo = Foo {
    bar: 123,
    baz: "hello".to_string(),
    biz: "world",
    qux: 456,
};

let foo_debug = format!("{:?}", foo);
assert_eq!(foo_debug, "Foo { bar: foobar123, baz: foobaz\"hello\", biz: foobar\"world\", qux: 456 }");
```

### Field attributes

Currently the only field attribute support is a format specifier.

```rust
use debugify::Debugify;

#[derive(Debugify)]
#[debugify(field_name(bar = "foo{:?}"))]
struct Foo {
    #[debugify("bar{:?}")]
    bar: i32,
    baz: String,
}

let foo = Foo {
    bar: 123,
    baz: "hello".to_string(),
};

let foo_debug = format!("{:?}", foo);
assert_eq!(foo_debug, "Foo { bar: bar123, baz: \"hello\" }");
```

Field attributes take precedence over item attributes.

## Enums

Enums are supported as well. Item attributes are apply to all
variants, and each variant is treated essentially as a struct.

```rust
use debugify::Debugify;

#[derive(Debugify)]
#[debugify(field_name([biz, qux] = "foo{:?}"))]
enum Foo {
    Bar {
        biz: i32,
        qux: String,
    },
    Baz {
        biz: i32,
        #[debugify("qux{:?}")]
        qux: String,
        quant: i64,
    }
}

let foo_1 = Foo::Bar {
    biz: 123,
    qux: "hello".to_string(),
};
let foo_2 = Foo::Baz {
    biz: 456,
    qux: "world".to_string(),
    quant: 789,
};

let foo_1_debug = format!("{:?}", foo_1);
assert_eq!(foo_1_debug, "Bar { biz: foo123, qux: foo\"hello\" }");

let foo_2_debug = format!("{:?}", foo_2);
assert_eq!(foo_2_debug, "Baz { biz: foo456, qux: qux\"world\", quant: 789 }");
```

## Tuple and unit structs and variants
Tuple structs and variants also support field format attributes. Of course, these don't interact at all with the field name rules.

Unit structs and variants are formatted as normal.
