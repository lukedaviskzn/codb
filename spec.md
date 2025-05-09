# Spec

## Types

### Primitives

#### Scalars

| Name        | Size (bytes) | Description |
|-------------|--------------|-------------|
| !           | 0            | Empty type (infallible), cannot be instantiated, implicitely casts to any other type |
| ()          | 0            | Unit type |
| bool        | 1            | Boolean, true or false |
| char        | 4            | Unicode scalar value |
| int{bits}   | bits / 8     | {bits} is either 8, 16, 32, 64, 128, 256 |
| uint{bits}  | bits / 8     | {bits} is either 8, 16, 32, 64, 128, 256 |
| float{bits} | bits / 8     | {bits} is either 16, 32, 64, 128 |
| decimal     | 16           | 128 bit decimal, consists of one 96 bit unsigned integer $m$, a 1 bit sign, and a scaling factor integer $e$ from 0 to 28 inclusive, and 21 unused bits. The value represented is $m / 10^e$.
| fp{bits}f{frac} | bits / 8 | {bits} is either 16, 32, 64, 128, {frac} is between 0 and {bits} inclusive and designates the number of bits after the decimal point.
| scalar      | ?            | Composite types can be designated as *scalar*, meaning they are not linear, and are copied rather than moved. |

#### Tuples

A tuple type is linear if any of its elements is linear, and scalar otherwise. Tuples types are written as `(TypeA, TypeB, ...)`. A tuple is instantied as `(valueA, valueB, ...)`. Tuples may be any length from 0 to 255. A 0-length tuple is the unit. Tuples are the anonymous product type. There is no anonymous sum type.

### Structs (Product)

Struct types are linear by default, unless marked as scalar. A struct is declared as:

```rust
struct MyStruct {
    fieldA: TypeA,
    fieldB: TypeB,
    ...
}
```

Structs may be denoted scalar, for example:

```rust
scalar struct Vector3 {
    x: f32,
    y: f32,
    z: f32,
}
```

Both scalar and linear structs are instantiated as follows:

```rust
MyStruct {
    fieldA: valueA,
    fieldB: valueB,
}
```

### Enums (Sum)

Enum types are linear by default, unless marked as scalar. A struct is declared as follows, and may have any of the follows variant types:

```rust
enum MyEnum {
    // Variant with no value, implicitely variant has type unit
    TagA,
    // Tuple variant
    TagB(TypeA, TypeB, ...),
    // Anonymous struct variant, this is the only place anonymous structs can be defined
    TagC {
        fieldA: TypeA,
        fieldB: TypeB,
        ...
    },
    ...
}
```

And can be marked as scalar as follows:

```rust
scalar enum MyEnum {
    ...
}
```

### References

A reference type points to an object without owning it.

Reference types are written as:

```rust
&Type
```

And can be instantiated as:

```rust
&value
```

## Functions

Functions can be defined as follows:

```rust
fn my_function(argA: TypeA, argB: TypeB, ...) -> TypeC {
    ...
}
```

Functions have type:
```rust
Fn(TypeA, TypeB, ...) -> TypeC
```

Functions are first class objects.

## Collections

Collections are the equivalent of relations. A collection is made of rows. Collections are RW locked (read for row read/write, write for row add/delete) and each row is also RW locked.

Collections have type: `Collection<Type>`, and rows have type `Row<Type>`.

We define a collection as follows:

```rust
let my_collection = Collection<Type>::new();
```

The collection type is:

```rust
struct Collection<Type> {
    schema: Schema,
    primary: BTreeMap<Value, Row<Type>>,
    indices: BTreeMap<Value, &weak Row<Type>>,
}
```

## Modules

Mobules are the equivalent of schemas. All types, functions, objects, and modules (except the root module) must be within a module.

A script may specify a using declaration to place itself within a module.

```cpp
using my_module;
```
