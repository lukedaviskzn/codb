# CoDB

Simple database with support for nested types.

Type system supports product (struct) types and sum (enum) types.

## File Format

File format based off of SQLite. The file is split 
into 4096 length pages. Large objects may be split 
across multiple pages using a linked list. The first 
page is the DB header. Which starts with the magic 
string "CoDB", followed by 3 u8's corresponding to 
the major, minor, and patch versions of the DB that
created the file. Then followed by 2 u64 "page 
pointers", the first being the DB manifest, and the second being the freelist. The freelist is not implemented, but would simply be linked list of 
all pages that are freed by the pager.

The DB manifest consists of a registry, and the 
relation set page pointer. These are encoded into 
strings and concatenated. The registry contains some 
information about types, while the relation set 
contains a map from a relation name to it's first page.

The registry starts with a root module, which 
contains a map from identifiers to "module items",
which are either types, functions, or other modules.
A function consists of an argument list, a return 
type, and an expression. Types are either struct 
types, enum types, or array types (either fixed-length
or variable length). Struct and enum types are maps 
from identifiers to types. Array types consist of 
a type, and an optional length.

A relation consists of a schema and a map from 
primary keys to rows. Both rows and keys are struct 
values. With a key type being a projection of the 
row type. The schema consists of the relations type, 
which must be a struct type, and the relations primary 
key.

## Features / To Do

- [x] Types
- [x] Relations
    - [x] Relation Binary Format
    - [ ] More Efficient Binary Format
- [x] Functions
- [x] Expressions
    - [x] Literals
- [x] Shell
- [ ] File Locks
- [ ] Constraints
    - [ ] Row/Type-Level Constraints (e.g. check constraint, refinement types)
    - [ ] Relation-Level Constraints (e.g. unique key)
    - [ ] DB-Level Constraints (e.g. foreign key)
- [ ] Type Inference
