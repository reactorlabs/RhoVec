# Formalization of dplyr

## Syntax

### Expressions

    e ::=
        | lit                           # literal
        | Vec(e_1, .. , e_n)            # vector constructor
        | v                             # value

### Literals

    lit ::=
        | num                           # integer

### Values

    v ::=
        | [lit_1 .. lit_n],T           # vector (and its type)

### Types

    T ::=
        | T_Int                         # integer


## Operational Semantics

### `e_1 --> e_2`

`e_1` reduces to `e_2`

    -------------------  :: E_Scalar2Vec
    num --> [num],T_Int


    (e_1 --> [num_1_1 .. num_1_m1],T_Int) .. (e_n --> [num_n_1 .. num_n_mn],T_Int)
    ------------------------------------------------------------------------------  :: E_VecCtor
    Vec(e_1, .., e_n) --> [num_1_1 .. num_1_m1 .. num_n_1 .. num_n_mn],T_Int


## TODO

  * conventions (e.g. `..` vs `...` vs `....` and how to handle indices)
  * evaluation contexts?
  * subsetting
  * symbols
  * tibbles
  * other literals/types (bool, float, string)
  * subset update
  * generalized subsetting
