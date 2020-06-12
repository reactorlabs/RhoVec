# Formalization of dplyr

## Syntax

### Expressions

    e ::=
        | lit                           # literal
        | Vec(e_1, .. , e_n)            # vector constructor
        | v                             # value

#### Notes

  * Bare values `v` are not available in the surface syntax; they are required
    here for evaluation.


### Literals

    lit ::=
        | num                           # integer


### Values

    v ::=
        | [lit_1 .. lit_n],T           # vector (and its type)

#### Notes

  * There are no scalar values, because scalars are actually one-element
    vectors.


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

#### Notes

  * `E_Scalar2Vec`: There are no scalars in R; literals are implicitly converted
    to one-element vectors.

  * `E_VecCtor`: The vector constructor (more precisely, the combine function,
    `c()`) can take vectors as arguments, and flattens them to return a single
    vector.


## TODO

  * conventions (e.g. `..` vs `...` vs `....` and how to handle indices)
  * evaluation contexts?
  * subsetting
  * symbols
  * tibbles
  * other literals/types (bool, float, string)
  * missing values
  * NULL vector
  * subset update
  * generalized subsetting
