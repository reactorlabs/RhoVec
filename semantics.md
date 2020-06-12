# Formalization of dplyr

## Syntax

### Expressions

    e ::=
        | lit                                   # literal
        | Vec(e_1, .. , e_n)                    # vector constructor
        | e_1[e_2]                              # subset1
        | e_1[]                                 # subset1 (nothing)
        | e_1[[e_2]]                            # subset2
        | v                                     # value

#### Notes

  * `..` denotes a possibly empty sequence, i.e., `e_1, .., e_n` may be a list
    of zero expressions.
  * `[` extracts a smaller vector, while `[[` extracts a single element. Note
    that "a single element" may be a one-element vector.
  * Bare values `v` are not available in the surface syntax; they are required
    here for evaluation.


### Literals

    lit ::=
        | num                                   # integer


### Values

    v ::=
        | [lit_1 .. lit_n],T                     # vector (and its type)

#### Notes

  * There are no scalar values, because scalars are actually one-element
    vectors.


### Types

    T ::=
        | T_Int                                 # integer


### Evaluation contexts

    E ::=
        | <>                                    # hole
        | Vec(v_1, .., v_n, E, e_1, .., e_m)
        | E[e]
        | v[E]
        | E[]
        | E[[e]]
        | v[[E]]

#### Notes

  * We use angle brackets `E<e>` to notate an evaluation context `E` filled with
    expression `e`, since square brackets `[]` are used for vectors and
    subsetting.


## Operational Semantics

### `e_1 --> e_2`

`e_1` reduces to `e_2`

    -------------------  :: E_Scalar2Vec
    num --> [num],T_Int


    (v_1 = [num_1_1 .. num_1_m1],T_Int) .. (v_n = [num_n_1 .. num_n_mn],T_Int)
    --------------------------------------------------------------------------  :: E_VecCtor
    Vec(v_1, .., v_n) --> [num_1_1 .. num_1_m1 .. num_n_1 .. num_n_mn],T_Int


    v = [num_1 .. num_n],Int
    ------------------------  :: E_Subset1_Nothing
    v[] --> v


    v_1 = [num_1 .. num_n],Int
    v_2 = [0],Int
    --------------------------  :: E_Subset1_Zero
    v_1[v_2] --> [],Int


    v_1 = [num_1 .. num_n],Int
    v_2 = [m],Int
    1 <= m <= n
    --------------------------  :: E_Subset2
    v_1[[v_2]] --> [num_m],Int

#### Notes

  * `E_Scalar2Vec`: There are no scalars in R; literals are implicitly converted
    to one-element vectors.

  * `E_VecCtor`: The vector constructor (more precisely, the combine function,
    `c()`) can take vectors as arguments, and flattens them to return a single
    vector.

  * `E_Subset1_Nothing`: Subsetting a vector with nothing (e.g., `v[]`) returns
    the original vector.

  * `E_Subset1_Zero`: Subsetting a vector with `0` returns an empty vector of
     the same type.

  * `E_Subset2`: Subsetting a vector with `[[` returns a single-element vector.
    The index for `[[` must be within bounds; it cannot be `0` or nothing.


## TODO

### Higher priority

These features are likely required.

  * boolean literals/types/vectors
  * logical subsetting
  * missing values
  * subset assignment
  * symbols
  * tibbles

### Medium priority

These might not be necessary, but are nice to have, or will be implemented
because other features depend on them.

  * positive and negative (including -0) subsetting
      * generalization of logical subsetting
  * subsetting with missing and out-of-bounds indices
  * named vectors and subsetting
  * general assignment to variables

### Lower priority

These features involve a lot of tedious mechanical work and might not be
necessary.

  * other literals/types (float, string)
  * coercions
  * matrices and arrays
  * NULL vector
  * `$` operator
  * subset assignment (out-of-bounds)
  * promises and laziness

