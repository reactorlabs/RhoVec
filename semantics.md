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
  * `...` denotes a sequence containing at least one element.
  * `[` extracts a smaller vector, while `[[` extracts a single element. Note
    that "a single element" may be a one-element vector.
  * Bare values `v` are not available in the surface syntax; they are required
    here for evaluation.


### Literals

    lit ::=
        | bool                                  # boolean
        | num                                   # integer


### Values

    v ::=
        | [lit_1 .. lit_n],T                     # vector (and its type)

#### Notes

  * There are no scalar values, because scalars are actually one-element
    vectors.
  * Vectors are homogeneous; every element in a vector has the same type.


### Types

    T ::=
        | T_Bool                                # boolean
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

    typeof(lit) = T
    ---------------  :: E_Scalar2Vec
    lit --> [lit],T


    (v_1 = [lit_1_1 .. lit_1_m1],T) .. (v_n = [lit_n_1 .. lit_n_mn],T)
    --------------------------------------------------------------------  :: E_VecCtor
    Vec(v_1, .., v_n) --> [lit_1_1 .. lit_1_m1 .. lit_n_1 .. lit_n_mn],T


    v = [lit_1 .. lit_n],T
    ----------------------  :: E_Subset1_Nothing
    v[] --> v


    v_1 = [lit_1 .. lit_n],T
    v_2 = [0],Int
    ------------------------  :: E_Subset1_Zero
    v_1[v_2] --> [],T


    v_1 = [lit_1 ... lit_n],T
    v_2 = [m],Int
    m in 1...n
    ------------------------  :: E_Subset2
    v_1[[v_2]] --> [num_m],T

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
    The vector must contain at least one element, and the index must be within
    bounds; it cannot be `0` or nothing.


### Auxiliary Functions

    ---------------------  :: Aux_TypeofBool
    typeof(bool) = T_Bool


    -------------------  :: Aux_TypeofInt
    typeof(num) = T_Int


## TODO

### Higher priority

These features are likely required.

  * logical subsetting
  * missing values
  * subset assignment
  * recycling
  * expressions
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

