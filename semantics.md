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

    bool ::=
        | False                                 # false
        | True                                  # true
        | NA_b                                  # missing

    num ::=
        | /[0-9]+/                              # number
        | NA_i                                  # missing

#### Notes

  * R represents missing values with `NA` (not applicable). Each type has its
    own missing value. I.e. there are three boolean values: `True`, `False`, and
    `NA_b`.
  * We use regular expression notation to specify what kind of literals are
    allowed. `/[0-9]+/` describes one or more occurrences of the digits from
    0 to 9.


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


    v_1 = [lit_1 .. lit_n],T
    v_2 = [num_1 .. num_m],T_Int
    v_3 = get_at_pos(v_1, v_2)
    ------------------------------  :: E_Subset1_Positive
    v_1[v_2] --> v_3


    v_1 = [lit_1 .. lit_n],T
    v_2 = [bool_1 .. bool_m],T_Bool
    v_2' = bool_vec_to_pos(v_2, 1, n)
    v_3 = get_at_pos(v_1, v_2')
    ---------------------------------  :: E_Subset1_Bool
    v_1[v_2] --> v_3


    v_1 = [lit_1 ... lit_n],T
    v_2 = [m],Int
    m in 1...n
    ------------------------  :: E_Subset2
    v_1[[v_2]] --> [lit_m],T

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

  * `E_Subset1_Positive`: Subsetting takes a positional vector of positive
    integers, which describes the index of elements to return. The index vector
    non-empty. Indices that are out of bounds or denoted by `NA_i` select `NA`
    (of the appropriate type).

  * `E_Subset1_Bool`: Subsetting takes a boolean vector. If the boolean vector
    contains `True`, then the element at the corresponding location is selected;
    if it contains `False`, then the corresponding element is skipped; and if it
    contains `NA_b`, then `NA` (of the appropriate type) is selected.
    * If the boolean vector is too long, then boolean values of `True` and
      `NA_b` select `NA` (of the appropriate type), while `False` values are
      skipped.
    * Internally, we convert a boolean vector into a positional vector (see
      previous case).

  * `E_Subset2`: Subsetting a vector with `[[` returns a single-element vector.
    The vector must contain at least one element, and the index must be within
    bounds; it cannot be `0` or nothing.


### Auxiliary Functions

    ---------------------  :: Aux_Typeof_Bool
    typeof(bool) = T_Bool


    -------------------  :: Aux_Typeof_Int
    typeof(num) = T_Int


    -----------------  :: Aux_NA_Bool
    NA(T_Bool) = NA_b


    ----------------  :: Aux_NA_Int
    NA(T_Int) = NA_i


    typeof(lit) = T
    v = [lit_1 .. lit_n],T
    ----------------------------------------  :: Aux_Prepend
    prepend(lit, v) = [lit lit_1 .. lit_n],T


    v_1 = [lit_1 .. lit_n],T
    v_2 = [],T_Int
    ---------------------------  :: Aux_GetAtPos_Base
    get_at_pos(v_1, v_2) = [],T


    v_1 = [lit_1 .. lit_n],T
    v_2 = [i num_1 .. num_m],T_Int
    v_2' = [num_1 .. num_m],T_Int
    i in 1..n
    v_3 = get_at_pos(v_1, v_2')
    v = prepend(lit_i, v_3)
    ------------------------------  :: Aux_GetAtPos_InBoundsCase
    get_at_pos(v_1, v_2) = v


    v_1 = [lit_1 .. lit_n],T
    v_2 = [i num_1 .. num_m],T_Int
    v_2' = [num_1 .. num_m],T_Int
    i not in 1..n \/ i = NA_i
    v_3 = get_at_pos(v_1, v_2')
    v = prepend(NA(T), v_3)
    ------------------------------  :: Aux_GetAtPos_OutBoundsCase
    get_at_pos(v_1, v_2) = v


    v_1 = [],T
    -------------------------------------  :: Aux_BoolVecToPos_Base
    bool_vec_to_pos(v_1, i, n) = [],T_Int


    v_1 = [True bool_1 .. bool_m],T_Bool
    v_1' = [bool_1 .. bool_m],T_Bool
    v_2 = bool_vec_to_pos(v_1', i+1, n)
    v = prepend(i, v_2)
    i <= n
    ------------------------------------   :: Aux_BoolVecToPos_TrueCase
    bool_vec_to_pos(v_1, i, n) = v


    v_1 = [NA_b bool_1 .. bool_m],T_Bool
    v_1' = [bool_1 .. bool_m],T_Bool
    v_2 = bool_vec_to_pos(v_1', i+1, n)
    v = prepend(NA_i, v_2)
    i <= n
    ------------------------------------   :: Aux_BoolVecToPos_NACase
    bool_vec_to_pos(v_1, i, n) = v


    v_1 = [bool_0 bool_1 .. bool_m],T_Bool
    v_1' = [bool_1 .. bool_m],T_Bool
    v_2 = bool_vec_to_pos(v_1', i+1, n)
    v = prepend(NA_i, v_2)
    bool_0 = True \/ bool_0 = NA_b
    i > n
    ------------------------------------   :: Aux_BoolVecToPos_OutBoundsCase
    bool_vec_to_pos(v_1, i, n) = v


    v_1 = [False bool_1 .. bool_m],T_Bool
    v_1' = [bool_1 .. bool_m],T_Bool
    v = bool_vec_to_pos(v_1', i+1, n)
    ------------------------------------  :: Aux_BoolVecToPos_FalseCase
    bool_vec_to_pos(v_1, i, n) = v

## TODO

### Higher priority

These features are likely required.

  * recycling (lgl index vector too short)
  * negative (including -0) subsetting
    * convert negative to positive (positional)
  * subset assignment
  * expressions
  * symbols
  * tibbles

### Medium priority

These might not be necessary, but are nice to have, or will be implemented
because other features depend on them.

  * named vectors and subsetting
  * general assignment to variables
  * core syntax and sugar?

### Lower priority

These features involve a lot of tedious mechanical work and might not be
necessary.

  * other literals/types (float, string)
  * coercions
  * other negative indexing
    * v[c(-1, -2, -4)]
    * double negative = positive indexing v[-c(-1, -2, -4)]
  * matrices and arrays
  * NULL vector
  * `$` operator
  * subset assignment (out-of-bounds)
  * promises and laziness

