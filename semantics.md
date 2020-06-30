# Formalization of dplyr

## Syntax

### Expressions

    e ::=
        | lit                                   # literal
        | Vec(e_1, .. , e_n)                    # vector constructor
        | e_1[]                                 # subset1 (nothing)
        | e_1[e_2]                              # subset1
        | e_1[e_2] <- e_3                       # subset1 assignment
        | e_1[[e_2]]                            # subset2
        | e_1[[e_2]] <- e_3                     # subset2 assignment
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
        | E[]
        | E[e]
        | v[E]
        | E[e] <- e
        | v[E] <- e
        | v[v] <- E
        | E[[e]]
        | v[[E]]
        | E[[e]] <- e
        | v[[E]] <- e
        | v[[v]] <- E

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
    v_2 = [num_1 .. num_m],T_Int
    forall i in 1..m : num_i >= 0
    v_3 = get_at_pos(v_1, v_2)
    -----------------------------  :: E_Subset1_Positive1
    v_1[v_2] --> v_3


    v_1 = [lit_1 .. lit_n],T
    v_2 = [num_1 .. num_m],T_Int
    forall i in 1..m : num_i <= 0
    v_2' = negate_vec(v_2)
    v_3 = get_at_pos(v_1, v_2')
    -----------------------------  :: E_Subset1_Positive2
    v_1[-v_2] --> v_3


    v_1 = [lit_1 .. lit_n],T
    v_2 = [num_1 .. num_m],T_Int
    forall i in 1..m : num_i >= 0
    v_1' = gen_bool_vec(v1)
    v_2' = neg_vec_to_bool(v_2, v_1')
    v_2'' = bool_vec_to_pos(v_2', 1)
    v_3 = get_at_pos(v_1, v_2'')
    ---------------------------------  :: E_Subset1_Negative1
    v_1[-v_2] --> v_3


    v_1 = [lit_1 .. lit_n],T
    v_2 = [num_1 .. num_m],T_Int
    forall i in 1..m : num_i <= 0
    v_1' = gen_bool_vec(v1)
    v_2' = negate_vec(v_2)
    v_2'' = neg_vec_to_bool(v_2', v_1')
    v_2''' = bool_vec_to_pos(v_2'', 1)
    v_3 = get_at_pos(v_1, v_2''')
    -----------------------------------  :: E_Subset1_Negative2
    v_1[v_2] --> v_3


    v_1 = [lit_1 .. lit_n],T
    v_2 = [bool_1 .. bool_m],T_Bool
    l = max(n, m)
    v_1' = extend(v_1, l-n)
    v_2' = recycle(v_2, v_2, v_2, l-m)
    v_2'' = bool_vec_to_pos(v_2', 1)
    v_3 = get_at_pos(v_1, v_2'')
    ----------------------------------  :: E_Subset1_Bool
    v_1[v_2] --> v_3


    v_1 = [lit_1 .. lit_n],T
    v_2 = [lit'_1 .. lit'_m],T
    n % m = 0
    v_3 = recycle(v_2, v_2, v_2, n-m)
    ---------------------------------  :: E_Subset1_Nothing_Assign
    v_1[] <- v_2 --> v_3


    v_1 = [lit_1 .. lit_l],T
    v_2 = [num_1 .. num_n],T_Int
    v_3 = [lit'_1 .. lit'_m],T
    n % m == 0
    v_2' = drop_zeros(v_2)
    v_3' = recycle(v_3, v_3, v_3, n-m)
    v_4 = update_at_pos(v_1, v_2', v_3')
    -----------------------------------  :: E_Subset1_Positive1_Assign
    v_1[v_2] <- v_3 --> v_4


    v_1 = [lit_1 .. lit_l],T
    v_2 = [num_1 .. num_n],T_Int
    forall i in 1..n : num_i <= 0
    v_3 = [lit'_1 .. lit'_m],T
    n % m == 0
    v_2' = negate_vec(v_2)
    v_2' = drop_zeros(v_2')
    v_3' = recycle(v_3, v_3, v_3, n-m)
    v_4 = update_at_pos(v_1, v_2'', v_3')
    -------------------------------------  :: E_Subset1_Positive2_Assign
    v_1[-v_2] <- v_3 --> v_4


    v_1 = [lit_1 ... lit_n],T
    v_2 = [m],Int
    m in 1...n
    -------------------------  :: E_Subset2
    v_1[[v_2]] --> [lit_m],T


    v_1 = [lit_1 .. lit_i lit_j lit_k .. lit_n],T
    v_2 = [k],T_Int
    v_3 = [lit],T
    v_4 = [lit_1 .. lit_i lit lit_k .. lit_n],T
    ---------------------------------------------  :: E_Subset2_Assign
    v_1[[v_2]] <- v_3 --> v_4

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

  * `E_Subset1_Positive1` and `E_Subset1_Positive2`: Subsetting takes
    a positional vector of positive integers, which describes the index of
    elements to return. The index vector is non-empty. Indices that are out of
    bounds or denoted by `NA_i` select `NA` (of the appropriate type).
    * Indices that are `0` do not select anything. Note that `x[0]` returns the
      empty vector.
    * If the index vector is negated, and all of its elements are negative, this
      is equivalent to positive subsetting.

  * `E_Subset1_Negative1` and `E_Subset1_Negative2`: Subsetting takes negative
     indices; either a vector of positive numbers is negated, or the vector
     contains negative numbers. Elements at those positions are excluded from
     the returned vector.
     * If a negative index is out of bounds, it is ignored.
     * Internally, the negative vector is converted to a boolean vector, which
       is then converted to a positional vector.

  * `E_Subset1_Bool`: Subsetting takes a boolean vector of the same length. If
    the boolean vector contains `True`, then the element at the corresponding
    location is selected; if it contains `False`, then the corresponding element
    is skipped; and if it contains `NA_b`, then `NA` (of the appropriate type)
    is selected.
    * If the boolean vector is too long, we extend the other vector with `NA`s
      (of the appropriate type) until both vectors have the same length.
    * If the boolean vector is too short, we recycle its elements until both
      vectors have the same length.
    * Internally, we convert a boolean vector into a positional vector (see
      previous cases).

  * `E_Subset1_Nothing_Assign`: The entire vector is replaced by a new one. The
    length of the original vector must be a multiple of the length of the
    replacement vector, which is recycled.

  * `E_Subset1_Positive1_Assign` and `E_Subset1_Positive2_Assign`: Elements
    selected by the index vector are replaced by the corresponding right-hand
    side vector.
    * If an index is out of bounds (and positive), the vector is first extended
      with `NA`s (of the appropriate type).
    * `0`s are dropped from the index vector.
    * The length of the index vector (including duplicates) must be a multiple
      of the length of the replacement vector, which is recycled.
    * If the index vector has duplicate values, then the corresponding vector
      element will be overwritten, e.g. `v[c(1, 1)] <- c(10, 11)` replaces the
      first element with `11`.

  * `E_Subset2`: Subsetting a vector with `[[` returns a single-element vector.
    The vector must contain at least one element, and the index must be within
    bounds; it cannot be `0` or nothing.

  * `E_Subset2_Assign`: Assignment with `[[` replaces a single element of the
    vector with a single new value.


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


    typeof(lit) = T
    v = [lit_1 .. lit_n],T
    ---------------------------------------  :: Aux_Append
    append(v, lit) = [lit_1 .. lit_n lit],T


    v_1 = [lit_1 .. lit_n],T
    v_2 = [],T_Int
    ---------------------------  :: Aux_GetAtPos_BaseCase
    get_at_pos(v_1, v_2) = [],T


    v_1 = [lit_1 .. lit_n],T
    v_2 = [i num_1 .. num_m],T_Int
    i = 0
    v_2' = [num_1 .. num_m],T_Int
    v = get_at_pos(v_1, v_2')
    ------------------------------  :: Aux_GetAtPos_ZeroCase
    get_at_pos(v_1, v_2) = v


    v_1 = [lit_1 .. lit_n],T
    v_2 = [i num_1 .. num_m],T_Int
    i in 1..n
    v_2' = [num_1 .. num_m],T_Int
    v_3 = get_at_pos(v_1, v_2')
    v = prepend(lit_i, v_3)
    ------------------------------  :: Aux_GetAtPos_InBoundsCase
    get_at_pos(v_1, v_2) = v


    v_1 = [lit_1 .. lit_n],T
    v_2 = [i num_1 .. num_m],T_Int
    i not in 1..n \/ i = NA_i
    v_2' = [num_1 .. num_m],T_Int
    v_3 = get_at_pos(v_1, v_2')
    v = prepend(NA(T), v_3)
    ------------------------------  :: Aux_GetAtPos_OutBoundsCase
    get_at_pos(v_1, v_2) = v


    v_1 = [],T_Bool
    ----------------------------------  :: Aux_BoolVecToPos_BaseCase
    bool_vec_to_pos(v_1, i) = [],T_Int


    v_1 = [True bool_1 .. bool_n],T_Bool
    v_1' = [bool_1 .. bool_n],T_Bool
    v_2 = bool_vec_to_pos(v_1', i+1)
    v = prepend(i, v_2)
    ------------------------------------   :: Aux_BoolVecToPos_TrueCase
    bool_vec_to_pos(v_1, i) = v


    v_1 = [False bool_1 .. bool_n],T_Bool
    v_1' = [bool_1 .. bool_n],T_Bool
    v = bool_vec_to_pos(v_1', i+1)
    ------------------------------------  :: Aux_BoolVecToPos_FalseCase
    bool_vec_to_pos(v_1, i) = v


    v_1 = [NA_b bool_1 .. bool_n],T_Bool
    v_1' = [bool_1 .. bool_n],T_Bool
    v_2 = bool_vec_to_pos(v_1', i+1)
    v = prepend(NA_i, v_2)
    ------------------------------------   :: Aux_BoolVecToPos_NACase
    bool_vec_to_pos(v_1, i) = v


    --------------------  :: Aux_Extend_BaseCase
    extend(v_1, 0) = v_1


    v_1 = [lit_1 .. lit_n],T
    v_1' = append(v_1, NA(T))
    v = extend(v_1', m-1)
    m > 0
    -------------------------  :: Aux_Extend_RecurseCase
    extend(v_1, m) = v


    -------------------------------  :: Aux_Recycle_BaseCase
    recycle(v_1, v_2, v_3, 0) = v_1


    v_1 = [lit_i .. lit_j],T
    v_2 = [],T
    v = recycle(v_1, v_3, v_3, m)
    m > 0
    -----------------------------  :: Aux_Recycle_CycleCase
    recycle(v_1, v_2, v_3, m) = v


    v_1 = [lit_i .. lit_j],T
    v_2 = [lit lit_1 .. lit_n],T
    v_1' = append(v_1, lit)
    v_2' = [lit_1 .. lit_n],T
    v = recycle(v_1', v_2', v_3, m-1)
    m > 0
    ---------------------------------  :: Aux_Recycle_RecurseCase
    recycle(v_1, v_2, v_3, m) = v


    v_1 = [],T_Int
    --------------------------  :: Aux_NegateVec_BaseCase
    negate_vec(v_1) = [],T_Int


    v_1 = [lit lit_1 .. lit_n],T_Int
    v_1' = [lit_1 .. lit_n],T_Int
    v_2 = negate_vec(v_1')
    v = prepend(-lit, v_2)
    --------------------------------  :: Aux_NegateVec_RecurseCase
    negate_vec(v_1) = v


    v_1 = [],T
    ---------------------------  :: Aux_GenBoolVec_BaseCase
    gen_bool_vec(v_1) = [],T_Bool


    v_1 = [lit lit_1 .. lit_n],T
    v_2 = [lit_1 .. lit_n],T
    v_3 = gen_bool_vec(v_2)
    v = prepend(True, v_3)
    ----------------------------  :: Aux_GenBoolVec_RecurseCase
    gen_bool_vec(v_1) = v


    v_1 = [],T_Int
    v_2 = [bool_1 .. bool_n],T_Bool
    -------------------------------  :: Aux_NegVecToBool_BaseCase
    neg_vec_to_bool(v_1, v_2) = v_2


    v_1 = [j num_1 .. num_n],T_Int
    v_1' = [num_1 .. num_n],T_Int
    v_2 = [bool_1 .. bool_i bool_j bool_k .. bool_m],T_Bool
    v_2' = [bool_1 .. bool_i False bool_k .. bool_m],T_Bool
    v = neg_vec_to_bool(v_1', v_2')
    -------------------------------------------------------  :: Aux_NegVecToBool_InBoundsCase
    neg_vec_to_bool(v_1, v_2) = v


    v_1 = [j num_1 .. num_n],T_Int
    v_1' = [num_1 .. num_n],T_Int
    v_2 = [bool_1 .. bool_m],T_Bool
    j not in 1..m
    v = neg_vec_to_bool(v_1', v_2)
    -------------------------------  :: Aux_NegVecToBool_OutBoundsCase
    neg_vec_to_bool(v_1, v_2) = v


    v_1 = [],T_Int
    --------------------------  :: Aux_DropZeros_BaseCase
    drop_zeros(v_1) = [],T_Int


    v_1 = [0 num_1 .. num_n],T_Int
    v_1' = [num_1 .. num_n],T_Int
    v = drop_zeros(v_1')
    ------------------------------  :: Aux_DropZeros_ZeroCase
    drop_zeros(v_1) = v


    v_1 = [num num_1 .. num_n],T_Int
    v_1' = [num_1 .. num_n],T_Int
    v_1'' = drop_zeros(v_1')
    v = prepend(num, v_1'')
    --------------------------------  :: Aux_DropZeros_NonZeroCase
    drop_zeros(v_1) = v


    v_1 = [lit_1 .. lit_n],T
    v_2 = [],T_Int
    v_3 = [],T
    ----------------------------------  :: Aux_UpdateAtPos_BaseCase
    update_at_pos(v_1, v_2, v_3) = v_1


    v_1 = [lit_1 .. lit_i lit_j lit_k .. lit_n],T
    v_2 = [j num_1 .. num_m],T_Int
    v_3 = [lit' lit'_1 .. lit'_m],T
    v_1' = [lit_1 .. lit_i lit' lit_k .. lit_n],T
    v_2' = [num_1 .. num_m],T_Int
    v_3' = [lit'_1 .. lit'_m],T
    v = update_at_pos(v_1', v_2', v_3')
    ---------------------------------------------  :: Aux_UpdateAtPos_InBoundsCase
    update_at_pos(v_1, v_2, v_3) = v


    v_1 = [lit_1 .. lit_n],T
    v_2 = [j num_1 .. num_m],T_Int
    v_3 = [lit'_1 .. lit'_m],T
    j not in 1..m
    v_1' = extend(v_1, j-m)
    v = update_at_pos(v_1', v_2, v_3)
    ---------------------------------  :: Aux_UpdateAtPos_OutBoundsCase
    update_at_pos(v_1, v_2, v_3) = v


## TODO

### Higher priority

These features are likely required.

  * negative subsetting
    * handle NAs
  * subset assignment
    * subset positive
      * handle NAs
    * subset bool
      * out-of-bounds
      * recycling
      * NA
    * subset negative
      * out-of-bounds
      * recycling
      * 0 and NA
  * expressions
    * lift to vectors
    * recycling
  * symbols
    * named vectors and subsetting
  * heap semantics; i.e. assignment to variables
  * data frames and/or tibbles

### Medium priority

These might not be necessary, but are nice to have, or will be implemented
because other features depend on them.

  * attributes
  * core syntax and sugar?

### Lower priority

These features involve a lot of tedious mechanical work and might not be
necessary.

  * bool/int coercion
    * `x[0] <- 1` will upcast
  * other literals/types (float, string)
    * testing and coercions
  * dimensions
    * matrices and arrays
  * lists
    * or treat them as "vectors" of some vector type
  * NULL vector
  * `$` operator
    * `x$y` is sugar for `x[["y"]]`, implies symbol-to-string coercion
  * promises and laziness

