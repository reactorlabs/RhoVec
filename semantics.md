# Formalization of dplyr

## Syntax

### Expressions

    e ::=
        | lit                                   # literal
        | Vec(e_1, ... , e_n)                   # vector constructor
        | e_1[]                                 # subset1 (nothing)
        | e_1[e_2]                              # subset1
        | e_1[-e_2]                             # subset1 (negative)
        | e_1[[e_2]]                            # subset2
        | x                                     # variable
        | x <- e                                # variable assignment
        | x[] <- e_1                            # subset1 assignment (nothing)
        | x[e_1] <- e_2                         # subset1 assignment
        | x[-e_1] <- e_2                        # subset1 assignment (negative)
        | x[[e_1]] <- e_2                       # subset2 assignment
        | e_1; e_2                              # sequencing
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

    C ::=
        | <>                                    # hole
        | Vec(v_1, .., v_n, C, e_1, .., e_m)
        | C[]
        | C[e]
        | v[C]
        | C[[e]]
        | v[[C]]
        | x <- C
        | x[C] <- e
        | x[v] <- C
        | x[[C]] <- e
        | x[[v]] <- C
        | C; e

#### Notes

  * We use angle brackets `C<e>` to notate an evaluation context `C` filled with
    expression `e`, since square brackets `[]` are used for vectors and
    subsetting.


### Environments

    E ::= { x -> v }*

#### Notes

  * An environment is a finite map from variables to values.
  * We use the notation `E{ x := v }` to denote environment `E` being updated
    with the new mapping of variable `x` to value `v`. If a previous value was
    mapped to `x`, it is overwritten.


## Operational Semantics

### `E C<e_1> --> E' C<e_2>`

Expression `e_1` in context `C` _reduces to_ expression `e_2` in context `C`,
updating environment `E` to the new environment `E'`.


    typeof(lit) = T
    -------------------------  :: E_Scalar2Vec
    E C<lit> --> E C<[lit],T>


    (v_1 = [lit_1_1 .. lit_1_m1],T) ... (v_n = [lit_n_1 .. lit_n_mn],T)
    ------------------------------------------------------------------------------  :: E_VecCtor
    E C<Vec(v_1, .., v_n)> --> E C<[lit_1_1 .. lit_1_m1 .. lit_n_1 .. lit_n_mn],T>


    v = [lit_1 .. lit_n],T
    ----------------------  :: E_Subset1_Nothing
    E C<v[]> --> E C<v>


    v_1 = [lit_1 .. lit_n],T
    v_2 = [num_1 .. num_m],T_Int
    forall i in 1..m : num_i >= 0 \/ num_i == NA_i
    v_3 = get_at_pos(v_1, v_2)
    ----------------------------------------------  :: E_Subset1_Positive
    E C<v_1[v_2]> --> E C<v_3>


    v_1 = [lit_1 .. lit_n],T
    v_2 = [bool_1 .. bool_m],T_Bool
    l = max(n, m)
    v_1' = extend(v_1, l-n)
    v_2' = recycle(v_2, v_2, v_2, l-m)
    v_2'' = bool_vec_to_pos(v_2', 1)
    v_3 = get_at_pos(v_1', v_2'')
    ----------------------------------  :: E_Subset1_Bool
    E C<v_1[v_2]> --> E C<v_3>


    v_1 = [lit_1 .. lit_n],T
    v_2 = [num_1 .. num_m],T_Int
    forall i in 1..m : num_i >= 0
    v_1' = gen_bool_vec(v1)
    v_2' = neg_vec_to_bool(v_2, v_1')
    v_2'' = bool_vec_to_pos(v_2', 1)
    v_3 = get_at_pos(v_1, v_2'')
    ---------------------------------  :: E_Subset1_Negative
    E C<v_1[-v_2] --> E C<v_3>


    v_1 = [lit_1 ... lit_n],T
    v_2 = [m],Int
    m in 1...n
    ----------------------------------  :: E_Subset2
    E C<v_1[[v_2]]> --> E C<[lit_m],T>


    x in E
    v = E(x)
    --------------------  :: E_Lookup
    E C<x> --> E C<v>


    E' = E{ x := v }
    -----------------------  :: E_Assign
    E C<x <- v> --> E' C<v>


    x in E
    E(x) = [lit_1 .. lit_n],T
    v_2 = [lit'_1 .. lit'_m],T
    n % m == 0
    v_3 = truncate_or_recycle(v_2, v_2, v_2, n-m)
    E' = E{ x := v_3 }
    ---------------------------------------------  :: E_Subset1_Nothing_Assign
    E C<x[] <- v_2> --> E' C<v_3>


    x in E
    E(x) = [lit_1 .. lit_l],T
    v_1 = [num_1 .. num_n],T_Int
    v_2 = [lit'_1 .. lit'_m],T
    forall i in 1..n : num_i >= 0
    v_1' = drop_zeros(v_2)
    n' = length(v_2')
    n' % m == 0
    v_2' = truncate_or_recycle(v_3, v_3, v_3, n'-m)
    v_3 = update_at_pos(v_1, v_2', v_3')
    E' = E{ x := v_3 }
    -----------------------------------------------  :: E_Subset1_Positive_Assign
    E C<x[v_1] <- v_2> --> E' C<v_3>


    x in E
    E(x) = v_1
    v_1 = [lit_1 .. lit_l],T
    v_2 = [bool_1 .. bool_n],T_Bool
    forall i in 1..n : bool_i =/= NA_b
    v_3 = [lit'_1 .. lit'_m],T
    j = max(l, n)
    v_1' = extend(v_1, j-l)
    v_2' = recycle(v_2, v_2, v_2, j-n)
    v_2'' = bool_vec_to_pos(v_2', 1)
    j' = length(v_2'')
    j' % m == 0
    v_3' = truncate_or_recycle(v_3, v_3, v_3, j'-m)
    v_4 = update_at_pos(v_1, v_2'', v_3')
    E' = E{ x := v_4 }
    ----------------------------------------------  :: E_Subset1_Bool_Assign
    E C<x[v_2] <- v_3> --> E' C<v_4>


    x in E
    E(x) = v_1
    v_1 = [lit_1 .. lit_l],T
    v_2 = [num_1 .. num_n],T_Int
    forall i in 1..n : num_i >= 0
    v_3 = [lit'_1 .. lit'_m],T
    v_1' = gen_bool_vec(v1)
    v_2' = neg_vec_to_bool(v_2, v_1')
    v_2'' = bool_vec_to_pos(v_2', 1)
    n' = length(v_2'')
    n' % m == 0
    v_3' = truncate_or_recycle(v_3, v_3, v_3, n'-m)
    v_4 = update_at_pos(v_1, v_2'', v_3')
    E' = E{ x := v_4 }
    -----------------------------------------------  :: E_Subset1_Negative_Assign
    E C<x[-v_2] <- v_3> --> E' C<v_4>


    x in E
    E(x) = v_1
    v_1 = [lit_1 .. lit_i lit_j lit_k .. lit_n],T
    v_2 = [k],T_Int
    v_3 = [lit],T
    v_4 = [lit_1 .. lit_i lit lit_k .. lit_n],T
    E' = E{ x := v_4 }
    ---------------------------------------------  :: E_Subset2_Assign
    E C<x[[v_2]] <- v_3> --> E' C<v_4>


    ---------------------  :: E_Sequence
    E C<v; e> --> E' C<e>


#### Notes

  * In R, some cases cause a warning to be emitted. For these semantics,
    warnings are treated as errors.

  * `E_Scalar2Vec`: There are no scalars in R; literals are implicitly converted
    to one-element vectors.

  * `E_VecCtor`: The vector constructor (more precisely, the combine function,
    `c()`) can take vectors as arguments, and flattens them to return a single
    vector.
    * At least one argument must be supplied; empty vectors cannot be
      constructed this way.
    * This is a departure from R, where `c()` constructs the `NULL` vector (i.e.
      a special 0-length vector of type `NULL`).

  * `E_Subset1_Nothing`: Subsetting a vector with nothing (e.g., `v[]`) returns
    the original vector.

  * `E_Subset1_Zero`: Subsetting a vector with `0` returns an empty vector of
    the same type.

  * `E_Subset1_Positive`: Subsetting takes a positional vector of positive
    integers, which describes the index of elements to return. The index vector
    is non-empty. Indices that are out of bounds or denoted by `NA_i` select
    `NA` (of the appropriate type).
    * Indices that are `0` do not select anything. Note that `x[0]` returns the
      empty vector.
    * If the index vector is negated, and all of its elements are negative, this
      is equivalent to positive subsetting.
    * Positive subsetting can also be accomplished with `v1[-v2]` if `v2` is
      a vector of negative indices. We could support this if we had general
      expressions, since `-c(1, 2, 3)` is an expression  that evaluates to
      `c(-1, -2, -3)`; for now, we use specific syntax `e[-e]`.

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

  * `E_Subset1_Negative`: Subsetting takes negative indices; either a vector of
    positive numbers is negated, or the vector contains negative numbers.
    Elements at those positions are excluded from the returned vector.
     * If a negative index is out of bounds, it is ignored.
     * Repeated indices are ignored.
     * `NA`s are not allowed as indices.
     * Internally, the negative vector is converted to a boolean vector, which
       is then converted to a positional vector.
     * Negative subsetting can also be accomplished with `v1[v2]` if `v2` is
       a vector of negative indices. We could support this if we had general
       expressions, since `-c(1, 2, 3)` is an expression that evaluates to
       `c(-1, -2, -3)`; for now, we use specific syntax `e[-e]`.

  * `E_Subset1_Nothing_Assign`: The entire vector is replaced by a new one.
    * If the replacement vector is too short, it gets recycled. If it is too
      long, it gets truncated.

  * `E_Subset1_Positive1_Assign` and `E_Subset1_Positive2_Assign`: Elements
    selected by the index vector are replaced by the corresponding right-hand
    side vector.
    * If an index is out of bounds (and positive), the vector is first extended
      with `NA`s (of the appropriate type).
    * `0`s are dropped from the index vector.
    * `NA`s are not allowed in the index vector.
      * In R, `NA`s actually are allowed, but only if the RHS is a
        single-element vector.
    * If the replacement vector is too short, it gets recycled. If it is too
      long, it gets truncated.
    * If the index vector has duplicate values, then the corresponding vector
      element will be overwritten, e.g. `v[c(1, 1)] <- c(10, 11)` replaces the
      first element with `11`.

  * `E_Subset1_Bool_Assign`: A boolean vector is used as an index, and those
     elements selected by the boolean vector are updated.
     * If the boolean vector is longer than the original vector, the original
       vector is extended with `NA`s (of the appropriate type).
     * If the boolean vector is too short, it is recycled.
     * The boolean vector is converted to a positional vector.
     * If the replacement vector is shorter than the positional vector, it gets
       recycled. If it is too long, it gets truncated.

  * `E_Subset1_Negative1_Assign` and `E_Subset1_Negative2_Assign`: Similar to
    `E_Subset1_Negative1` and `E_Subset1_Negative2`, subsetting takes negative
    indices; either a vector of positive numbers is negated, or the vector
    contains negative numbers. The elements excluded by these indices are
    updated by the RHS.
    * If a negative index is out of bounds, it is ignored.
    * Repeated indices are ignored.
    * `NA`s are not allowed as indices.
    * Internally, the negative vector is converted to a boolean vector, which
      is then converted to a positional vector.
    * Assignment follows the same rules as `E_Subset1_Positive1_Assign` and
      `E_Subset1_Positive2_Assign`, where the length of the index vector must
      be a multiple of the length of the replacement vector. Furthermore, the
      replacement vector is either truncated or recycled to achieve the correct
      length.

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


    v = [lit_1 .. lit_n],T
    ----------------------  :: Aux_Length
    length(v) = n


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
    -------------------------------------  :: Aux_BoolVecToPos_FalseCase
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


    ----------------------  :: Aux_Truncate_BaseCase
    truncate(v_1, 0) = v_1


    v_1 = [lit_1 .. lit_n lit],T
    v_1' = [lit_1 .. lit_n],T
    v = truncate(v_1', m+1)
    m < 0
    ----------------------------  :: Aux_Truncate_RecurseCase
    truncate(v_1, m) = v


    v = recycle(v_1, v_2, v_3, m)
    n >= 0
    -----------------------------------------  :: Aux_TruncateOrRecycle_PosCase
    truncate_or_recycle(v_1, v_2, v_3, m) = v


    v = truncate(v_1, m)
    n < 0
    -----------------------------------------  :: Aux_TruncateOrRecycle_NegCase
    truncate_or_recycle(v_1, v_2, v_3, m) = v


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
    j not in 1..m \/ j == NA_i
    v_1' = extend(v_1, j-m)
    v = update_at_pos(v_1', v_2, v_3)
    ---------------------------------  :: Aux_UpdateAtPos_OutBoundsCase
    update_at_pos(v_1, v_2, v_3) = v


## TODO

### Higher priority

These features are likely required.

  * doublecheck environment and assignment/sequencing
  * implement semantics so we can execute them and write test cases
  * semantics are probably buggy, already too complicated to keep track of with
    pencil-and-paper

### Medium priority

These might not be necessary, but are nice to have, or will be implemented
because other features depend on them.

  * heap, multiple environments
  * expressions
    * lift to vectors
    * recycling
  * symbols
    * named vectors and subsetting
  * data frames and/or tibbles
  * attributes
  * core syntax and sugar?
    * there seems to be a set of core operations that include extending,
      recycling, truncation, neg-to-bool conversion, bool-to-pos conversion,
      get-at-pos, update-at-pos, and so on.

### Lower priority

These features involve a lot of tedious mechanical work and might not be
necessary.

  * bool/int coercion
    * `x[0] <- 1` will upcast
  * other literals/types (float, string)
    * testing and coercions
    * note that `x[-0.1]` is coerced to `x[-1]` while `x[0.1]` is coerced to
      `x[0]`!
  * dimensions
    * matrices and arrays
  * lists
    * or treat them as "vectors" of some vector type
  * NULL vector (e.g. default empty vector)
  * `$` operator
    * `x$y` is sugar for `x[["y"]]`, implies symbol-to-string coercion
  * promises and laziness

