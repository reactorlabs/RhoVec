# Formalization of R vectors

## Syntax

### Literals

    bool ::=
        | NA_b                                      # missing
        | F                                         # false
        | T                                         # true

    int ::=
        | NA_i                                      # missing
        | <digit>+                                  # number

    lit ::=
        | NULL                                      # null vector
        | bool                                      # boolean
        | int                                       # integer

#### Notes

  * R represents missing values with `NA` (not applicable). Each type has its
    own missing value. E.g., there are three boolean values: `T`, `F`, and
    `NA_b`.
  * `NULL` is the literal for the null vector, a vector of length 0 and type
    `T_Null`.
  * In R, numeric literals default to (double precision) floating point numbers.
    Integer literals must have an `l` or `L` suffix. For now, we only support
    integer literals, but do not require the suffix.
  * We treat `-1` as a negation expression of the single-element vector `1`.


### Expressions

    e ::=
        | lit                                       # literal
        | x                                         # variable
        | Combine(e_1, .. , e_n)                    # combine
        | Matrix(e_1, e_2, e_3)                     # matrix
        | Dim(e)                                    # dimension getter
        | -e                                        # negate
        | e_1[]                                     # subset1 (nothing)
        | e_1[e_2]                                  # subset1
        | e_1[[e_2]]                                # subset2
        | e_1; ... ; e_2                            # sequencing
        | x <- e                                    # variable assignment
        | Dim(x) <- e                               # dimension setter
        | x[] <- e_1                                # subset1 (nothing) assignment
        | x[e_1] <- e_2                             # subset1 assignment
        | x[[e_1]] <- e_2                           # subset2 assignment
        | v                                         # value (vector)

#### Notes

  * An identifier `x` is a token that matches the regex
      `/[a-zA-Z.][a-zA-Z0-9._]*/`, i.e. the first character must be a letter
      or `.`, but the remaining characters may be alphanumeric, `_`, or `.`.
  * `..` denotes a possibly empty sequence, i.e., `e_1, .., e_n` may be a list
    of zero expressions.
  * `...` denotes a sequence containing at least one element.
  * `[` extracts a smaller vector, while `[[` extracts a single element. Note
    that "a single element" may be a one-element vector.
  * Bare values `v` are not available in the surface syntax; they are required
    here for evaluation.


### Types

    T ::=
        | T_Null                                    # Null
        | T_Bool                                    # boolean
        | T_Int                                     # integer


### Values

    v ::=
        | [lit_1 .. lit_n],T,v_d                    # vector, type, and dimensions

    Vnull ::=
        | [],T_Null,Vnull                           # null vector

#### Notes

  * There are no scalar values, because scalars are actually one-element
    vectors.
  * Vectors are homogeneous; every element in a vector has the same type.
  * `v_d` is the dimensions vector, and must have type `T_Int` or `T_Null`.
    * The dimensions vector itself may have dimensions.
    * In R, the dimensions vector is part of the attributes list.
  * `Vnull` refers to the singleton null vector. Note that `Vnull` is a value
    that cannot be referred to from the surface syntax, unlike the `NULL`
    literal.
  * `Vnull` is cyclic, as its dimensions vector is also null and therefore
     refers to itself.


### Evaluation contexts

    C ::=
        | <>                                        # hole
        | Combine(v_1, .., v_n, C, e_1, .., e_m)    # combine
        | Matrix(C, e_2, e_3)                       # matrix
        | Matrix(v_1, C, e_3)
        | Matrix(v_1, v_2, C)
        | Dim(C)                                    # dimension getter
        | -C                                        # negate
        | C[]                                       # subset1 (nothing)
        | C[e]                                      # subset1
        | v[C]
        | C[[e]]                                    # subset2
        | v[[C]]
        | v_1; .. ; v_n; C; e_1; .. ; e_m           # sequencing
        | x <- C                                    # variable assignment
        | Dim(x) <- C                               # dimension setter
        | x[] <- C                                  # subset1 (nothing) assignment
        | x[C] <- e                                 # subset1 assignment
        | x[v] <- C
        | x[[C]] <- e                               # subset2 assignment
        | x[[v]] <- C

#### Notes

  * We use angle brackets `C<e>` to notate an evaluation context `C` filled with
    expression `e`, since square brackets `[]` are used for vectors and
    subsetting.
  * Evaluation proceeds in left-to-right order.


### Environments

    E ::= { x -> v }*

#### Notes

  * An environment is a finite map from variables to values.
  * We use the notation `E{ x := v }` to denote environment `E` being updated
    with the new mapping of variable `x` to value `v`. If a previous value was
    mapped to `x`, it is overwritten.
  * `E(x)` denotes the value that was bound to `x` in environment `E`.


## Operational Semantics

### `E C<e_1> --> E' C<e_2>`

Expression `e_1` in context `C` _reduces to_ expression `e_2` in context `C`,
updating environment `E` to the new environment `E'`.



    -------------------------------  :: E_Lit_Null
    E C<NULL> --> E C<Vnull>

The NULL literal represents a vector of length zero, with type T_Null.


    typeof(lit) = T /\ T =/= T_Null
    -------------------------------  :: E_Lit
    E C<lit> --> E C<[lit],T,Vnull>

There are no scalars in R; non-null literals are implicitly converted to
one-element vectors.


    x in E
    v = E(x)
    --------------------  :: E_Var
    E C<x> --> E C<v>

    Error if:
      - x not in E

Looks up the value of `x` in the environment.



    ---------------------------------  :: E_Combine_Empty
    E C<Combine()> --> E C<Vnull>

Passing zero arguments to `Combine` will return the null vector, `Vnull`.


    (v_1 = [lit_1_1 .. lit_1_m1],T,v_d1) ... (v_n = [lit_n_1 .. lit_n_mn],T,v_d2)
    -----------------------------------------------------------------------------------------  :: E_Combine
    E C<Combine(v_1, ..., v_n)> --> E C<[lit_1_1 .. lit_1_m1 .. lit_n_1 .. lit_n_mn],T,Vnull>

    Error if:
      - vectors have different types

`Combine` takes vectors as arguments, and combines/flattens them into a single
vector. The dimensions vectors are ignored, so the resulting vector as null
dimensions.

_Note:_ In R, arguments may also have different types, as vectors will be
coerced to a common type.


    v_1 = [],T,Vnull
    v_2 = [i],T_Int,v_d2
    v_3 = [j],T_Int,v_d3
    n2 = i*j
    v_1' = NA(T)
    v_1'' = recycle(v_1', v_1', v_1', n2-1)
          = [lit_1 .. lit_n2],T,Vnull
    v_d2 = [i j],T_Int,Vnull
    v = [lit_1 .. lit_n2],T,v_d2
    i > 0 /\ j > 0
    ---------------------------------------  :: E_Matrix_Empty
    E C<Matrix(v_1, v_2, v_3)> --> E C<v>

    Error if:
      - i is negative
      - j is negative

If the provided vector `v_1` is empty, then it is converted to an `NA` of the
appropriate type, and extended to fill the required dimensions.


    v_1 = [lit_1 .. lit_n1],T,v_d1
    v_2 = [i],T_Int,v_d2
    v_3 = [j],T_Int,v_d3
    v_1' = strip_dim(v_1)
    n2 = i*j
    v_1'' = truncate(v_1', n1-n2)
    n2 % n1 == 0
    v_1''' = recycle(v_1'', v_1'', v_1'', n2-n1)
           = [lit_1 .. lit_n2],T,v_d1
    v_d2 = [i j],T_Int,Vnull
    v = [lit_1 .. lit_n2],T,v_d2
    i > 0 /\ j > 0
    --------------------------------------------  :: E_Matrix
    E C<Matrix(v_1, v_2, v_3)> --> E C<v>

    Error if:
      - v_2 does not have type T_Int
      - v_3 does not have type T_Int
      - v_2 does not have 1 element
      - v_3 does not have 1 element
      - n2 % n1 =/= 0
      - i is negative
      - j is negative

A matrix is created from the elements of `v_1`, with `v_2` rows and `v_3`
columns. If the length of `v_1` is less than the product of dimensions, then it
must evenly divide the dimensions; otherwise `v_1` is truncated.

`v_2` and `v_3` must both be integer vectors of length 1.

_Note:_ In R, `v_2` and `v_3` may have length greater than 1; in that case,
elements after the first one are ignored. Furthermore, if `v_2` has multiple
elements and `v_3` is omitted, then `v_3` is taken to be the second element of
`v_2`.


    v_1 = [lit_1 .. lit_n],T,v_d
    ----------------------------  :: E_Dim
    E C<Dim(v_1)> --> E C<v_d>

Returns the dimension vector.


    v_1 = [lit_1 .. lit_n],T_Int,v_d
    v = negate(v_1)
    --------------------------------  :: E_Negate
    E C<-v_1> --> E C<v>

    Error if:
      - v_1 does not have type T_Int

Negates every element of the vector.


    v = [lit_1 .. lit_n],T,v_d
    --------------------------  :: E_Subset1_Nothing
    E C<v[]> --> E C<v>

Does nothing; returns the original vector.


    v_1 = Vnull
    --------------------------  :: E_Subset1_Null
    E C<v_1[v_2]> --> E C<v_1>

    v_1 = Vnull
    ----------------------------  :: E_Subset2_Null
    E C<v_1[[v_2]]> --> E C<v_1>

Indexing the null vector `Vnull` always returns `Vnull`. No error checking is performed.


    v_1 = [lit_1 .. lit_n1],T,v_d1
    v_2 = [bool_1 .. bool_n2],T_Bool,v_d2
    l = max(n1, n2)
    v_1' = strip_dim(v_1)
    v_1'' = extend(v_1', l-n1)
    v_2' = strip_dim(v_2)
    v_2'' = recycle(v_2', v_2', v_2', l-n2)
    v_2''' = bool_to_pos_vec(v_2'', 1)
    v = get_at_pos(v_1'', v_2''')
    T =/= T_Null
    ---------------------------------------  :: E_Subset1_Bool
    E C<v_1[v_2]> --> E C<v>

If the index vector contains `T`, then the element at the corresponding
location is selected; if it contains `F` then the corresponding element is
skipped; if it contains `NA_b`, then `NA` (of the appropriate type) is selected.
The dimensions of both vectors are ignored.

If the boolean vector is too long, we extend the base vector with `NA`s. If the
boolean vector is too short, we recycle it.


    v_1 = [lit_1 .. lit_n1],T,v_d1
    v_2 = [int_1 .. int_n2],T_Int,v_d2
    v_1' = strip_dim(v_1)
    v_2' = strip_dim(v_2)
    forall i in 1..n2 : int_i >= 0 \/ int_i == NA_i
    v = get_at_pos(v_1', v_2')
    T =/= T_Null
    -----------------------------------------------  :: E_Subset1_Positive
    E C<v_1[v_2]> --> E C<v>

    v_1 = [lit_1 .. lit_n1],T,v_d1
    v_2 = [int_1 .. int_n2],T_Int,v_d2
    v_1' = strip_dim(v_1)
    v_2' = strip_dim(v_2)
    forall i in 1..n2 : int_i <= 0 /\ int_i =/= NA_i
    v_1'' = gen_bool_vec(v_1')
    v_2'' = neg_to_bool_vec(v_2', v_1'')
    v_2''' = bool_to_pos_vec(v_2'', 1)
    v = get_at_pos(v_1', v_2''')
    T =/= T_Null
    ------------------------------------------------  :: E_Subset1_Negative
    E C<v_1[v_2] --> E C<v>

    Error if:
      - v_2 mixes positive and negative subscripts
      - v_2 mixes negative and NA subscripts

Positive subsetting returns elements at the positions specified by the index
vector. Indices that are out of bounds are denoted by `NA_i` select `NA` (of the
appropriate type). Indices that are `0` do not select anything. (If the index
vector contains only `0`s, then subsetting returns the empty vector.) The
dimensions of both vectors are ignored.

Negative subsetting returns elements excluded by the index vector. Indices that
are out of bounds or repeated are ignored. `NA`s are not allowed as indices.


    v_1 = [lit_1 ... lit_n1],T,v_d1
    v_2 = [i],T_Int,v_d2
    product(v_d2) = 1
    i in 1...n1 /\ T =/= T_Null
    ----------------------------------------  :: E_Subset2
    E C<v_1[[v_2]]> --> E C<[lit_i],T,Vnull>

    Error if:
      - v_2 has 0 elements
      - v_2 has more than 1 element
      - v_2 does not have type T_Int
      - v_2 does not have null dimensions
      - i == NA_i
      - i == 0
      - i < 0
      - i > n1
      - product of v_d2 is not 1

Subsetting with `[[` returns a single-element vector. The index vector must
contain a single, non-`NA` element that is within bounds.

The index vector must either have null dimensions, or non-null dimensions whose
product is 1.


    E' = E{ x := v }
    -----------------------  :: E_Assign
    E C<x <- v> --> E' C<v>

Assignment updates the environment and returns the value being assigned.


    x in E
    E(x) = v_1
    v_1 = [lit_1 .. lit_n1],T_1,v_d1
    v = [lit_1 .. lit_n],T,Vnull
    E' = E{ x := v }
    ----------------------------------  :: E_Dim_Assign_Null
    E C<Dim(x) <- Vnull> --> E' C<Vnull>

    Error if:
      - x not in E

Assigning null dimensions to a vector removes its dimensions.


    x in E
    E(x) = v_1
    v_1 = [lit_1 .. lit_n1],T_1,v_d1
    v_2 = [lit'_1 .. lit'_n2],T_Int,v_d2
    forall i in 1 .. n2 : lit'_i > 0
    v = [lit_1 .. lit_n1],T,v_2
    E' = E{ x := v }
    length(v_1) = product(v_2)
    length(v_2) in 1...2
    ----------------------------------  :: E_Dim_Assign
    E C<Dim(x) <- v_2> --> E' C<v_2>

    Error if:
      - x not in E
      - v_2 does not have type T_Int or T_Null
      - length of v_1 is not equal to the product of v_2
      - length of v_2 < 1
      - length of v_2 > 2
      - dimensions vector contains negative values

The entire vector is replaced by a new one, with updated dimensions. The
length of the vector must be equal to the product of the supplied dimension
vector. The dimension vector must be a non-null integer vector.


    x in E
    E(x) = v_1
    v_1 = [lit_1 .. lit_n1],T,Vnull
    v_2 = [lit'_1 ... lit'_n2],T,Vnull
    n1 % n2 == 0
    v = recycle(v_2, v_2, v_2, n1-n2)
    E' = E{ x := v }
    T =/= T_Null
    ----------------------------------  :: E_Subset1_Nothing_Assign
    E C<x[] <- v_2> --> E' C<v_2>

    Error if:
      - x not in E
      - n2 == 0
      - n1 % n2 =/= 0
      - v_1 and v_2 have different types
      - T == T_Null

The entire vector is replaced by a new one, which is recycled if necessary. The
replacement vector is returned. Subset assignment to the null vector is not
allowed, as there is no coercion here.

_Note:_ In R, `n2` does not need to be a multiple of `n1`; however, a warning
is issued. Additionally, the vectors may have different types, as coercion is
performed.

**TODO:** Handle non-null dimensions.


    x in E
    E(x) = v_1
    v_1 = [lit_1 .. lit_n1],T,Vnull
    v_2 = [bool_1 .. bool_n2],T_Bool,Vnull
    v_3 = [lit'_1 ... lit'_n3],T,Vnull
    forall i in 1..n2 : bool_i =/= NA_b
    l = max(n1, n2)
    v_1' = extend(v_1, l-n1)
    v_2' = recycle(v_2, v_2, v_2, l-n2)
    v_2'' = bool_to_pos_vec(v_2', 1)
    n2' = length(v_2'')
    n2' % n3 == 0
    v_3' = recycle(v_3, v_3, v_3, n2'-n3)
    v = update_at_pos(v_1, v_2'', v_3')
    E' = E{ x := v }
    T =/= T_Null
    --------------------------------------  :: E_Subset1_Bool_Assign
    E C<x[v_2] <- v_3> --> E' C<v_3>

    Error if:
      - x not in E
      - v_2 contains NAs
      - n3 == 0
      - n2' % n3 =/= 0
      - v_1 and v_3 have different types
      - T == T_Null

This follows similar rules to `E_Subset1_Bool`, where elements corresponding to
`T` are replaced. The base vector may be extended, the index vector may be
recycled, and the replacement vector may be recycled. Subset assignment to the
null vector is not allowed, as there is no coercion here.

_Note:_ In R, `n3` does not need to be a multiple of `n_2'` (the length of `v_2`
after recycling and conversion to a positional vector); however, a warning is
issued. `v_1` and `v_3` may have different types because of coercion. Finally,
`v_2` may contain `NA`s, but only if `v_3` has length one.

**TODO:** Handle non-null dimensions.


    x in E
    E(x) = v_1
    v_1 = [lit_1 .. lit_n1],T,Vnull
    v_2 = [int_1 .. int_n2],T_Int,Vnull
    v_3 = [lit'_1 .. lit'_n3],T,Vnull
    forall i in 1..n2 : int_i == 0
    T =/= T_Null
    -----------------------------------  :: E_Subset1_Zero_Assign
    E C<x[v_2] <- v_3> --> E' C<v_3>

    Error if:
      - x not in E
      - T == T_Null

This is a special case of `Subset1_Assign` where all elements of `v_2` are `0`:
nothing is updated and the value of the replacement vector is returned. Subset
assignment to the null vector is not allowed, as there is no coercion here.

**TODO:** Handle non-null dimensions.


    x in E
    E(x) = v_1
    v_1 = [lit_1 .. lit_n1],T,Vnull
    v_2 = [int_1 .. int_n2],T_Int,Vnull
    v_3 = [lit'_1 ... lit'_n3],T,Vnull
    forall i in 1..n2 : int_i >= 0
    v_2' = drop_zeros(v_2)
    n2' = length(v_2')
    n2' % n3 == 0
    v_3' = recycle(v_3, v_3, v_3, n2'-n3)
    v = update_at_pos(v_1, v_2', v_3')
    E' = E{ x := v }
    T =/= T_Null
    -------------------------------------  :: E_Subset1_Positive_Assign
    E C<x[v_2] <- v_3> --> E' C<v_3>

    x in E
    E(x) = v_1
    v_1 = [lit_1 .. lit_n1],T,Vnull
    v_2 = [int_1 .. int_n2],T_Int,Vnull
    v_3 = [lit'_1 ... lit'_n3],T,Vnull
    forall i in 1..n2 : int_i <= 0
    v_1' = gen_bool_vec(v_1)
    v_2' = neg_to_bool_vec(v_2, v_1')
    v_2'' = bool_to_pos_vec(v_2', 1)
    n2' = length(v_2'')
    n2' % n3 == 0
    v_3' = recycle(v_3, v_3, v_3, n2'-n3)
    v = update_at_pos(v_1, v_2'', v_3')
    E' = E{ x := v }
    T =/= T_Null
    -------------------------------------  :: E_Subset1_Negative_Assign
    E C<x[v_2] <- v_3> --> E' C<v_3>

    Error if:
      - x not in E
      - v_2 contains NAs
      - n3 == 0
      - n2' % n3 =/= 0
      - v_1 and v_3 have different types
      - v_2 mixes positive and negative subscripts
      - T == T_Null

These are similar to `E_Subset1_Positive` and `E_Subset1_Negative` where `v_2`
specifies which elements to replace. The replacement vector may be recycled.

If the index vector has duplicate values, then the corresponding vector element
will be overwritten, e.g. `v[c(1, 1)] <- c(10, 11)` replaces the first element
with `11`.

Subset assignment to the null vector is not allowed, as there is no coercion
here.

_Note:_ In R, `n3` does not need to be a multiple of `n_2'` (the length of `v_2`
after dropping `0`s or conversion to a positional vector); however, a warning is
issued. `v_1` and `v_3` may have different types because of coercion. Finally,
`v_2` may contain `NA`s, but only if `v_3` has length one.

**TODO:** Handle non-null dimensions.


    x in E
    E(x) = v_1
    v_1 = [lit_1 .. lit_n1],T,Vnull
    v_2 = [i],T_Int,Vnull
    v_3 = [lit],T,Vnull
    l = max(n1, i)
    extend(v_1, l-n1) = v_1'
    v_1' = [lit_1 .. lit_j lit_i lit_k .. lit_l],T,Vnull
    v = [lit_1 .. lit_j lit lit_k .. lit_l],T,Vnull
    E' = E{ x := v }
    T =/= T_Null
    ----------------------------------------------------  :: E_Subset2_Assign
    E C<x[[v_2]] <- v_3> --> E' C<v_3>

    Error if:
      - x not in E
      - v_2 has 0 elements
      - v_2 has more than 1 element
      - v_2 does not have type T_Int
      - v_3 has 0 elements
      - v_3 has more than 1 element
      - v_1 and v_3 have different types
      - i == NA_i
      - i == 0
      - i < 0
      - T == T_Null

Assignment with `[[` only updates a single element of the vector, i.e. the index
vector must contain a single, non-NA element. If the index is out of bounds,
then the base vector is extended with `NA`s.

Subset assignment to the null vector is not allowed, as there is no coercion
here.

**TODO:** Handle non-null dimensions.


### Auxiliary Functions

    ---------------------  :: Aux_Typeof_Null
    typeof(NULL) = T_Null


    ---------------------  :: Aux_Typeof_Bool
    typeof(bool) = T_Bool


    -------------------  :: Aux_Typeof_Int
    typeof(int) = T_Int


    -----------------  :: Aux_NA_Bool
    NA(T_Bool) = NA_b


    ----------------  :: Aux_NA_Int
    NA(T_Int) = NA_i


    typeof(lit) = T
    v = [lit_1 .. lit_n],T,Vnull
    ----------------------------------------------  :: Aux_Prepend
    prepend(lit, v) = [lit lit_1 .. lit_n],T,Vnull


    typeof(lit) = T
    v = [lit_1 .. lit_n],T,Vnull
    ---------------------------------------------  :: Aux_Append
    append(v, lit) = [lit_1 .. lit_n lit],T,Vnull


    v = [lit_1 .. lit_n],T,Vnull
    ----------------------------  :: Aux_Length
    length(v) = n


    v = [],T,Vnull
    --------------  :: Aux_Product
    product(v) = 1


    v = [int_1 int_2 .. int_m],T_Int,Vnull
    v' = [int_2 .. int_m],T_Int,Vnull
    n = int_1 * product(v')
    --------------------------------------  :: Aux_Product
    product(v) = n


    v_1 = [lit_1 .. lit_n],T,v_d
    v = [lit_1 .. lit_n]T,Vnull
    ----------------------------  :: Aux_Strip_Dim
    strip_dim(v_1) = v


    v_1 = [],T_Int,v_d
    ------------------  :: Aux_Negate_BaseCase
    negate(v_1) = v_1


    v_1 = [NA_i int_1 .. int_n],T_Int,v_d
    v_1' = [int_1 .. int_n],T_Int,v_d
    v_2 = negate(v_1')
    v = prepend(NA_i, v_2)
    -------------------------------------  :: Aux_Negate_NACase
    negate(v_1) = v_1


    v_1 = [int int_1 .. int_n],T_Int,v_d
    v_1' = [int_1 .. int_n],T_Int,v_d
    v_2 = negate(v_1')
    v = prepend(-int, v_2)
    ------------------------------------  :: Aux_Negate_RecurseCase
    negate(v_1) = v_1


    v_1 = [lit_1 .. lit_n],T,Vnull
    v_2 = [],T_Int,Vnull
    ------------------------------  :: Aux_GetAtPos_BaseCase
    get_at_pos(v_1, v_2) = [],T


    v_1 = [lit_1 .. lit_n],T,Vnull
    v_2 = [0 int_1 .. int_m],T_Int,Vnull
    v_2' = [int_1 .. int_m],T_Int,Vnull
    v = get_at_pos(v_1, v_2')
    ------------------------------------  :: Aux_GetAtPos_ZeroCase
    get_at_pos(v_1, v_2) = v


    v_1 = [lit_1 .. lit_n],T,Vnull
    v_2 = [i int_1 .. int_m],T_Int,Vnull
    i in 1..n
    v_2' = [int_1 .. int_m],T_Int,Vnull
    v_3 = get_at_pos(v_1, v_2')
    v = prepend(lit_i, v_3)
    ------------------------------------  :: Aux_GetAtPos_InBoundsCase
    get_at_pos(v_1, v_2) = v


    v_1 = [lit_1 .. lit_n],T,Vnull
    v_2 = [i int_1 .. int_m],T_Int,Vnull
    i not in 1..n \/ i = NA_i
    v_2' = [int_1 .. int_m],T_Int,Vnull
    v_3 = get_at_pos(v_1, v_2')
    v = prepend(NA(T), v_3)
    ------------------------------------  :: Aux_GetAtPos_OutBoundsCase
    get_at_pos(v_1, v_2) = v


    v_1 = [],T_Bool,Vnull
    ----------------------------------------  :: Aux_BoolToPosVec_BaseCase
    bool_to_pos_vec(v_1, i) = [],T_Int,Vnull


    v_1 = [T bool_1 .. bool_n],T_Bool,Vnull
    v_1' = [bool_1 .. bool_n],T_Bool,Vnull
    v_2 = bool_to_pos_vec(v_1', i+1)
    v = prepend(i, v_2)
    ---------------------------------------   :: Aux_BoolToPosVec_TCase
    bool_to_pos_vec(v_1, i) = v


    v_1 = [F bool_1 .. bool_n],T_Bool,Vnull
    v_1' = [bool_1 .. bool_n],T_Bool,Vnull
    v = bool_to_pos_vec(v_1', i+1)
    ---------------------------------------  :: Aux_BoolToPosVec_FCase
    bool_to_pos_vec(v_1, i) = v


    v_1 = [NA_b bool_1 .. bool_n],T_Bool,Vnull
    v_1' = [bool_1 .. bool_n],T_Bool,Vnull
    v_2 = bool_to_pos_vec(v_1', i+1)
    v = prepend(NA_i, v_2)
    ------------------------------------------   :: Aux_BoolToPosVec_NACase
    bool_to_pos_vec(v_1, i) = v


    ------------------------------  :: Aux_Truncate_BaseCase1
    truncate(v_1, 0) = v_1


    v_1 = [],T,Vnull
    ------------------------------  :: Aux_Truncate_BaseCase2
    truncate(v_1, m) = v_1


    v_1 = [lit_1 .. lit_i lit_j],T,Vnull
    v_1' = [lit_1 .. lit_i],T,Vnull
    v = truncate(v_1', m-1)
    m > 0
    ------------------------------------  :: Aux_Truncate_RecurseCase
    truncate(v_1, m) = v_1


    --------------------  :: Aux_Extend_BaseCase
    extend(v_1, 0) = v_1


    v_1 = [lit_1 .. lit_n],T,Vnull
    v_1' = append(v_1, NA(T))
    v = extend(v_1', m-1)
    m > 0
    ------------------------------  :: Aux_Extend_RecurseCase
    extend(v_1, m) = v


    m <= 0
    -------------------------------  :: Aux_Recycle_BaseCase
    recycle(v_1, v_2, v_3, m) = v_1


    v_1 = [lit_i .. lit_j],T,Vnull
    v_2 = [],T,Vnull
    v = recycle(v_1, v_3, v_3, m)
    m > 0
    ------------------------------  :: Aux_Recycle_CycleCase
    recycle(v_1, v_2, v_3, m) = v


    v_1 = [lit_i .. lit_j],T,Vnull
    v_2 = [lit lit_1 .. lit_n],T,Vnull
    v_1' = append(v_1, lit)
    v_2' = [lit_1 .. lit_n],T,Vnull
    v = recycle(v_1', v_2', v_3, m-1)
    m > 0
    ----------------------------------  :: Aux_Recycle_RecurseCase
    recycle(v_1, v_2, v_3, m) = v


    v_1 = [],T,Vnull
    ---------------------------  :: Aux_GenBoolVec_BaseCase
    gen_bool_vec(v_1) = [],T_Bool


    v_1 = [lit lit_1 .. lit_n],T,Vnull
    v_1' = [lit_1 .. lit_n],T,Vnull
    v_2 = gen_bool_vec(v_1')
    v = prepend(T, v_2)
    ----------------------------------  :: Aux_GenBoolVec_RecurseCase
    gen_bool_vec(v_1) = v


    v_1 = [],T_Int,Vnull
    v_2 = [bool_1 .. bool_n],T_Bool,Vnull
    -------------------------------------  :: Aux_NegToBoolVec_BaseCase
    neg_to_bool_vec(v_1, v_2) = v_2


    v_1 = [-j int_1 .. int_n],T_Int,Vnull
    v_1' = [int_1 .. int_n],T_Int,Vnull
    v_2 = [bool_1 .. bool_i bool_j bool_k .. bool_m],T_Bool,Vnull
    v_2' = [bool_1 .. bool_i F bool_k .. bool_m],T_Bool,Vnull
    v = neg_to_bool_vec(v_1', v_2')
    -------------------------------------------------------------  :: Aux_NegToBoolVec_InBoundsCase
    neg_to_bool_vec(v_1, v_2) = v


    v_1 = [-j int_1 .. int_n],T_Int,Vnull
    v_1' = [int_1 .. int_n],T_Int,Vnull
    v_2 = [bool_1 .. bool_m],T_Bool,Vnull
    j not in 1..m
    v = neg_to_bool_vec(v_1', v_2)
    -------------------------------------  :: Aux_NegToBoolVec_OutBoundsCase
    neg_to_bool_vec(v_1, v_2) = v


    v_1 = [],T_Int,Vnull
    --------------------------  :: Aux_DropZeros_BaseCase
    drop_zeros(v_1) = [],T_Int


    v_1 = [0 int_1 .. int_n],T_Int,Vnull
    v_1' = [int_1 .. int_n],T_Int,Vnull
    v = drop_zeros(v_1')
    ------------------------------------  :: Aux_DropZeros_ZeroCase
    drop_zeros(v_1) = v


    v_1 = [int int_1 .. int_n],T_Int,Vnull
    v_1' = [int_1 .. int_n],T_Int,Vnull
    v_1'' = drop_zeros(v_1')
    v = prepend(int, v_1'')
    int =/= 0
    --------------------------------------  :: Aux_DropZeros_NonZeroCase
    drop_zeros(v_1) = v


    v_1 = [lit_1 .. lit_n],T,Vnull
    v_2 = [],T_Int,Vnull
    v_3 = [],T,Vnull
    ----------------------------------  :: Aux_UpdateAtPos_BaseCase
    update_at_pos(v_1, v_2, v_3) = v_1


    v_1 = [lit_1 .. lit_i lit_j lit_k .. lit_n],T,Vnull
    v_2 = [j int_1 .. int_m],T_Int,Vnull
    v_3 = [lit' lit'_1 .. lit'_m],T,Vnull
    v_1' = [lit_1 .. lit_i lit' lit_k .. lit_n],T,Vnull
    v_2' = [int_1 .. int_m],T_Int,Vnull
    v_3' = [lit'_1 .. lit'_m],T,Vnull
    v = update_at_pos(v_1', v_2', v_3')
    ---------------------------------------------------  :: Aux_UpdateAtPos_InBoundsCase
    update_at_pos(v_1, v_2, v_3) = v


    v_1 = [lit_1 .. lit_n],T,Vnull
    v_2 = [j int_1 .. int_m],T_Int,Vnull
    v_3 = [lit'_1 .. lit'_m],T,Vnull
    j not in 1..m /\ j =/= NA(T)
    v_1' = extend(v_1, j-m)
    v = update_at_pos(v_1', v_2, v_3)
    ------------------------------------  :: Aux_UpdateAtPos_OutBoundsCase
    update_at_pos(v_1, v_2, v_3) = v


## TODO

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
    * matrices
    * arrays
  * lists
    * or treat them as "vectors" of some vector type
  * `$` operator
    * `x$y` is sugar for `x[["y"]]`, implies symbol-to-string coercion
  * promises and laziness

