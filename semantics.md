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
        | e_1[ne_2]                                 # subset1 vector
        | e_1[[e_2]]                                # subset2 vector
        | e_1[ne_2,ne_3]                            # subset1 matrix
        | e_1[[e_2,e_3]]                            # subset2 matrix
        | e_1; ... ; e_2                            # sequencing
        | x <- e                                    # variable assignment
        | Dim(x) <- e                               # dimension setter
        | x[] <- e_1                                # subset1 (nothing) assignment
        | x[e_1] <- e_2                             # subset1 assignment
        | x[[e_1]] <- e_2                           # subset2 assignment
        | x[e_1,e_2] <- e_3                         # subset1 matrix assignment
        | x[[e_1,e_2]] <- e_3                       # subset2 matrix assignment
        | v                                         # value (vector)

    ne ::=                                          # optional expression
         | e                                        # expression
         | ε                                        # empty

    nv ::=                                          # optional value
         | v                                        # value
         | ε                                        # empty

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
  * `ε` denotes an empty expression or value, i.e. syntactically nothing should
    appear


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
        | C[ne]                                     # subset1 vector
        | v[C]
        | C[[e]]                                    # subset2 vector
        | v[[C]]
        | C[ne,ne]                                  # subset1 matrix
        | v[C,ne]
        | v[nv,C]
        | C[[e,e]]                                  # subset2 matrix
        | v[[C,e]]
        | v[[v,C]]
        | v_1; .. ; v_n; C; e_1; .. ; e_m           # sequencing
        | x <- C                                    # variable assignment
        | Dim(x) <- C                               # dimension setter
        | x[] <- C                                  # subset1 (nothing) assignment
        | x[C] <- e                                 # subset1 assignment
        | x[v] <- C
        | x[[C]] <- e                               # subset2 assignment
        | x[[v]] <- C
        | x[C,e] <- e                               # subset1 matrix assignment
        | x[v,C] <- e
        | x[v,v] <- C
        | x[[C,e]] <- e                             # subset2 matrix assignment
        | x[[v,C]] <- e
        | x[[v,v]] <- C

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

    (v_1 = Vnull ... v_n = Vnull)
    ------------------------------------------  :: E_Combine_Null
    E C<Combine(v_1, ..., v_n)> --> E C<Vnull>

    (v_1 = [lit_1_1 .. lit_1_m1],T,v_d1) ... (v_n = [lit_n_1 .. lit_n_mn],T,v_d2)
    T =/= T_Null
    -----------------------------------------------------------------------------------------  :: E_Combine
    E C<Combine(v_1, ..., v_n)> --> E C<[lit_1_1 .. lit_1_m1 .. lit_n_1 .. lit_n_mn],T,Vnull>

    Error if:
      - vectors have different types

`Combine` takes vectors as arguments, and combines/flattens them into a single
vector. The dimensions vectors are ignored, so the resulting vector as null
dimensions.

Passing zero arguments to `Combine` will return the null vector, `Vnull`.
Combining multiple null vectors will return the null vector.

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
appropriate type, and recycled to fill the required dimensions.


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


    v_1 = Vnull
    ---------------------------  :: E_Subset1_Null_Vector
    E C<v_1[nv_2]> --> E C<v_1>

    v_1 = Vnull
    ----------------------------  :: E_Subset2_Null_Vector
    E C<v_1[[v_2]]> --> E C<v_1>

    v_1 = Vnull
    --------------------------------  :: E_Subset1_Null_Matrix
    E C<v_1[nv_2,nv_3]> --> E C<v_1>

    v_1 = Vnull
    --------------------------------  :: E_Subset2_Null_Matrix
    E C<v_1[[v_2,v_3]]> --> E C<v_1>

Indexing the null vector `Vnull` always returns `Vnull`. No error checking is
performed.


    v_1 = [lit_1 .. lit_n],T,v_d
    v_1' = strip_dim(v_1)
    nv_2' = strip_dim(nv_2)
    v_2'' = make_subscript(nv_2', n)
    v = get_at_pos(v_1', v_2'')
    T =/= T_Null
    --------------------------------  :: E_Subset1_Vector
    E C<v_1[nv_2]> --> E C<v>

    Error if:
      - nv_2 mixes positive and negative subscripts
      - nv_2 mixes negative and NA subscripts

If `nv_2` is missing, then subsetting returns the original vector.

If `nv_2` is null, then subsetting is equivalent to subsetting with a 0 index,
i.e., the empty vector is returned.

If `nv_2` is a vector of positive integers, then elements at the positions
specified by `nv_2` are selected. Indices that are `0` are dropped (and if the
index vector contains only `0`s, then subsetting returns the empty vector).
Indices that are `NA_i` or out of bounds select `NA` (of the appropriate type).

If `nv_2` is a vector of negative integers, then elements excluded by those
indices are returned. Indices that are out of bounds or repeated are ignored.

If `nv_2` is a boolean vector, then the positions where `nv_2` is `T` are
selected, positions that are `F` are dropped, and positions that are `NA_b`
select `NA` (of the appropriate type). If `nv_2` is too short, it is recycled.


    v_1 = [lit_1 .. lit_n1],T,v_d1
    v_d1 = [r c],T_Int,v_d
    v_1' = strip_dim(v_1)
    nv_2' = strip_dim(nv_2)
    nv_3' = strip_dim(nv_3)
    v_2'' = make_matrix_subscript(nv_2', r)
          = [int_1 .. int_n2],T_Int,v_d2
    v_3'' = make_matrix_subscript(nv_3', c)
          = [int'_1 .. int'_n3],T_Int,v_d3
    forall i in 1..n2 : 1 <= int_i  <= r \/ int_i == NA_i
    forall i in 1..n3 : 1 <= int'_i <= c \/ int_i == NA_i
    v_4 = vectors_to_pos_vec(v_2'', v_3'', v_2'')
    v_1'' = get_at_pos(v_1', v_4)
          = [lit'_1 .. lit'_n],T,v_d1'
    v_d' = [n2 n3],T_Int,Vnull
    v = [lit'_1 .. lit'_n],T,v_d'
    T =/= T_Null
    -----------------------------------------------------  E_Subset1_Matrix
    E C<v_1[nv_2,nv_3]> --> E C<v>

    Error if:
      - nv_2 or nv_3 mix positive and negative indices
      - nv_2 or nv_3 mix negative and NA indices
      - nv_2 or nv_3 have positive out-of-bounds indices
      - nv_2 is a boolean vector and longer than r
      - nv_3 is a boolean vector and longer than c

`nv_2` is an index vector that selects rows. `nv_3` is an index vector that
selects columns. The dimensions of `nv_2` and `nv_3` are ignored.

A missing index vector selects everything.

A null index vector is equivalent to an index of `0`, i.e. it selects nothing.

A vector of positive integers selects the specified rows (for `nv_2`) or columns
(for `nv_3`). Indices that are `0` are dropped (and if the index vector contains
only `0`s then nothing is selected). Indices that are `NA_i` select `NA` (of the
appropriate type). Indices must be within bounds of the matrix.

A vector of negative integers excludes the specified rows (for `nv_2`) or
columns (for `nv_3`). Indices that are out of bounds or repeated are ignored.

A vector of booleans selects the rows (for `nv_2`) or columns (for `nv_3`) that
are `T` and drops those that are `F`. Indices that are `NA_b` select `NA` (of
the appropriate type). If the index vector is too short, it is recycled. It is
an error if the index vector is too long.


    v_1 = [lit_1 .. lit_n1],T,v_d1
    v_d1 = [r c],T_Int,v_d
    v_2 = [int_1 .. int_n2],T_Int,v_d2
    v_d2 = [r' 2],T_Int,v_d'
    forall i in    1..r' : 0 <= int_i <= r \/ int_i == NA_i
    forall i in r'+1..n2 : 0 <= int_i <= c \/ int_i == NA_i
    v_1' = strip_dim(v_1)
    v_2' = strip_dim(v_2)
    v_2'' = matrix_to_pos_vec(v_2', r, r', 1)
    v = get_at_pos(v_1', v_2'')
    T =/= T_Null
    -------------------------------------------------------  :: E_Subset1_Matrix_Matrix
    E C<v_1[v_2]> --> E C<v>


**TODO**:
subset 1/2   extract/assign     vector/matrix
1               assign              vector
1               assign              matrix
2               extract             vector
2               extract             matrix
2               assign              vector
2               assign              matrix

**TODO**: After assignment, figure out where we don't need to strip dims


    v_1 = [lit_1 ... lit_n1],T,v_d1
    v_2 = [i],T_Int,v_d2
    i in 1...n1 /\ T =/= T_Null
    ----------------------------------------  :: E_Subset2_Vector
    E C<v_1[[v_2]]> --> E C<[lit_i],T,Vnull>

    Error if:
      - v_2 is omitted
      - v_2 has 0 elements
      - v_2 has more than 1 element
      - v_2 does not have type T_Int
      - i == NA_i
      - i == 0
      - i < 0
      - i > n1

Subsetting with `[[` returns a single-element vector. The index vector must
contain a single, non-`NA` integer that is within bounds. The dimensions of both
vectors are ignored.


    v_1 = [lit_1 ... lit_n],T,v_d1
    v_d1 = [r c],T_Int,v_d
    v_2 = [i],T_Int,v_d2
    v_3 = [j],T_Int,v_d3
    i in 1...r /\ j in 1...c
    k = i + (j-1)*r
    k in 1...n /\ T =/= T_Null
    --------------------------------------------  :: E_Subset2_Matrix
    E C<v_1[[v_2,v_3]]> --> E C<[lit_k],T,Vnull>

    Error if:
      - v_2 or v_3 are omitted
      - v_2 or v_3 have 0 elements
      - v_2 or v_3 have more than 1 element
      - v_2 or v_3 do not have type T_Int
      - i == NA_i or j == NA_i
      - i == 0 or j == 0
      - i < 0 or j < 0
      - i > r or j > c

Subsetting with `[[` returns a single-element vector. When subsetting a matrix,
two index vectors must be provided, each containing a single, non-`NA` integer
that is within bounds.



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
    v_1' = gen_bool_vec(n1)
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


    ----------------------------  :: Aux_Strip_Dim_Missing
    strip_dim(ε) = ε


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


    v_1 = gen_bool_vec(n)
    v = bool_to_pos_vec(v_1, 1)
    ---------------------------  :: Aux_MakeSubscript_Nothing
    make_subscript(ε, n) = v


    v_1 = Vnull
    v = [],T_Int,Vnull
    --------------------------  :: Aux_MakeSubscript_Null
    make_subscript(v_1, n) = v


    v_1 = [int_1 .. int_n1],T_Int,Vnull
    v_1' = drop_zeros(v_1)
    forall i in 1..n1 : int_i >= 0 \/ int_i = NA_i
    ----------------------------------------------  :: Aux_MakeSubscript_Positive
    make_subscript(v_1, n) = v_1'


    v_1 = [int_1 .. int_n1],T_Int,Vnull
    forall i in 1..n1 : int_i <= 0 /\ int_i =/= NA_i
    v_2 = gen_bool_vec(n)
    v_1' = neg_to_bool_vec(v_1, v_2)
    v_1'' = bool_to_pos_vec(v_1', 1)
    ------------------------------------------------ :: Aux_MakeSubscript_Negative
    make_subscript(v_1, n) = v_1''


    v_1 = [bool_1 .. bool_n1],T_Bool,Vnull
    l = max(n, n1)
    v_1' = recycle(v_1, v_1, v_1, l-n1)
    v_1'' = bool_to_pos_vec(v_1', 1)
    --------------------------------------  :: Aux_MakeSubscript_Bool
    make_subscript(v_1, n) = v_1''


    v_1 = [bool_1 .. bool_n1],T_Bool,Vnull
    n1 <= n
    v_1' = recycle(v_1, v_1, v_1, n-n1)
    v_1'' = bool_to_pos_vec(v_1', 1)
    --------------------------------------  :: Aux_MakeMatrixSubscript_Bool
    make_matrix_subscript(v_1, n) = v_1''


    v = make_subscript(v_1, n)
    --------------------------------------  :: Aux_MakeMatrixSubscript
    make_matrix_subscript(v_1, n) = v


    v_1 = [lit_1 .. lit_n],T,Vnull
    v_2 = [],T_Int,Vnull
    ---------------------------------  :: Aux_GetAtPos_BaseCase
    get_at_pos(v_1, v_2) = [],T,Vnull


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
    ------------------------------------------  :: Aux_BoolToPosVec_NACase
    bool_to_pos_vec(v_1, i) = v


    v_1 = [int_1 .. int_n1],T_Int,Vnull
    v_2 = [],T_Int,Vnull
    v = [],T_Int,Vnull
    -------------------------------------  :: Aux_VectorsToPosVec_BaseCase
    vectors_to_pos_vec(v_1, v_2, v_3) = v


    v_1 = [],T_Int,Vnull
    v_2 = [j int'_1 .. int'_n2],T_Int,Vnull
    v_2' = [int'_1 .. int'_n2],T_Int,Vnull
    v = vectors_to_pos_vec(v_3, v_2', v_3)
    ----------------------------------------  :: Aux_VectorsToPosVec_NextColCase
    vectors_to_pos_vec(v_1, v_2, v_3) = v


    v_1 = [i int_1 .. int_n1],T_Int,Vnull
    v_2 = [j int'_1 .. int'_n2],T_Int,Vnull
    i = NA_i \/ j = NA_i
    v_1' = [int_1 .. int_n1],T_Int,Vnull
    v_4 = vectors_to_pos_vec(v_1', v_2, v_3)
    v = prepend(NA_i, v_4)
    ----------------------------------------  :: Aux_VectorsToPosVec_NACase
    vectors_to_pos_vec(v_1, v_2, v_3) = v


    v_1 = [i int_1 .. int_n1],T_Int,Vnull
    v_2 = [j int'_1 .. int'_n2],T_Int,Vnull
    i =/= NA_i /\ j =/= NA_i
    v_1' = [int_1 .. int_n2],T_Int,Vnull
    r = length(v_3)
    k = i + (j-1)*r
    v_4 = vectors_to_pos_vec(v_1', v_2, v_3)
    v = prepend(k, v_4)
    ----------------------------------------  :: Aux_VectorsToPosVec_InBoundsCase
    vectors_to_pos_vec(v_1, v_2, v_3) = v


    ----------------------------------------------------  :: Aux_MatrixToPosVec_BaseCase
    matrix_to_pos_vec(v_1, r, r', r'+1) = [],T_Int,Vnull


    v_1 = [int_1 .. int_n],T_Int,Vnull
    int_k = NA_i \/ int_(k+r') = NA_i
    v_1' = matrix_to_pos_vec(v_1, r, r', k+1)
    v = prepend(NA_i, v_1')
    -----------------------------------------  :: Aux_MatrixToPosVec_NACase
    matrix_to_pos_vec(v_1, r, r', k) = v


    v_1 = [int_1 .. int_n],T_Int,Vnull
    i = int_k
    j = int_(k+r')
    l = i + (j-1)*r
    v_1' = matrix_to_pos_vec(v_1, r, r', k+1)
    v = prepend(l, v_1')
    -----------------------------------------  :: Aux_MatrixToPosVec_RecurseCase
    matrix_to_pos_vec(v_1, r, r', k) = v


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


    ---------------------------------  :: Aux_GenBoolVec_BaseCase
    gen_bool_vec(0) = [],T_Bool,Vnull


    v_1 = gen_bool_vec(n-1)
    v = prepend(T, v_1)
    n > 0
    ----------------------------------  :: Aux_GenBoolVec_RecurseCase
    gen_bool_vec(n) = v


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


    v = [],T_Int,Vnull
    ------------------  :: Aux_DropZeros_BaseCase
    drop_zeros(v) = v


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

