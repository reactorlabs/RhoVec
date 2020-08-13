# Examples

## Subset assignment with boolean vector

```
# LHS one element, gets recycled
> v <- 1:5
> v[T] <- 0    # equiv to v[c(T, T, T, T, T)] and v[c(1, 2, 3, 4, 5)]
> v
[1] 0 0 0 0 0

> v <- 1:5
> v[T]
[1] 1 2 3 4 5
> v[F]          # not the same as empty subsetting!
numeric(0)

# LHS too short, gets recycled
> v <- 1:5
> v[c(T, F, T)]  # equiv to v[c(T, F, T, T, F)] and v[c(1, 3, 4)]
[1] 1 3 4
> v[c(T, F, T)] <- 0
> v
[1] 0 2 0 0 5

# LHS too short, with NAs, gets recycled
> v <- 1:5
> v[c(T, F, NA)]  # equiv to v[c(T, F, NA, T, F)] and v[c(1, NA, 4)]
[1]  1  NA  4
> v[c(T, F, NA)] <- 0
> v
[1] 0 2 3 0 5

# out of bounds, extends vector
> v <- 1:5
> v[c(T, T, T, T, T, T)]  # equiv to v[c(1, 2, 3, 4, 5, 6)]
[1]  1  2  3  4  5 NA
> v[c(T, T, T, T, T, T)] <- 0
> v
[1] 0 0 0 0 0 0

# LHS gets recycled
# LHS not multiple of RHS, WARNING
> v <- 1:5
> v[T] <- c(10, 11)
Warning message:
In v[T] <- c(10, 11) :
  number of items to replace is not a multiple of replacement length

# LHS gets recycled
# LHS multiple of RHS, gets recycled
> v <- 1:6
> v[T]
[1] 1 2 3 4 5 6
> v[T] <- c(10, 11)
[1] 10 11 10 11 10 11

# LHS gets recycled
# LHS not multiple of RHS, WARNING
> v <- 1:6
> v[c(T, F)]  # equiv to v[c(T, F, T, F, T, F)] and v[c(1, 3, 5)]
[1] 1 3 5
> v[c(T, F)] <- c(10, 11)
Warning message:
In v[c(T, F)] <- c(10, 11) :
  number of items to replace is not a multiple of replacement length

# LHS gets recycled
# LHS is multiple or RHS, gets recycled
> v <- 1:6
> v[c(T, F)] <- c(10, 11, 12)
> v
[1] 10  2 11  4 12  6

# NAs not allowed if RHS has multiple elements
> v <- 1:5
> v[c(T, NA)] # equiv to v[c(1, NA, 3, NA, 5)]
[1]  1 NA  3 NA  5
> v[c(T, NA)] <- c(10, 11, 12)
Error in v[c(T, NA)] <- c(10, 11, 12) :
  NAs are not allowed in subscripted assignments
> v[c(T, NA)] <- 0
> v
[1] 0 2 0 4 0

# RHS too long, gets truncated
> v <- 1:6
> v[c(T, F, F)] <- c(10, 11, 12)  # equiv to v[c(1, 4)]
Warning message:
In v[c(T, F, F)] <- c(10, 11, 12) :
  number of items to replace is not a multiple of replacement length
> v
[1] 10  2  3 11  5 6

# RHS gets truncated, with a warning
> v <- 1:3
> v
[1] 1 2 3
> v[c(T, T, F)] <- c(9, 8, 7)
Warning message:
In v[c(T, T, F)] <- c(9, 8, 7) :
  number of items to replace is not a multiple of replacement length
> v
[1] 9 8 3

# RHS is right length, everything ok
> v <- 1:3
> v
[1] 1 2 3
> v[c(T, T, F)] <- c(9, 8)
> v
[1] 9 8 3

# ???
# boolean vector extends original vector, boolean vector converted to pos
# vector, then do assignment
# of course, RHS gets recycled
> v <- 1:3
> v[c(T, T, T, T, F)] <- c(9, 8)
> v
[1]  9  8  9  8 NA
```

## Subset assignment with negative vector

```
### In all examples, v is [1 2 3 4 5] before the assignment

# recycle RHS
> v[-1] <- 0
> v
[1] 1 0 0 0 0

# recycle RHS
> v[-1] <- c(10, 11)
> v
[1]  1 10 11 10 11

# recycle RHS, with warning
> v[-1] <- c(10, 11, 12)
Warning message:
In v[-1] <- c(10, 11, 12) :
  number of items to replace is not a multiple of replacement length
> v
[1]  1 10 11 12 10

# recycle RHS
> v[-1] <- c(10, 11, 12, 13)
> v
[1]  1 10 11 12 13

# recycle RHS
> v[-c(1, 3)] <- 0
> v
[1] 1 0 3 0 0

# recycle RHS, warning
> v[-c(1, 3)] <- c(10, 11)
Warning message:
In v[-c(1, 3)] <- c(10, 11) :
  number of items to replace is not a multiple of replacement length
> v
[1]  1 10  3 11 10

# recycle RHS
v[-c(1, 3)] <- c(10, 11, 12)
> v
[1]  1 10  3 11 12

# truncate RHS, warning
> v[-c(1, 3)] <- c(10, 11, 12, 13)
Warning message:
In v[-c(1, 3)] <- c(10, 11, 12, 13) :
  number of items to replace is not a multiple of replacement length
> v
[1]  1 10  3 11 12

## out of bounds negative index is dropped before checking length
## equivalent: neg-to-pos conversion drops OOB indices
# recycle RHS
> v[-c(1, 0, 3, 10)] <- 0
> v
[1] 1 0 3 0 0

# recycle RHS, warning
> v[-c(1, 0, 3, 10)] <- c(10, 11)
Warning message:
In v[-c(1, 0, 3, 10)] <- c(10, 11) :
  number of items to replace is not a multiple of replacement length
> v
[1]  1 10  3 11 10

# recycle RHS
v[-c(1, 0, 3, 10)] <- c(10, 11, 12)
> v
[1]  1 10  3 11 12

# truncate RHS, warning
> v[-c(1, 0, 3, 10)] <- c(10, 11, 12, 13)
Warning message:
In v[-c(1, 0, 3, 10)] <- c(10, 11, 12, 13) :
  number of items to replace is not a multiple of replacement length
> v
[1]  1 10  3 11 12

## duplicate indices are ignored
## equivalent: this is handled by neg-to-pos conversion
# recycle RHS
> v[-c(1, 3, 1)] <- 0
> v
[1] 1 0 3 0 0

# recycle RHS, warning
> v[-c(1, 3, 1)] <- c(10, 11)
Warning message:
In v[-c(1, 3, 1)] <- c(10, 11) :
  number of items to replace is not a multiple of replacement length
> v
[1]  1 10  3 11 10

# recycle RHS
v[-c(1, 3, 1)] <- c(10, 11, 12)
> v
[1]  1 10  3 11 12

# truncate RHS, warning
> v[-c(1, 3, 1)] <- c(10, 11, 12, 13)
Warning message:
In v[-c(1, 3, 1)] <- c(10, 11, 12, 13) :
  number of items to replace is not a multiple of replacement length
> v
[1]  1 10  3 11 12

# error
> v[-c(1, NA)] <- 0
Error in v[-c(1, NA)] <- 0 :
  only 0's may be mixed with negative subscripts

# nothing
> v[-NA] <- 0
> v
[1] 1 2 3 4 5
```
