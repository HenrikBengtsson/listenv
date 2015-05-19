Copyright Henrik Bengtsson, 2015

## List environments
_List environments_ are environments that behaves as lists by
overriding the subsetting functions for environments such that they
also emulates some of the index subsetting that lists have.  For example,
```r
x <- listenv()
for (i in 1:3) {
  x[[i]] <- i^2
}
names(x) <- c("a", "b", "c")
```
The values of a list environment can be retrieved individually via
`x$b` and `x[["b"]]` just as with regular environments, but also via
`x[[2]]` as with regular lists.
To retrieve all values of an environment as a list, use `as.list(x)`.

### Examples
Here is a longer set of examples illustrating what the list environments provides:
```r
> x <- listenv()
> x[[1]] <- { 1 }
> x[[3]] <- { "Hello world!" }
> length(x)
3
> seq_along(x)
[1] 1 2 3
> names(x) <- c("a", "b", "c")
> x$b <- TRUE
> x[[1]]
1
> as.list(x)
$a
[1] 1

$b
[1] TRUE

$c
[1] "Hello world!"
```

It is possible to also specify the length upfront, e.g.
```r
> x <- listenv(length=4)
> seq_along(x)
[1] 1 2 3 4
```
