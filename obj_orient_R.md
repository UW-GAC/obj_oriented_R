# Object-Oriented Programming in R
Stephanie Gogarten  
11/1/2017  



## What is Object-Oriented Programming?

The traditional way to think about programming is that a program is a series of instructions to the computer.  If you have a set of instructions you want to execute more than once, then you define a "function" that takes some input, does something with it, and returns some output.  Thus you can think of a program as executing a series of functions.  Each function specifies the type of input it requires, and the type of output it produces.

An example (in python):

```
# define a function
def squared(x):
    return x * x

# define a variable
y = 5

# call the function
z = squared(y)
```

Alternatively, you can think of a program in terms of the data it is operating on, rather than in terms of what it is doing with the data. You can define a data "class" that has specific components, and (this is the key part) specific functions (usually called "methods") that are defined to work with that data class.  As an example, you could define a class called "Number" that contains one variable (or "field" or "slot") and one method.

```
# define a class called "Number"
class Number:
    value = 1

    def squared(self):
        return self.value * self.value
```

This class is an abstract definition; to use it, we create an "object" (often called an "instance" of the class).

```
y = Number()

# set the 'value' field of y
y.value = 5

# call the 'squared' method
z = y.squared()
```

The two examples have the same result, but how we got there is different.  In the first example, we defined a function and passed it an argument.  In the second example, we defined an object, and then operated on that object.  That is object-oriented programming. 


## R vs. Everything Else

### OOP in other languages

In most languages, the fundamental OO unit is a piece of data. Let's say we want to define a Variant class in Python.
```
class Variant:
    def __init__(self, chrom, pos, ref, alt):
        self.chrom = chrom
        self.pos = pos
        self.ref = ref
        self.alt = alt
```

We define methods *in the scope of the Variant class*. Let's say we want to define a "length" method that counts the number of characters in the ref allele. That method definition is part of the class definition:

```
    def length(self):
        return(len(self.ref)
```

```
var = Variant(1, 100, "AGG", "A")
var.length()
```

We might have another class "Sequence" that also has a length:
```
y <- Sequence("ACGT")
y.length()
```

`length` for a Variant and `length` for a Sequence are *completely separate from each other*.


### OOP in R is "backwards"

In R, the fundamental OO unit is a *generic function*.


```r
length
```

```
## function (x)  .Primitive("length")
```

Here, `length` exists outside of any class, and a class can define a method that will be associated with it.


```r
x <- 1:10
class(x)
```

```
## [1] "integer"
```

```r
length(x)
```

```
## [1] 10
```

```r
y <- list(1:10,1:10)
class(y)
```

```
## [1] "list"
```

```r
length(y)
```

```
## [1] 2
```

The class of the first argument tells the `length` generic which method to call: do I need to count the number of elements in a vector, or the number of items in a list?


## S3 vs S4 classes

R has multiple OO systems: S3, S4, reference classes, and the brand-new R6 (another type of reference class). I'm only going to talk about the first two.

### S3

This was the first OO system in R, and is very simple and unstructured. There are no formal class definitions. We can make a class by simply assigning a name to it:


```r
var <- list(chrom=1, pos=100, ref="AGG", alt="A")
class(var) <- "variant"
class(var)
```

```
## [1] "variant"
```

But it is still a list also:


```r
is.list(var)
```

```
## [1] TRUE
```

What happens if we call `length` on this new class?


```r
length(var)
```

```
## [1] 4
```

Since the class was derived from a list, `length` is using the method defined for a list class. We would like to change this. The syntax in S3 is `<generic name>.<class name>`:


```r
length.variant <- function(x) {nchar(x$ref)}
length(var)
```

```
## [1] 3
```

Most CRAN packages (including the tidyverse) use S3.
S3 is very easy, but often it's too easy: with no formal classes, you can do things that make no sense. 


```r
x <- "lovely bunch of coconuts"
class(x) <- "variant"
length(x)
```

```
## Error: $ operator is invalid for atomic vectors
```


### S4

Bioconductor packages rely heavily on S4 classes, which have the same primacy of generic functions, but with formal class definitions. Let's make a Variant class in S4:


```r
setClass("Variant", slots=c(chrom="character", pos="integer", ref="character", alt="character"))
```

Like the Python "init" method, we need a constructor for this class. This is generally a function with the same name as the class, and a call to `new` inside it:


```r
Variant <- function(chrom, pos, ref, alt) {
    new("Variant", chrom=chrom, pos=pos, ref=ref, alt=alt)
}
```

We can define a `length` method for our class. We pass `setMethod` the name of the generic function, the method "signature" (the class for which we are defining the method), and the function definition. 


```r
setMethod("length", "Variant", function(x) {nchar(x@ref)})
```

```
## [1] "length"
```

The `@` symbol access a slot in the class. Best practices in programming dictate that we only use this symbol inside method definitions (so a user of our class should never have to type it). Instead, define accessor methods to return the contents of slots.


```r
setMethod("ref", "Variant", function(x) {return(x@ref)})
```

```
## Error in setMethod("ref", "Variant", function(x) {: no existing definition for function 'ref'
```

What just happened? This is an example of R's backwards OO system - we can't define a method called `ref` for our class, because there is no generic function called `ref`. First we have to define one:


```r
setGeneric("ref", function(x) standardGeneric("ref"))
```

```
## [1] "ref"
```

Now we can try again:


```r
setMethod("ref", "Variant", function(x) {return(x@ref)})
```

```
## [1] "ref"
```

Now let's create a variant:


```r
var <- Variant(1, 100, "AGG", "G")
```

```
## Error in validObject(.Object): invalid class "Variant" object: 1: invalid object for slot "chrom" in class "Variant": got class "numeric", should be or extend class "character"
## invalid class "Variant" object: 2: invalid object for slot "pos" in class "Variant": got class "numeric", should be or extend class "integer"
```

Here we see the effect of the formal class definition: each slot has a specific type that we have to supply.


```r
var <- Variant("1", 100L, "AGG", "G")
```

That will end up being annoying in general use, so we might want to modify our constructor:


```r
Variant <- function(chrom, pos, ref, alt) {
    new("Variant", chrom=as.character(chrom), pos=as.integer(pos), ref=ref, alt=alt)
}
var <- Variant(1, 100, "AGG", "G")
length(var)
```

```
## [1] 3
```

```r
ref(var)
```

```
## [1] "AGG"
```

What happens if we try to print var?


```r
var
```

```
## An object of class "Variant"
## Slot "chrom":
## [1] "1"
## 
## Slot "pos":
## [1] 100
## 
## Slot "ref":
## [1] "AGG"
## 
## Slot "alt":
## [1] "G"
```

That tells us what we want to know, but it takes up a lot of screen space. Let's define a more compact display of a Variant. For this we use the `show` method:


```r
setMethod("show", "Variant", function(x) {cat("chr", x@chrom, ":", x@pos, "_", x@ref, "/", x@alt, sep="", "\n")})
```

```
## Warning: For function 'show', signature 'Variant': argument in method
## definition changed from (x) to (object)
```

```
## [1] "show"
```

What's that all about? It's because the argument to the `show` generic function is called "object":


```r
show
```

```
## standardGeneric for "show" defined from package "methods"
## 
## function (object) 
## standardGeneric("show")
## <bytecode: 0x7faa78e032b0>
## <environment: 0x7faa78a91108>
## Methods may be defined for arguments: object
## Use  showMethods("show")  for currently available ones.
## (This generic function excludes non-simple inheritance; see ?setIs)
```

R changed this for us automatically, but it's better to do it right:


```r
setMethod("show", "Variant", function(object) {cat("chr", object@chrom, ":", object@pos, "_", object@ref, "/", object@alt, sep="", "\n")})
```

```
## [1] "show"
```

```r
var
```

```
## chr1:100_AGG/G
```



## Advanced topics

### Replacement methods

What if we want to change an object of our class? We can define a "replacement method", so we can use R's assignment operator to assign a new value to one of the slots. We call `setReplaceMethod`, which is similar to `setMethod` except that its signature has two arguments: the class of the object and the class of the new value being assigned to the slot. The replacement method must return the object itself.

We have to define another generic function, this time using the `<-` operator:


```r
setGeneric("ref<-", function(x, value) standardGeneric("ref<-"))
```

```
## [1] "ref<-"
```

```r
setReplaceMethod("ref", c("Variant", "character"), function(x, value) {
    x@ref <- value
    x
})
```

```
## [1] "ref<-"
```

```r
ref(var)
```

```
## [1] "AGG"
```

```r
ref(var) <- "GAAAAAA"
ref(var)
```

```
## [1] "GAAAAAA"
```

We defined the replacement value for `ref` as a character. What if we try to give it an argument of a different type?


```r
ref(var) <- 12
```

```
## Error in (function (classes, fdef, mtable) : unable to find an inherited method for function 'ref<-' for signature '"Variant", "numeric"'
```


### Multiple dispatch

A method signature can contain multiple classes, so you can define different functions for your method depending on, for example, whether its second argument is a character or an integer.

Let's define a method called "equal" that tells us whether two variants are equivalent to within a ref/alt swap.


```r
setGeneric("equal", function(x, y) standardGeneric("equal"))
```

```
## [1] "equal"
```

```r
setMethod("equal", c("Variant", "Variant"), function(x, y) {
    x@chrom == y@chrom & x@pos == y@pos & setequal(c(x@ref, x@alt), c(y@ref, y@alt))
})
```

```
## [1] "equal"
```

```r
var1 <- Variant(1, 100, "A", "G")
var2 <- Variant(1, 100, "G", "A")
equal(var1, var2)
```

```
## [1] TRUE
```

But what if we want to compare x to a list of other variants? We can write another method where the second argument has a different class.


```r
setMethod("equal", c("Variant", "list"), function(x, y) {
    lapply(y, function(i) equal(x, i))
})
```

```
## [1] "equal"
```

```r
var3 <- Variant(2, 100, "C", "T")
equal(var1, list(var2, var3))
```

```
## [[1]]
## [1] TRUE
## 
## [[2]]
## [1] FALSE
```


### Inheritance

You can define a class that inherits all slots and methods from the parent class, but with some added features. This works basically the same way as it does in other languages.

Let's make a class called "SNP", which is the same as a Variant, except that ref and alt can only be single bases.


```r
setClass("SNP", contains="Variant")
SNP <- function(chrom, pos, ref, alt) {
    object <- new("SNP", Variant(chrom, pos, ref, alt))
}
```

To enforce the constraint, we define a "validity method" for the SNP class. This is a function that returns TRUE if the object is a valid instance of the class, and otherwise prints an error message.


```r
setValidity("SNP", function(object) {
    if (nchar(object@ref) > 1) {
        return("'ref' must be a single character")
    }
    if (nchar(object@alt) > 1) {
        return("'alt' must be a single character")
    }
    TRUE
})
```

```
## Class "SNP" [in ".GlobalEnv"]
## 
## Slots:
##                                               
## Name:      chrom       pos       ref       alt
## Class: character   integer character character
## 
## Extends: "Variant"
```

```r
snp <- SNP(1,100,"AGG","G")
```

```
## Error in validObject(.Object): invalid class "SNP" object: 'ref' must be a single character
```

```r
snp <- SNP(1,100,"A","G")
```

We can use methods on SNP that were defined for Variant:


```r
length(snp)
```

```
## [1] 1
```

```r
ref(snp)
```

```
## [1] "A"
```


### Beware the NAMESPACE clashes

One of the biggest problems with R's OO system is when multiple packages define generic functions with the same name. This can lead to some very confusing behavior.


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.2
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
x <- data.frame(a=1, b=2)
rename(x, c=b)
```

```
##   a c
## 1 1 2
```

```r
library(GenomicRanges)
```

```
## Loading required package: stats4
```

```
## Loading required package: BiocGenerics
```

```
## Loading required package: parallel
```

```
## 
## Attaching package: 'BiocGenerics'
```

```
## The following objects are masked from 'package:parallel':
## 
##     clusterApply, clusterApplyLB, clusterCall, clusterEvalQ,
##     clusterExport, clusterMap, parApply, parCapply, parLapply,
##     parLapplyLB, parRapply, parSapply, parSapplyLB
```

```
## The following objects are masked from 'package:dplyr':
## 
##     combine, intersect, setdiff, union
```

```
## The following objects are masked from 'package:stats':
## 
##     IQR, mad, sd, var, xtabs
```

```
## The following objects are masked from 'package:base':
## 
##     anyDuplicated, append, as.data.frame, cbind, colMeans,
##     colnames, colSums, do.call, duplicated, eval, evalq, Filter,
##     Find, get, grep, grepl, intersect, is.unsorted, lapply,
##     lengths, Map, mapply, match, mget, order, paste, pmax,
##     pmax.int, pmin, pmin.int, Position, rank, rbind, Reduce,
##     rowMeans, rownames, rowSums, sapply, setdiff, sort, table,
##     tapply, union, unique, unsplit, which, which.max, which.min
```

```
## Loading required package: S4Vectors
```

```
## 
## Attaching package: 'S4Vectors'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     first, rename
```

```
## The following object is masked from 'package:base':
## 
##     expand.grid
```

```
## Loading required package: IRanges
```

```
## 
## Attaching package: 'IRanges'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     collapse, desc, slice
```

```
## Loading required package: GenomeInfoDb
```

```r
rename(x, c=b)
```

```
## Error in rename(x, c = b): object 'b' not found
```

We were warned that the object `rename` was "masked from package:dplyr": this means that when we loaded GenomicRanges, its generic for `rename` clobbered the generic previously defined by dplyr. We can still use it if we reference the package explicitly:


```r
dplyr::rename(x, c=b)
```

```
##   a c
## 1 1 2
```

Bioconductor maintains a package called BiocGenerics specifically to help with this problem. Its sole purpose is to contain generic functions used by more than one package, so that each package can import that one generic and define methods on it.



## More information

http://adv-r.had.co.nz/OO-essentials.html

https://bioconductor.org/packages/devel/bioc/vignettes/S4Vectors/inst/doc/S4QuickOverview.pdf

https://kasperdanielhansen.github.io/genbioconductor/html/R_S4.html

