\name{make_tree}
\alias{make_tree}
\docType{package}
\title{
  Convert data frame to tree structure.
}
\description{
  Convert data frame to tree structure.
}
\usage{
make_tree(d, parent_sep = ",", ancestor = NULL, attrib = NULL)
}
\arguments{
  \item{d}{
    Data frame to be converted into a tree structure.
  }
  \item{parent_sep}{
    String separating parent ids in case of multiple parents.
  }
  \item{ancestor}{
    Random id identifying the initial tree from which the tree defined
    by `d' was derived.
  }
  \item{attrib}{
    Attributes to be attached to final tree structure.
  }
}
\value{
  Returns an object of S3 class `tree'.
}
\details{
  The data frame must have the columns `id' and `parents' corresponding
  to node identifiers and a (by default comma-separated) list of parent
  node identifiers.

  The `ancestor' argument should not be specified unless the tree in `d'
  was derived from another tree in which case `ancestor' should be set
  to the latter tree's `ancestor'.  The `ancestor' id is needed to make
  sure that whenever we operate on more than one tree, all trees derive
  from the same ancestor and are thus compatible in terms of nodes,
  edges, and supplementary data.
}
\examples{
library(treeFun)

d <- read.table(textConnection("\
id parents label
0 - a
1 0 b
2 0 c
3 1 d
4 1 e
5 1 f
6 2 g
7 2 h
8 7 i
"), header = TRUE, stringsAsFactors = FALSE)

##         0a
##        / \
##      /     \
##    1b       2c
##   /|\      / \
##  / | \    /   \
## 3d 4e 5f 6g    7h
##                 \
##                  \
##                   8i

print(make_tree(d))
}
