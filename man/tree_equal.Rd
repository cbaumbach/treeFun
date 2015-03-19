\name{tree_equal}
\alias{tree_equal}
\docType{package}
\title{
  Test if two trees have the same edges
}
\description{
  Two trees are equal if they have the same edges.  This means that two
  trees that differ in some respect other than their edges will
  nonetheless be considered equal.
}
\usage{
tree_equal(tree1, tree2)
}
\arguments{
  \item{tree1}{
    Tree object.
  }
  \item{tree2}{
    Tree object.
  }
}
\value{
  Returns TRUE if "tree1" and "tree2" have the same edges.
}
\seealso{
  \code{\link{make_tree}}
}
\examples{
library(treeFun)

d1 <- read.table(textConnection("\
id parent label
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

d2 <- d1
d2$label <- toupper(d1$label)
d2 <- d2[sample.int(nrow(d2)),]

## d2 has the same structure as d1 but its labels are in uppercase and
## its rows are permuted.

tree_equal(make_tree(d1), make_tree(d2))
}
