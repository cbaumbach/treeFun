\name{overlap_tree}
\alias{overlap_tree}
\docType{package}
\title{
  Find common subtree.
}
\description{
  Find common subtree.
}
\usage{
overlap_tree(trees)
}
\arguments{
  \item{trees}{
    List of tree object.s
  }
}
\value{
  Returns the largest tree that is a subtree of all trees in "trees".
}
\seealso{
  \code{\link{make_tree}}
  \code{\link{induced_tree}}
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

tr <- make_tree(d)

itr1 <- induced_tree(c("1","8"), tr)
itr2 <- induced_tree(c("2","5"), tr)

\dontrun{
tree2dot(overlap_tree(list(itr1, itr2)), "coretree.dot")
}
}
