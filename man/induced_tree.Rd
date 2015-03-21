\name{induced_tree}
\alias{induced_tree}
\docType{package}
\title{
  Build tree induced by set of node ids.
}
\description{
  Build tree induced by set of node ids.
}
\usage{
induced_tree(ids, tree)
}
\arguments{
  \item{ids}{
    Character vector of node ids.
  }
  \item{tree}{
    Tree object.
  }
}
\value{
  Returns the subtree of "tree" that is induced by "ids", that is, the
  tree consisting of the nodes of "tree" with ids in "ids" and all their
  ancestor nodes.
}
\seealso{
  \code{\link{make_tree}}
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
tree2dot(itr1, "subtree1.dot")
tree2dot(itr2, "subtree2.dot")
}
}
