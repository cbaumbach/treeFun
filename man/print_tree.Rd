\name{print_tree}
\alias{print_tree}
\docType{package}
\title{
  Show tree representation in dot format
}
\description{
  Show tree representation in dot format
}
\usage{
print_tree(tree)
}
\arguments{
  \item{tree}{
    Tree object.
  }
}
\value{
  Produces a representation of the tree in dot format.  Sometimes that
  is enough to give you an idea of what the tree looks like.  Often,
  however, it's better to convert that representation into a picture
  using tools from the open source graph visualization software Graphviz
  (www.graphviz.org) and look at that instead (see \code{tree2dot}).
}
\seealso{
  \code{\link{make_tree}}
  \code{\link{tree2dot}}
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

print_tree(make_tree(d))
}
