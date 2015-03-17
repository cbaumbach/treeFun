\name{tree2dot}
\alias{tree2dot}
\docType{package}
\title{
  Write tree to dot file.
}
\description{
  Write tree to dot file.
}
\usage{
tree2dot(tree, filename)
}
\arguments{
  \item{tree}{
    Tree object.
  }
  \item{filename}{
    Name of output file.
  }
}
\value{
  None.
}
\seealso{
  \code{\link{make_tree}}
}
\examples{
library(treeFun)

d <- read.table(textConnection("\
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

\dontrun{tree2dot(make_tree(d), "tree.dot")}
}
