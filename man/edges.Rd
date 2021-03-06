\name{edges}
\alias{edges}
\docType{package}
\title{
  Extract edges of tree object.
}
\description{
  Extract edges of tree object.
}
\usage{
edges(tree)
}
\arguments{
  \item{tree}{
    Tree object.
  }
}
\value{
  Return data frame with edges of tree object.
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

edges(make_tree(d))
}
