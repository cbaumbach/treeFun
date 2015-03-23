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
make_tree(d, parent_sep = ",")
}
\arguments{
  \item{d}{
    Data frame to be converted into a tree structure.
  }
  \item{parent_sep}{
    String separating parent ids in case of multiple parents.
  }
}
\value{
  Returns a tree object.
}
\details{
  The data frame must have the columns "id" and "parents" corresponding
  to node identifiers and a (by default comma-separated) list of parent
  node identifiers.
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

\dontrun{tree2dot(make_tree(d))}
}
