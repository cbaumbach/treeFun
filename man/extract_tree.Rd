\name{extract_tree}
\alias{extract_tree}
\docType{package}
\title{
  Downstream extraction of subtrees
}
\description{
  Downstream extraction of subtrees
}
\usage{
extract_tree(tree, depth, from = tree$root)
}
\arguments{
  \item{tree}{
    Tree object from which to extract subtree.
  }
  \item{depth}{
    Number of levels to extract from `tree'.
  }
  \item{from}{
    Id of node where extraction should begin.
  }
}
\value{
  Returns a subtree of `tree'.
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
print(extract_tree(tr, 2))
print(extract_tree(tr, 2, from = "1"))
}
