\name{treeFun-package}
\alias{treeFun-package}
\alias{treeFun}
\docType{package}
\title{
Functions for dealing with genetic data
}
\description{
This is a collection of functions for creating, manipulation, and
plotting tree structures.  If you want to convert your trees into
displayable file formats, you will need the open source graph
visualization software Graphviz (www.graphviz.org).
}
\details{
\tabular{ll}{
Package: \tab treeFun\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-03-17\cr
License: \tab GPL (>= 2)\cr
}

Index:
\preformatted{
  edges         Extract edges of tree object
  induced_tree  Build tree induced by set of node ids
  make_tree     Convert data frame to tree structure
  nodes         Extract nodes of tree object
  overlap_tree  Find common subtree
  tree_equal    Test if two trees have the same edges
  tree2dot      Write tree to dot file
}
}
\author{
Clemens Baumbach

Maintainer: Clemens Baumbach <clemens.baumbach@helmholtz-muenchen.de>
}
\references{
Graphviz (open source graph visualization software, \url{www.graphviz.org}).
}
\keyword{ manip }
\keyword{ attribute }
\keyword{ list }
\keyword{ utilities }
\keyword{ package }
\seealso{
  \code{\link{tree2dot}}
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

d2 <- d
d2$label <- toupper(d$label)
d2 <- d2[sample.int(nrow(d2)),]

## d2 has the same structure as d1 but its labels are in uppercase and
## its rows are permuted.

tr <- make_tree(d)
tr2 <- make_tree(d2)

tree_equal(tr, tr2)

nodes(tr)
edges(tr)

\dontrun{tree2dot(tr, "tree.dot")}

itr1 <- induced_tree(c("1","8"), tr)
itr2 <- induced_tree(c("2","5"), tr)

overlap_tree(list(itr1, itr2))
}
