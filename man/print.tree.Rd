\name{print.tree}
\alias{print.tree}
\docType{package}
\title{
  Show tree representation in dot format
}
\description{
  Show tree representation in dot format
}
\usage{
\method{print}{tree}(x, nodef = NULL, edgef = NULL, \dots)
}
\arguments{
  \item{x}{
    Tree object.
  }
  \item{nodef}{
    Function for printing node attributes in dot format.
  }
  \item{edgef}{
    Function for printing node attributes in dot format.
  }
  \item{...}{
    Ignored.
  }
}
\value{
  Produces a representation of the tree in dot format.  Sometimes that
  is enough to give you an idea of what the tree looks like.  Often,
  however, it's better to convert that representation into a picture
  using tools from the open source graph visualization software Graphviz
  (www.graphviz.org) and look at that instead (see \code{tree2dot}).
}
\details{
  The `nodef'/`edgef' functions should print dot format node/edge
  attributes like `[label="node1"]' to standard output.  The `nodef'
  function is called with two arguments: the current node id and a data
  frame.  The `edgef' function is called with three arguments: the id of
  the node where the edge starts (the parent node), the id of the node
  where the edge ends (the child node), and a data frame.  In both
  cases, the data frame is the one that was used to construct the
  initial tree object (an ancestor of `tree') using \code{make_tree}.
  Rows corresponding to nodes that are not in `tree' were removed from
  the data frame.  By default, nodes are labeled with their id and edges
  are drawn as arrows from parent to child without any label.
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

print(make_tree(d))
}
