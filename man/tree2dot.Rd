\name{tree2dot}
\alias{tree2dot}
\docType{package}
\title{
  Write tree to dot file.
}
\description{
  Write tree to dot file.  This is basically a wrapper around the
  \code{print_tree} function that writes the dot format representation
  of the given tree to a file for later conversion into a picture using
  tools from the open source graph visualization software Graphviz
  (www.graphviz.org).
}
\usage{
tree2dot(tree, filename, ...)
}
\arguments{
  \item{tree}{
    Tree object.
  }
  \item{filename}{
    Name of output file.
  }
  \item{...}{
    Arguments passed to \code{print_tree} like `nodef' and `edgef'.
  }
}
\value{
  Produces a file in dot format that can be converted into a picture
  using tools from the open source graph visualization software Graphviz
  (www.graphviz.org).  For example, to convert the file `foo.dot' into
  the png file `foo.dot.png' you could run `dot -Tpng -O foo.dot' from
  the command line.
}
\details{
  You can change the way your trees are annotated by passing your own
  functions for printing dot format node and edge attributes to
  \code{tree2dot} via arguments `nodef' and `edgef' that get passed to
  \code{print_tree}.
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

\dontrun{tree2dot(make_tree(d), "tree.dot")}
}
