## treeFun.R

make_tree <- function(d, id = "id", parent = "parent", label = "label",
                      root_parent = "-")
{
    root_id <- NA

    make_node <- function(x)
    {
        if (x[[parent]] == root_parent)
            root_id <<- x[[id]]         # remember root id

        list(id       = as.character(x[[id]]),
             parent   = as.character(x[[parent]]),
             children = character(0L),
             label    = as.character(x[[label]]))
    }

    ## Create nodes.
    nodes <- apply(d, 1L, make_node)
    names(nodes) <- as.character(d[[id]])

    ## Add children to parent nodes.
    for (child in names(nodes)) {
        if (child == root_id)
            next
        parent <- nodes[[child]]$parent
        children <- nodes[[parent]]$children
        nodes[[parent]]$children <- c(child, children)
    }

    list(root = root_id, nodes = nodes, root_parent = root_parent)
}

print_nodes <- function(tree)
{
    f <- function(root, nodes)
    {
        ## Print current node.
        node <- nodes[[root]]
        pr(root, "[label=\"", node$label, "\"];")

        ## Print child nodes.
        for (child in node$children)
            f(child, nodes)
    }
    f(tree$root, tree$nodes)
}

print_edges <- function(tree)
{
    f <- function(root, nodes)
    {
        for (child in nodes[[root]]$children) {
            pr(root, "->", child, ";")
            f(child, nodes)
        }
    }
    f(tree$root, tree$nodes)
}

print_tree <- function(tree)
{
    pr("digraph {")
    print_nodes(tree)
    print_edges(tree)
    pr("}")
}

tree2dot <- function(tree, filename)
{
    sink(filename)
    print_tree(tree)
    sink()
}

induced_tree <- function(ids, tree)
{
    root_parent <- tree$root_parent
    nodes       <- tree$nodes

    f <- function(id, visited)
    {
        if (id %in% visited)            # already been here
            return(visited)

        visited <- c(id, visited)       # add id to visited nodes

        if (nodes[[id]]$parent == root_parent) # reached root node
            return(visited)

        f(nodes[[id]]$parent, visited)
    }

    ## Find upstream nodes.
    visited <- character(0L)
    for (id in ids)
        visited <- f(id, visited)

    ## Build subtree from upstream nodes.
    make_tree(data.frame(
        id     = visited,
        parent = sapply(nodes[visited], `[[`, "parent"),
        label  = sapply(nodes[visited], `[[`, "label")),
              root_parent = root_parent)
}

## So far overlap_tree only works if all trees have the same root.  In
## principle, it should also work for arbitrary overlapping trees as
## long as no tree is empty.  The difficulty in that case would be to
## find the root_parent of the overlap tree which is needed in the
## call to make_tree.  We know that it's one of the root_parents of
## the trees that we overlap.  But which?
overlap_tree <- function(trees)
{
    ## All trees must have the same root.
    stopifnot(all_neighbors(`==`, sapply(trees, `[[`, "root")))

    common_nodes <- Reduce(intersect,
                           lapply(trees, function(x) names(x$nodes)))
    make_tree(data.frame(
        id = common_nodes,
        parent = sapply(trees[[1]]$nodes[common_nodes], `[[`, "parent"),
        label  = sapply(trees[[1]]$nodes[common_nodes], `[[`, "label")),
              root_parent = trees[[1]]$root_parent)
}

nodes <- function(tree)
{
    names(tree$nodes)
}

edges <- function(tree)
{
    data.frame(
        id     = names(tree$nodes),
        parent = sapply(tree$nodes, `[[`, "parent"),
        stringsAsFactors = FALSE)
}
