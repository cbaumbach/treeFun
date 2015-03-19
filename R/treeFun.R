## treeFun.R

make_tree <- function(d, id = "id", parent = "parent", label = "label")
{
    make_node <- function(x)
    {
        list(id       = as.character(x[[id]]),
             parent   = as.character(x[[parent]]),
             children = character(0L),
             label    = as.character(x[[label]]))
    }

    ## Create nodes.
    nodes <- apply(d, 1L, make_node)
    node_ids <- as.character(d[[id]])
    names(nodes) <- node_ids

    ## Add children to parent nodes and find root node.
    root <- NA
    for (child in names(nodes)) {
        parent <- nodes[[child]]$parent
        ## The root is the only node that doesn't have a parent in the
        ## tree.
        if (! parent %in% node_ids) {
            root <- child
            next
        }
        children <- nodes[[parent]]$children
        nodes[[parent]]$children <- c(child, children)
    }

    list(root = root, nodes = nodes)
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
    nodes <- tree$nodes
    node_ids <- names(nodes)

    f <- function(id, visited)
    {
        if (id %in% visited)            # already been here
            return(visited)

        visited <- c(id, visited)       # add id to visited nodes

        if (! nodes[[id]]$parent %in% node_ids) # reached root node
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
        label  = sapply(nodes[visited], `[[`, "label")))
}

overlap_tree <- function(trees)
{
    common_nodes <- Reduce(intersect,
                           lapply(trees, function(x) names(x$nodes)))
    make_tree(data.frame(
        id = common_nodes,
        parent = sapply(trees[[1]]$nodes[common_nodes], `[[`, "parent"),
        label  = sapply(trees[[1]]$nodes[common_nodes], `[[`, "label")))
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

tree_equal <- function(tree1, tree2)
{
    ## Two trees are equal if they have the same edges.
    edg1 <- edges(tree1)
    edg2 <- edges(tree2)

    edg1 <- edg1[order(edg1$id, edg1$parent),]
    edg2 <- edg2[order(edg2$id, edg2$parent),]

    identical(edg1, edg2)
}
