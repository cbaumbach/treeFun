## treeFun.R

make_tree <- function(d, id = "id", parent = "parent", label = "label",
                      parent_sep = ",")
{
    make_node <- function(x)
    {
        list(id       = as.character(x[[id]]),
             parent   = split_parents(x[[parent]], parent_sep),
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
        for (parent in nodes[[child]]$parent) {
            ## The root is the only node that doesn't have a parent in
            ## the tree.
            if (! parent %in% node_ids) {
                root <- child
                next
            }
            children <- nodes[[parent]]$children
            nodes[[parent]]$children <- c(child, children)
        }
    }

    list(root = root, nodes = nodes)
}

print_nodes <- function(tree)
{
    visited <- character(0L)

    f <- function(root, nodes)
    {
        ## Don't print twice.
        if (root %in% visited)
            return()
        else
            visited <<- c(root, visited)

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

split_parents <- function(parents, parent_sep = ",")
{
    unlist(strsplit(as.character(parents), parent_sep))
}

combine_parents <- function(parents, parent_sep = ",")
{
    paste0(sort(parents), collapse = parent_sep)
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

        parents <- nodes[[id]]$parent
        if (! parents[1L] %in% node_ids) # reached root node
            return(visited)

        for (pid in parents)            # visit parent nodes
            visited <- f(pid, visited)

        return(visited)
    }

    ## Find upstream nodes.
    visited <- character(0L)
    for (id in ids)
        visited <- f(id, visited)

    ## Build subtree from upstream nodes.
    make_tree(data.frame(
        id     = visited,
        parent = sapply(lapply(nodes[visited], `[[`, "parent"), combine_parents),
        label  = sapply(nodes[visited], `[[`, "label")))
}

overlap_tree <- function(trees)
{
    common_nodes <- Reduce(intersect,
                           lapply(trees, function(x) names(x$nodes)))
    make_tree(data.frame(
        id = common_nodes,
        parent = sapply(lapply(trees[[1]]$nodes[common_nodes], `[[`, "parent"), combine_parents),
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
        parent = sapply(lapply(tree$nodes, `[[`, "parent"), combine_parents),
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
