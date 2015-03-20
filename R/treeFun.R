## treeFun.R

root_marker <- "root"

make_tree <- function(d, id = "id", parent = "parent", label = id,
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
        pids <- nodes[[child]]$parent
        ## Only root has none of its parents in the tree.
        if (!any(pids %in% node_ids)) {
            if (is.na(child))
                stop("Root node id is missing (NA).")
            if (!is.na(root))
                stop("Found >=1 root node: ", root, ", ", child)
            root <- child
            nodes[[root]]$root <- TRUE  # add root marker
            next
        }

        for (pid in pids)
            nodes[[pid]]$children <- c(child, nodes[[pid]]$children)
    }

    if (is.na(root))
        stop("Couldn't find root node.")

    list(root = root, nodes = nodes)
}

is_root <- function(node)
{
    root_marker %in% names(node)
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

        node <- nodes[[id]]
        parents <- node$parent
        if (is_root(node))              # reached root node
            return()

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
