## treeFun.R

root_marker <- "root"

make_tree <- function(d, parent_sep = ",")
{
    make_node <- function(x)
    {
        list(id       = as.character(x[["id"]]),
             parents  = split_parents(x[["parents"]], parent_sep),
             children = character(0L))
    }

    ## Create nodes.
    nodes <- apply(d[, c("id", "parents")], 1L, make_node)
    node_ids <- as.character(d[["id"]])
    names(nodes) <- node_ids

    ## Add children to parent nodes and find root node.
    root <- NA
    for (child in names(nodes)) {
        pids <- nodes[[child]]$parents
        ## Ignore parents that are not part of the tree.
        pids <- pids[pids %in% node_ids]
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

    list(root = root, nodes = nodes, data = d)
}

is_root <- function(node)
{
    root_marker %in% names(node)
}

print_nodes <- function(tree)
{
    nodes <- tree$nodes
    visited <- character(0L)

    f <- function(id)
    {
        ## Don't print twice.
        if (id %in% visited)
            return()
        visited <<- c(id, visited)

        ## Print current node.
        node <- nodes[[id]]
        pr("\"", id, "\"", "[label=\"", node$label, "\"];")

        ## Print child nodes.
        for (child in node$children)
            f(child)
    }
    f(tree$root)
}

print_edges <- function(tree)
{
    f <- function(root, nodes)
    {
        for (child in nodes[[root]]$children) {
            pr("\"", root, "\"->\"", child, "\";")
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
    visited <- character()

    f <- function(id)
    {
        if (id %in% visited)            # already been here
            return()

        visited <<- c(id, visited)      # add id to visited nodes

        node <- nodes[[id]]
        parents <- node$parents
        if (is_root(node))              # reached root node
            return()

        for (pid in parents)            # visit parent nodes
            f(pid)
    }

    ## Find upstream nodes.
    for (id in ids)
        f(id)

    ## Build subtree from upstream nodes.
    make_tree(tree$data[tree$data$id %in% visited, ])
}

overlap_tree <- function(trees)
{
    common_nodes <- Reduce(intersect,
                           lapply(trees, function(x) names(x$nodes)))

    d <- trees[[1L]]$data
    make_tree(d[d$id %in% common_nodes, ])
}

nodes <- function(tree)
{
    names(tree$nodes)
}

edges <- function(tree)
{
    data.frame(
        id      = names(tree$nodes),
        parents = sapply(lapply(tree$nodes, `[[`, "parents"), combine_parents),
        stringsAsFactors = FALSE)
}

tree_equal <- function(tree1, tree2)
{
    ## Two trees are equal if they have the same edges.
    edg1 <- edges(tree1)
    edg2 <- edges(tree2)

    edg1 <- edg1[order(edg1$id, edg1$parents),]
    edg2 <- edg2[order(edg2$id, edg2$parents),]

    identical(edg1, edg2)
}

extract_tree <- function(tree, depth, from = tree$root)
{
    if (missing(depth))
        stop("Missing DEPTH argument.")

    depth <- as.integer(depth)
    if (is.na(depth) || depth <= 0L)
        stop("DEPTH must be >= 1L.")

    nodes <- tree$nodes
    visited <- character()
    f <- function(root, n)
    {
        if (root %in% visited) return()
        visited <<- c(root, visited)
        if (n == 1L) return()
        for (child in nodes[[root]]$children)
            f(child, n - 1L)
    }
    f(as.character(from), depth)

    make_tree(tree$data[tree$data$id %in% visited, ])
}
