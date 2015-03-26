## treeFun.R

root_marker <- "root"

make_tree <- function(d, parent_sep = ",", ancestor = NULL,
                      attrib = NULL)
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
            nodes[[root]][[root_marker]] <- TRUE # add root marker
            next
        }

        for (pid in pids)
            nodes[[pid]]$children <- c(child, nodes[[pid]]$children)
    }

    if (is.na(root))
        stop("Couldn't find root node.")

    if (is.null(ancestor))
        ancestor <- sample.int(999999L, 1L) # create random ancestor id

    tr <- list(root       = root,
               nodes      = nodes,
               data       = d,
               parent_sep = parent_sep,
               ancestor   = ancestor)

    if (!is.null(attrib))
        attributes(tr) <- attrib

    if (!inherits(tr, "tree"))
        class(tr) <- c("tree", class(tr))

    tr
}

make_derived_tree <- function(node_ids, tree)
{
    make_tree(tree$data[tree$data$id %in% node_ids, ],
              parent_sep = tree$parent_sep,
              ancestor   = tree$ancestor,
              attrib     = attributes(tree))
}

is_root <- function(node)
{
    root_marker %in% names(node)
}

print_nodes <- function(tree, nodef)
{
    data <- tree$data
    nodes <- tree$nodes
    seen <- make_observer()
    attrib <- attributes(tree)

    f <- function(id)
    {
        if (seen(id)) return()          # already been here

        ## Print current node.
        node <- nodes[[id]]
        ## Quote ids to avoid problems.
        pr1(double_quote(id))
        nodef(id, data, attrib)
        pr(";")

        ## Print child nodes.
        for (child in node$children)
            f(child)
    }
    f(tree$root)
}

print_edges <- function(tree, edgef)
{
    data <- tree$data
    nodes <- tree$nodes
    seen <- make_observer()
    attrib <- attributes(tree)

    f <- function(root)
    {
        if (seen(root)) return()        # alreayd been here

        d <- nodes$data
        for (child in nodes[[root]]$children) {
            pr1(double_quote(root), "->", double_quote(child))
            edgef(root, child, data, attrib)
            pr(";")
            f(child)
        }
    }
    f(tree$root)
}

print.tree <- function(x, nodef = NULL, edgef = NULL, ...)
{
    if (is.null(nodef))
        ## Use node id as label by default.
        nodef <- function(id, data, attrib) pr1("[label=", double_quote(id), "]")

    if (is.null(edgef))
        edgef <- function(from, to, data, attrib) return("")

    pr("digraph {")
    print_nodes(x, nodef)
    print_edges(x, edgef)
    pr("}")
}

tree2dot <- function(tree, filename, ...)
{
    sink(filename)
    print(tree, ...)
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
    if (length(ids) == 0L)
        stop("IDS must be of length >= 1.")

    nodes <- tree$nodes
    seen <- make_observer()

    f <- function(id)
    {
        if (seen(id)) return()          # already been here

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
    tr <- make_derived_tree(seen(show = TRUE), tree)
    attr(tr, "induced_by") <- ids
    tr
}

overlap_tree <- function(trees)
{
    if (!all_neighbors(`==`, sapply(trees, `[[`, "ancestor")))
        stop("Trees must have a common ancestor.")

    common_nodes <- Reduce(intersect,
                           lapply(trees, function(x) names(x$nodes)))

    make_derived_tree(common_nodes, trees[[1L]])
}

nodes <- function(tree)
{
    names(tree$nodes)
}

edges <- function(tree)
{
    tree$data[, c("id", "parents")]
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
    seen <- make_observer()

    f <- function(root, n)
    {
        if (seen(root)) return()        # already been here

        if (n == 1L) return()
        for (child in nodes[[root]]$children)
            f(child, n - 1L)
    }
    f(as.character(from), depth)

    make_derived_tree(seen(show = TRUE), tree)
}
