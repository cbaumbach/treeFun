context("Working with tree structures.")

test_that("transforming from and to data frames works", {
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
"), header = TRUE, stringsAsFactors = FALSE, colClasses = "character")

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
    edg <- edges(tr)

    rownames(edg) <- NULL
    rownames(d)   <- NULL

    expect_that(identical(sort(nodes(tr)), sort(d$id)), is_true())
    expect_that(identical(edg[order(edg$id),], d[order(d$id), c("id", "parent")]), is_true())
})

test_that("tree comparisons work", {
    d1 <- read.table(textConnection("\
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
"), header = TRUE, stringsAsFactors = FALSE, colClasses = "character")

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

    d2 <- d1
    d2$label <- toupper(d1$label)
    d2 <- d2[sample.int(nrow(d2)),]

    ## d2 has the same structure as d1 but its labels are in uppercase
    ## and its rows are permuted.

    d3 <- d1[-nrow(d1),]                # d1 without the "8" node

    tr1 <- make_tree(d1)
    tr2 <- make_tree(d2)
    tr3 <- make_tree(d3)

    expect_that(tree_equal(tr1, tr2), is_true())
    expect_that(tree_equal(tr1, tr3), is_false())
    expect_that(tree_equal(tr2, tr3), is_false())
})

test_that("induced and overlap trees work", {
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
"), header = TRUE, stringsAsFactors = FALSE, colClasses = "character")

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

    rownames(d) <- d$id

    tr <- make_tree(d)
    itr1 <- induced_tree("6", tr)
    itr2 <- induced_tree("8", tr)
    itr3 <- induced_tree("7", tr)
    itr4 <- induced_tree("3", tr)
    itr5 <- induced_tree(c("3", "4"), tr)
    itr6 <- induced_tree(c("5", "6"), tr)

    expect_that(tree_equal(itr1, make_tree(d[c("0", "2", "6"),])), is_true())
    expect_that(tree_equal(itr2, make_tree(d[c("0", "2", "7", "8"),])), is_true())
    expect_that(tree_equal(itr3, make_tree(d[c("0", "2", "7"),])), is_true())
    expect_that(tree_equal(itr4, make_tree(d[c("0", "1", "3"),])), is_true())
    expect_that(tree_equal(itr5, make_tree(d[c("0", "1", "3", "4"),])), is_true())
    expect_that(tree_equal(itr6, make_tree(d[c("0", "1", "5", "2", "6"),])), is_true())
})
