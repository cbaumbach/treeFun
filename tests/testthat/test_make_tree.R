context("Converting data frames into trees")

test_that("it works for different kind of predicates", {
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
"), header = TRUE, stringsAsFactors = FALSE,
        colClasses = "character")

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
