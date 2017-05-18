test_that("FAIL TEST: require GLM", {
    out <- lm(Sepal.Length ~ ., data = iris)
    expect_error(GIM(out), "glm required for estimation")
})

