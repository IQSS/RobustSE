# check to make sure fitted model is a GLM
test_that("FAIL TEST: require GLM", {
    out <- lm(Sepal.Length ~ ., data = iris)
    expect_error(GIM(out, B=30, B2=25), "glm object required for estimation")
})


