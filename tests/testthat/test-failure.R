
# check to make sure fitted model is a GLM
test_that("FAIL TEST: require GLM", {
    out <- lm(Sepal.Length ~ ., data = iris)
    expect_error(GIM(out, B=30, B2=25), "glm object required for estimation")
})

library(Ecdat)
data(Fatality)
library(MASS)
data(Pima.te)
library(MASS)
data(epil)

test_that("PASS TEST: no strange robust standard errors",{
    # ols modeling traffic fatality rate
    ols <- glm(mrall ~ beertax + factor(year), data=Fatality)

    # Quick rule of thumb for model misspecification
    ols.hc <- GIM(ols, full = FALSE)$'Rule of Thumb'

    # Quick rule of thumb for model misspecification; data clustered by state
    ols.cl <- GIM(ols, full = FALSE, cluster = Fatality$state)$'Rule of Thumb'

    logit <- glm(type ~ age + npreg + glu + bp + skin + bmi,
                 data = Pima.te, family = binomial("logit"))

    # Quick rule of thumb for model misspecification
    logit.hc <- GIM(logit, full = FALSE)$'Rule of Thumb'

    # arbitrary cluster to test clustering
    Pima.te$cluster <- as.factor(sample(1:20, nrow(Pima.te), replace = TRUE))

    # Quick rule of thumb for model misspecification; data clustered by subject
    logit.cl <- GIM(logit, full = FALSE, cluster = Pima.te$cluster)$'Rule of Thumb'

    probit <- glm(type ~ age + npreg + glu + bp + skin + bmi,
                  data = Pima.te, family = binomial("probit"))

    # Quick rule of thumb for model misspecification
    probit.hc <- GIM(probit, full = FALSE)$'Rule of Thumb'

    # Quick rule of thumb for model misspecification; data clustered by subject
    probit.cl <- GIM(probit, full = FALSE, cluster = Pima.te$cluster)$'Rule of Thumb'

    # Poisson regression modeling seizure counts for epileptics
    pois <- glm(y ~ trt + base + age + V4 + lbase + lage,
                data = epil, family = "poisson")

    # Quick rule of thumb for model misspecification
    pois.hc <- GIM(pois, full = FALSE)$'Rule of Thumb'

    # Quick rule of thumb for model misspecification; data clustered by subject
    pois.cl <- GIM(pois, full = FALSE, cluster = epil$subject)$'Rule of Thumb'

    neg.bin <- glm.nb(y ~ trt + base + age + V4 + lbase + lage,
                      data = epil)

    # Quick rule of thumb for model misspecification
    nb.hc <- GIM(neg.bin, full = FALSE)$'Rule of Thumb'

    # Quick rule of thumb for model misspecification; data clustered by subject
    nb.cl <- GIM(neg.bin, full = FALSE, cluster = epil$subject)$'Rule of Thumb'

    # test to see that ratio of standard errors is a reasonable size

    expect_that(ols.hc, is_more_than(.85))
    expect_that(ols.cl, is_more_than(.85))
    expect_that(logit.hc, is_more_than(.85))
    expect_that(logit.cl, is_more_than(.85))
    expect_that(probit.hc, is_more_than(.85))
    expect_that(probit.cl, is_more_than(.85))
    expect_that(pois.hc, is_more_than(.85))
    expect_that(pois.cl, is_more_than(.85))
    expect_that(nb.hc, is_more_than(.85))
    expect_that(nb.cl, is_more_than(.85))

})



