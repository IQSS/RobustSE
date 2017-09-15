<!-- README.md is generated from README.Rmd. Please edit that file -->
RobustSE
========

[![Travis-CI Build
Status](https://travis-ci.org/IQSS/RobustSE.svg?branch=master)](https://travis-ci.org/IQSS/RobustSE)

The RobustSE package implements the generalized information matrix (GIM)
test to detect model misspecification described by [King & Roberts
(2015)](https://gking.harvard.edu/files/gking/files/robust_0.pdf).

When a researcher suspects a model may be misspecified, rather than
attempting to correct by fitting robust standard errors, the GIM test
should be utilized as a formal statistical test for model
misspecification. If the GIM test rejects the null hypothesis, the
researcher should re-specify the model, as it is possible estimators of
the misspecified model will be biased.

The `GIM()` function in the RobustSE package can be used to derive a
quick rule of thumb estimate for whether or not a model is misspecified
by comparing the size of classic and robust standard errors, as well as
perform the full GIM test for linear, Poisson, and Negative binomial
regressions.

The `GIM()` function accepts
`glm(..., family = gaussian(link="identity"))`,
`glm(..., family=binomial)` with logit and probit links,
`glm(..., family = "poisson")`, and `glm.nb(...)` objects as input. It
is suggested that the researcher first perform the quick rule of thumb
evaluation for model misspecification on their desired model by setting
`full = FALSE`. In this case, `GIM()` will return the estimated
coefficients, classic standard errors, robust standard errors, z-values,
p-values with respect to the robust standard errors, and the rule of
thumb for misspecification. If the rule of thumb is greater than 1.5
(i.e. any robust standard error is 1.5 times larger than its classic
standard error), the output suggests there may be misspecification and
to perform the full GIM test on the fitted model.

The full GIM test can be performed by setting `full = TRUE`. In this
scenario, the researcher must also set the number of bootstraps for each
part of the double bootstrap (`B` and `B2`) that makes up the full GIM
test. The full GIM test returns the same things as the quick test above,
as well as the GIM test statistic and the GIM test p-value. If the GIM
test null hypothesis is rejected, this is a signal of model
misspecification and the reseacher should consider re-specifying the
model.

If the researcher believes it is appropriate to use cluster-robust
standard errors, `GIM()` can handle this as well for both the rule of
thumb test and the full test by setting `cluster = data$cluster` and the
same parameters described above. In this scenario, both the rule of
thumb and the full test return the same things, except the reported
robust standard errors are cluster-robust standard errors.

Installation
------------

You can install RobustSE from Github with:

    # install.packages("devtools")
    devtools::install_github("IQSS/RobustSE")

Example
-------

NOTE: In order for standard errors to be calcualted correctly GLM
objects must be used.

OLS with and without clustering

    # install.packages("Ecdat")
    library(Ecdat)
    library(RobustSE)
    data(Fatality)

    # ols modeling traffic fatality rate
    ols <- glm(mrall ~ beertax + factor(year), data = Fatality)

    # Quick rule of thumb for model misspecification
    GIM(ols, full = FALSE)

    ## [1] "Max ratio of robust to classic standard errors is 1.252"
    ## [1] "Rule of thumb suggests your model is NOT misspecified"

    ## $Coefficients
    ##                      Estimate  Std. Err. Robust Std. Err.      z value
    ## (Intercept)       1.894847715 0.08565853       0.10726683 17.664805510
    ## beertax           0.366335812 0.06260000       0.05509815  6.648786556
    ## factor(year)1983 -0.082035881 0.11167343       0.13064051 -0.627951340
    ## factor(year)1984 -0.071733094 0.11167336       0.12368611 -0.579960773
    ## factor(year)1985 -0.110545807 0.11167647       0.12306338 -0.898283559
    ## factor(year)1986 -0.016118470 0.11168154       0.12406081 -0.129923939
    ## factor(year)1987 -0.015535488 0.11169502       0.12483837 -0.124444820
    ## factor(year)1988 -0.001027129 0.11171801       0.12160959 -0.008446117
    ##                      Pr(>|z|)
    ## (Intercept)      7.828216e-70
    ## beertax          2.955192e-11
    ## factor(year)1983 5.300358e-01
    ## factor(year)1984 5.619411e-01
    ## factor(year)1985 3.690344e-01
    ## factor(year)1986 8.966266e-01
    ## factor(year)1987 9.009631e-01
    ## factor(year)1988 9.932611e-01
    ## 
    ## $`Rule of Thumb`
    ## [1] 1.252

    # Full GIM test for model misspecification
    GIM(ols, full = TRUE, B = 30, B2 = 25)

    ## ===========================================================================[1] "omegaB"
    ##          [,1]
    ## [1,] 360.2396

    ## $Coefficients
    ##                      Estimate  Std. Err. Robust Std. Err.      z value
    ## (Intercept)       1.894847715 0.08565853       0.10726683 17.664805510
    ## beertax           0.366335812 0.06260000       0.05509815  6.648786556
    ## factor(year)1983 -0.082035881 0.11167343       0.13064051 -0.627951340
    ## factor(year)1984 -0.071733094 0.11167336       0.12368611 -0.579960773
    ## factor(year)1985 -0.110545807 0.11167647       0.12306338 -0.898283559
    ## factor(year)1986 -0.016118470 0.11168154       0.12406081 -0.129923939
    ## factor(year)1987 -0.015535488 0.11169502       0.12483837 -0.124444820
    ## factor(year)1988 -0.001027129 0.11171801       0.12160959 -0.008446117
    ##                      Pr(>|z|)
    ## (Intercept)      7.828216e-70
    ## beertax          2.955192e-11
    ## factor(year)1983 5.300358e-01
    ## factor(year)1984 5.619411e-01
    ## factor(year)1985 3.690344e-01
    ## factor(year)1986 8.966266e-01
    ## factor(year)1987 9.009631e-01
    ## factor(year)1988 9.932611e-01
    ## 
    ## $`Rule of Thumb`
    ## [1] 1.252
    ## 
    ## $`GIM test statistic`
    ##          [,1]
    ## [1,] 360.2396
    ## 
    ## $`GIM pval`
    ## [1] 0.6129032

    # Quick rule of thumb for model misspecification; data clustered by state
    GIM(ols, full = FALSE, cluster = Fatality$state)

    ## [1] "Max ratio of robust to classic standard errors is 1.987"
    ## [1] "Rule of thumb suggests your model is misspecified, it is suggested that you run the full GIM test"

    ## $Coefficients
    ##                      Estimate  Std. Err. Robust Std. Err.     z value
    ## (Intercept)       1.894847715 0.08565853       0.14476869 13.08879540
    ## beertax           0.366335812 0.06260000       0.12435912  2.94578956
    ## factor(year)1983 -0.082035881 0.11167343       0.03562723 -2.30261755
    ## factor(year)1984 -0.071733094 0.11167336       0.04650340 -1.54253433
    ## factor(year)1985 -0.110545807 0.11167647       0.04890053 -2.26062599
    ## factor(year)1986 -0.016118470 0.11168154       0.06132064 -0.26285553
    ## factor(year)1987 -0.015535488 0.11169502       0.06791986 -0.22873262
    ## factor(year)1988 -0.001027129 0.11171801       0.06646702 -0.01545321
    ##                      Pr(>|z|)
    ## (Intercept)      3.816119e-39
    ## beertax          3.221315e-03
    ## factor(year)1983 2.130037e-02
    ## factor(year)1984 1.229438e-01
    ## factor(year)1985 2.378243e-02
    ## factor(year)1986 7.926619e-01
    ## factor(year)1987 8.190767e-01
    ## factor(year)1988 9.876706e-01
    ## 
    ## $`Rule of Thumb`
    ## [1] 1.987

    # Full GIM test for model misspecification; data clustered by state
    GIM(ols, full = TRUE, B = 30, B2 = 25, cluster = Fatality$state)

    ## ===========================================================================[1] "omegaB"
    ##         [,1]
    ## [1,] 1369710

    ## $Coefficients
    ##                      Estimate  Std. Err. Robust Std. Err.     z value
    ## (Intercept)       1.894847715 0.08565853       0.14476869 13.08879540
    ## beertax           0.366335812 0.06260000       0.12435912  2.94578956
    ## factor(year)1983 -0.082035881 0.11167343       0.03562723 -2.30261755
    ## factor(year)1984 -0.071733094 0.11167336       0.04650340 -1.54253433
    ## factor(year)1985 -0.110545807 0.11167647       0.04890053 -2.26062599
    ## factor(year)1986 -0.016118470 0.11168154       0.06132064 -0.26285553
    ## factor(year)1987 -0.015535488 0.11169502       0.06791986 -0.22873262
    ## factor(year)1988 -0.001027129 0.11171801       0.06646702 -0.01545321
    ##                      Pr(>|z|)
    ## (Intercept)      3.816119e-39
    ## beertax          3.221315e-03
    ## factor(year)1983 2.130037e-02
    ## factor(year)1984 1.229438e-01
    ## factor(year)1985 2.378243e-02
    ## factor(year)1986 7.926619e-01
    ## factor(year)1987 8.190767e-01
    ## factor(year)1988 9.876706e-01
    ## 
    ## $`Rule of Thumb`
    ## [1] 1.987
    ## 
    ## $`GIM test statistic`
    ##         [,1]
    ## [1,] 1369710
    ## 
    ## $`GIM pval`
    ## [1] 0.03225806

Poisson regression with and without clustering

    # install.packages("MASS")
    library(MASS)

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:Ecdat':
    ## 
    ##     SP500

    data(epil)

    # Poisson regression modeling seizure counts for epileptics
    pois <- glm(y ~ trt + base + age + V4 + lbase + lage,
                data = epil, family = "poisson")

    # Quick rule of thumb for model misspecification
    GIM(pois, full = FALSE)

    ## [1] "Max ratio of robust to classic standard errors is 2.823"
    ## [1] "Rule of thumb suggests your model is misspecified, it is suggested that you run the full GIM test"

    ## $Coefficients
    ##                 Estimate   Std. Err. Robust Std. Err.    z value
    ## (Intercept)   3.05539782 0.981367261      2.770554231  1.1028110
    ## trtprogabide -0.05484456 0.050130822      0.111300461 -0.4927613
    ## base          0.01163038 0.001393269      0.003554125  3.2723606
    ## age          -0.05615889 0.034616587      0.096318357 -0.5830549
    ## V4           -0.15976960 0.054583706      0.110260460 -1.4490199
    ## lbase         0.63808395 0.076092125      0.175360128  3.6387060
    ## lage          2.24198067 0.982082103      2.657910922  0.8435123
    ##                  Pr(>|z|)
    ## (Intercept)  0.2701092286
    ## trtprogabide 0.6221812746
    ## base         0.0010665347
    ## age          0.5598563211
    ## V4           0.1473320265
    ## lbase        0.0002740114
    ## lage         0.3989419651
    ## 
    ## $`Rule of Thumb`
    ## [1] 2.823

    # Full GIM test for model misspecification
    GIM(pois, full = TRUE, B = 30, B2 = 25)

    ## ===========================================================================[1] "omegaB"
    ##          [,1]
    ## [1,] 1719.892

    ## $Coefficients
    ##                 Estimate   Std. Err. Robust Std. Err.    z value
    ## (Intercept)   3.05539782 0.981367261      2.770554231  1.1028110
    ## trtprogabide -0.05484456 0.050130822      0.111300461 -0.4927613
    ## base          0.01163038 0.001393269      0.003554125  3.2723606
    ## age          -0.05615889 0.034616587      0.096318357 -0.5830549
    ## V4           -0.15976960 0.054583706      0.110260460 -1.4490199
    ## lbase         0.63808395 0.076092125      0.175360128  3.6387060
    ## lage          2.24198067 0.982082103      2.657910922  0.8435123
    ##                  Pr(>|z|)
    ## (Intercept)  0.2701092286
    ## trtprogabide 0.6221812746
    ## base         0.0010665347
    ## age          0.5598563211
    ## V4           0.1473320265
    ## lbase        0.0002740114
    ## lage         0.3989419651
    ## 
    ## $`Rule of Thumb`
    ## [1] 2.823
    ## 
    ## $`GIM test statistic`
    ##          [,1]
    ## [1,] 1719.892
    ## 
    ## $`GIM pval`
    ## [1] 0.03225806

    # Quick rule of thumb for model misspecification; data clustered by subject
    GIM(pois, full = FALSE, cluster = epil$subject)

    ## [1] "Max ratio of robust to classic standard errors is 3.819"
    ## [1] "Rule of thumb suggests your model is misspecified, it is suggested that you run the full GIM test"

    ## $Coefficients
    ##                 Estimate   Std. Err. Robust Std. Err.    z value
    ## (Intercept)   3.05539782 0.981367261      3.747787934  0.8152537
    ## trtprogabide -0.05484456 0.050130822      0.161525833 -0.3395405
    ## base          0.01163038 0.001393269      0.004463859  2.6054535
    ## age          -0.05615889 0.034616587      0.129917414 -0.4322661
    ## V4           -0.15976960 0.054583706      0.066555042 -2.4005634
    ## lbase         0.63808395 0.076092125      0.231276515  2.7589656
    ## lage          2.24198067 0.982082103      3.643433766  0.6153483
    ##                 Pr(>|z|)
    ## (Intercept)  0.414927130
    ## trtprogabide 0.734202614
    ## base         0.009175273
    ## age          0.665548027
    ## V4           0.016369854
    ## lbase        0.005798464
    ## lage         0.538324745
    ## 
    ## $`Rule of Thumb`
    ## [1] 3.819

    # Full GIM test for model misspecification; data clustered by subject
    GIM(pois, full = TRUE, B = 30, B2 = 25, cluster = epil$subject)

    ## ===========================================================================[1] "omegaB"
    ##          [,1]
    ## [1,] 5943.136

    ## $Coefficients
    ##                 Estimate   Std. Err. Robust Std. Err.    z value
    ## (Intercept)   3.05539782 0.981367261      3.747787934  0.8152537
    ## trtprogabide -0.05484456 0.050130822      0.161525833 -0.3395405
    ## base          0.01163038 0.001393269      0.004463859  2.6054535
    ## age          -0.05615889 0.034616587      0.129917414 -0.4322661
    ## V4           -0.15976960 0.054583706      0.066555042 -2.4005634
    ## lbase         0.63808395 0.076092125      0.231276515  2.7589656
    ## lage          2.24198067 0.982082103      3.643433766  0.6153483
    ##                 Pr(>|z|)
    ## (Intercept)  0.414927130
    ## trtprogabide 0.734202614
    ## base         0.009175273
    ## age          0.665548027
    ## V4           0.016369854
    ## lbase        0.005798464
    ## lage         0.538324745
    ## 
    ## $`Rule of Thumb`
    ## [1] 3.819
    ## 
    ## $`GIM test statistic`
    ##          [,1]
    ## [1,] 5943.136
    ## 
    ## $`GIM pval`
    ## [1] 0.03225806
