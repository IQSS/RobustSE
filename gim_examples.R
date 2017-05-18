# Examples for GIM function in RobustSE package

# install.packages("Ecdat")
library(Ecdat)
data(Fatality)

# ols modeling traffic fatality rate
ols <- glm(mrall ~ beertax + factor(year), data=Fatality,
           family = gaussian(link="identity"))

# Quick rule of thumb for model misspecification
GIM(ols, full = FALSE)

# Full GIM test for model misspecification
GIM(ols, full = TRUE, B = 30, B2 = 25)

# Quick rule of thumb for model misspecification; data clustered by state
GIM(ols, full = FALSE, cluster = Fatality$state)

# Full GIM test for model misspecification; data clustered by state
GIM(ols, full = TRUE, B = 30, B2 = 25, cluster = Fatality$state)

#=====================================================================
# install.packages("MASS")
library(MASS)
data(Pima.te)

logit <- glm(type ~ age + npreg + glu + bp + skin + bmi,
             data = Pima.te, family = binomial("logit"))

# Quick rule of thumb for model misspecification
GIM(logit, full = FALSE)

# Full GIM test for model misspecification
GIM(logit, full = TRUE, B = 30, B2 = 25)

# arbitrary cluster to test clustering
Pima.te$cluster <- as.factor(sample(1:20, nrow(Pima.te), replace = TRUE))

# Quick rule of thumb for model misspecification; data clustered by subject
GIM(logit, full = FALSE, cluster = Pima.te$cluster)

# Full GIM test for model misspecification; data clustered by subject
GIM(logit, full = TRUE, B = 30, B2 = 25, cluster = Pima.te$cluster)

#=====================================================================

probit <- glm(type ~ age + npreg + glu + bp + skin + bmi,
             data = Pima.te, family = binomial("probit"))

# Quick rule of thumb for model misspecification
GIM(probit, full = FALSE)

# Full GIM test for model misspecification
GIM(probit, full = TRUE, B = 30, B2 = 25)

# arbitrary cluster to test clustering
Pima.te$cluster <- as.factor(sample(1:20, nrow(Pima.te), replace = TRUE))

# Quick rule of thumb for model misspecification; data clustered by subject
GIM(probit, full = FALSE, cluster = Pima.te$cluster)

# Full GIM test for model misspecification; data clustered by subject
GIM(probit, full = TRUE, B = 30, B2 = 25, cluster = Pima.te$cluster)

#=====================================================================

# install.packages("MASS")
library(MASS)
data(epil)

# Poisson regression modeling seizure counts for epileptics
pois <- glm(y ~ trt + base + age + V4 + lbase + lage,
            data = epil, family = "poisson")

# Quick rule of thumb for model misspecification
GIM(pois, full = FALSE)

# Full GIM test for model misspecification
GIM(pois, full = TRUE, B = 30, B2 = 25)

# Quick rule of thumb for model misspecification; data clustered by subject
GIM(pois, full = FALSE, cluster = epil$subject)

# Full GIM test for model misspecification; data clustered by subject
GIM(pois, full = TRUE, B = 30, B2 = 25, cluster = epil$subject)

#=========================================================================

# Negative binomial regression modeling seizure counts for epileptics
neg.bin <- glm.nb(y ~ trt + base + age + V4 + lbase + lage,
                  data = epil)

# Quick rule of thumb for model misspecification
GIM(neg.bin, full = FALSE)

# Full GIM test for model misspecification
GIM(neg.bin, full = TRUE, B = 30, B2 = 25)

# Quick rule of thumb for model misspecification; data clustered by subject
GIM(neg.bin, full = FALSE, cluster = epil$subject)

# Full GIM test for model misspecification; data clustered by subject
GIM(neg.bin, full = TRUE, B = 30, B2 = 25, cluster = epil$subject)
