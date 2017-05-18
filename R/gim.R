#' GIM
#'
#' Takes in glm output, performs GIM test, returns estimated coefficients,
#'   classic standard errors, robust standard errors, rule of thumb for
#'   misspecified model, GIM test statistic and p-value.
#'
#' @name GIM
#' @param out : Output from glm function
#' @param full : TRUE if you want to perform the full GIM test, FALSE if you want to
#' quickly check if your model is misspecified
#' @param B : Number of new datasets to test
#' @param B2 : Number of bootstraps for each new dataset
#' @param cluster : For clustered data
#' @param time : For time series data
#' @export

GIM <- function(out, full=TRUE, B, B2, cluster = NA, time = NA){
    if(full==TRUE){
        if(family(out)$family == 'gaussian')
        {
            gim.out <- bootstrapIM.gaussian(out, B, B2, cluster=cluster, time=time)
        }
        else if(family(out)$family == 'binomial' & family(out)$link == "logit")
        {
            gim.out <- bootstrapIM.logit(out, B, B2, cluster=cluster, time=time)
        }
        else if(family(out)$family == 'binomial' & family(out)$link == "probit")
        {
            gim.out <- bootstrapIM.probit(out, B, B2, cluster=cluster, time=time)
        }
        else if(family(out)$family == 'poisson')
        {
            gim.out <- bootstrapIM.poisson(out, B, B2, cluster=cluster, time=time)
        }
        else if(grepl("Negative Binomial", family(out)$family) == TRUE)
        {
            gim.out <- bootstrapIM.negbin(out, B, B2, cluster=cluster, time=time)
        }
    }

    if(length(cluster)<2 & length(time)<2){
        coefs <- out$coefficients
        classic.se <- sqrt(diag(vcov(out)))
        robust.model <- as.matrix(lmtest::coeftest(out, vcov=sandwich::vcovHC))
        robust.se <- robust.model[,2]
        ests <- matrix(c(coefs,classic.se,robust.se,robust.model[,3],robust.model[,4]),ncol=5)
        rownames(ests) <- names(coefs)
        colnames(ests) <- c("Estimate", "Std. Err.", "Robust Std. Err.","z value","Pr(>|z|)")
        ROT <- max(robust.se/classic.se)
    }

    if(length(cluster)>=2){
        coefs <- out$coefficients
        classic.se <- sqrt(diag(vcov(out)))
        clust.vcov <- clust.robust(out, cluster)
        robust.model <- as.matrix(lmtest::coeftest(out, vcov=clust.vcov))
        robust.se <- robust.model[,2]
        ests <- matrix(c(coefs,classic.se,robust.se,robust.model[,3],robust.model[,4]),ncol=5)
        rownames(ests) <- names(coefs)
        colnames(ests) <- c("Estimate", "Std. Err.", "Robust Std. Err.","z value","Pr(>|z|)")
        ROT <- max(robust.se/classic.se)
    }


    if(exists("gim.out") == FALSE){
        if(ROT >= 1.5){
            print(paste("Max ratio of robust to classic standard errors is", round(ROT,3),sep = " "))
            print("Rule of thumb suggests your model is misspecified, it is suggested that you run the full GIM test")
        }

        else{
            print(paste("Max ratio of robust to classic standard errors is", round(ROT,3),sep = " "))
            print("Rule of thumb suggests your model is NOT misspecified")
        }

        return(list("Coefficients" = ests, "Rule of Thumb" = round(ROT,3)))
    }

    else{
        return(list("Coefficients" = ests, "Rule of Thumb" = round(ROT,3), "GIM test statistic" = gim.out$stat, "GIM pval" = gim.out$pval))
    }

}


