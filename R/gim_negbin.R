bootstrapIM.negbin <- function(lm1, B, B2, cluster=NA, time=NA)
{
    X <- model.matrix(lm1)
    y <- unname(lm1$y)
    data <- data.frame(lm1$y,model.matrix(lm1)[,-1])
    names(data)[which(names(data) == "lm1.y")] <- colnames(lm1$model)[1]
    ok <- !is.na(lm1$coefficients)
    X <- X[,ok]
    beta <- lm1$coefficients[ok]

    lambda <- exp(X%*%beta)
    alpha <- lm1$theta

    if(length(cluster) < 2){
        grad <- sandwich::estfun(lm1)
        meat <- t(grad)%*%grad
        bread <- -solve(vcov(lm1))
        Dhat <- diag(nrow(X)^(-1/2)*(meat + bread))
    }

    if(length(cluster)>=2){
        meat <- meat.clust(lm1, cluster)
        bread <- -solve(vcov(lm1))
        Dhat <- diag(nrow(X)^(-1/2)*(meat + bread))

    }

    D <- list()
    Dbar <- rep(0, length(Dhat))

    pb = txtProgressBar(min = 1, max = B, initial = 1)

    for(i in 1:B){
        yB <- rnegbin(nrow(data), lambda, alpha)
        XB <- X[!is.na(yB),]
        yB <- yB[!is.na(yB)]
        lm1B <- glm.nb(yB ~ model.matrix(lm1)[,-1])

        if(length(cluster)<2){
            grad <- sandwich::estfun(lm1B)
            meat <- t(grad)%*%grad
            bread <- -solve(vcov(lm1B))
            D[[i]] <- diag(nrow(X)^(-1/2)*(meat + bread))
        }

        if(length(cluster)>=2){
            meat <- meat.clust(lm1B, cluster)
            bread <- -solve(vcov(lm1B))
            D[[i]] <- diag(nrow(X)^(-1/2)*(meat + bread))
        }

        Dbar <- D[[i]] + Dbar
        DBbar <- rep(0, length(Dhat))
        DB <- list()

        for(j in 1:B2){
            yB2 <- rnegbin(nrow(data), lambda, alpha)
            XB2 <- X[!is.na(yB2),]
            yB2 <- yB2[!is.na(yB2)]
            lm1B2 <- glm.nb(yB2 ~ model.matrix(lm1)[,-1])

            if(length(cluster)<2){
                grad <- sandwich::estfun(lm1B2)
                meat <- t(grad)%*%grad
                bread <- -solve(vcov(lm1B2))
                DB[[j]] <- diag(nrow(X)^(-1/2)*(meat + bread))
            }

            if(length(cluster)>=2){
                meat <- meat.clust(lm1B2, cluster)
                bread <- -solve(vcov(lm1B2))
                DB[[j]] <- diag(nrow(X)^(-1/2)*(meat + bread))
            }

            DBbar <- DB[[j]] + DBbar
        }

        DBbar <- DBbar/B2
        VBb <- matrix(0, nrow=length(DBbar), ncol=length(DBbar))
        for(j in 1:B2){
            VBb <- VBb + (DB[[j]] - DBbar)%*%t(DB[[j]]-DBbar)
        }
        VBb <- VBb/(B2-1)
        #invVBb <- invcov.shrink(VBb)
        invVBb <- MASS::ginv(VBb)
        T[i] <- t(D[[i]])%*%invVBb%*%D[[i]]
        #print(i)
        #print(T[i])
        #if(i%%10==0) print(i)
        setTxtProgressBar(pb,i)
    }

    Dbar <- Dbar/B

    Vb <- matrix(0, nrow=length(Dbar), ncol=length(Dbar))
    for(i in 1:B){
        Vb <- Vb + (D[[i]] - Dbar)%*%t(D[[i]]-Dbar)
    }

    Vb <- Vb/(B-1)

    #T <- NULL
    invVb <- MASS::ginv(Vb)

    omegaB <- t(Dhat)%*%invVb%*%Dhat
    print("omegaB")
    print(omegaB)
    pb = (B+1-sum(T< as.numeric(omegaB)))/(B+1)

    return(list(stat=omegaB, pval=pb))
}
