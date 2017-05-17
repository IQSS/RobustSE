
# normal log likelihood for GIM test
ll.normal.bsIM <- function(par,y,X, sigma){
    beta <- par[1:length(X)]
    sigma2 <- sigma
    -1/2 * (sum(log(sigma2) + (y -(X%*%beta))^2/sigma2))
}

bootstrapIM.gaussian <- function(lm1, B, B2, cluster=NA, time=NA)
{
    X <- model.matrix(lm1)
    y <- unname(lm1$y)
    data <- data.frame(lm1$y, model.matrix(lm1)[,-c(1)])
    names(data)[which(names(data) == "lm1.y")] <- as.character(lm1$formula[[2]])
    ok <- !is.na(lm1$coefficients)
    X <- X[,ok]
    beta <- lm1$coefficients[ok]

    mu <- X%*%beta
    sigma <- sum(lm1$residuals^2)/(nrow(model.matrix(lm1))-ncol(model.matrix(lm1)))
    grad <- apply(cbind(y,model.matrix(lm1)),1,function(x) maxLik::numericGradient(ll.normal.bsIM, beta, y=x[1], X=x[2:length(x)], sigma=sigma))

    if(length(cluster)<2 & length(time)<2){
        meat <- grad%*%t(grad)
        bread <- -solve(vcov(lm1))
        matri <- nrow(X)^(-1/2)*(meat + bread)
        Dhat <- as.vector(matri[lower.tri(matri, diag = TRUE)])
        #Dhat <- as.vector(diag(matri))
        #Verify robust se is calculated correctly
        #print(paste("First", diag(solve(bread)%*%meat%*%solve(bread))))
        #print(paste("Second", diag(vcovHC(lm1, type="HC0"))))
    }

    if(length(cluster)>=2){

        bread <- -solve(vcov(lm1))
        meat <- meat.clust(lm1, cluster)
        matri <- nrow(X)^(-1/2)*(meat + bread)
        Dhat <- as.vector(matri[lower.tri(matri, diag = TRUE)])

    }

    if(length(time)>1){

        bread <- -solve(vcov(lm1))
        bread2 <- t(X)%*%X
        breadratio <- -bread/bread2
        plm1 <- plm(formula, data, model="within")
        meat <- vcovSCCchange(plm1)*breadratio[1,1]^2
        print(dim(bread))
        matri <- (nrow(X)^(-1/2))*(meat + bread[2:nrow(bread),2:nrow(bread)])
        Dhat <- diag(matri)
        #Verify robust se is calculated correctly
        #print(paste("First", diag(solve(bread)[2:nrow(bread),2:nrow(bread)]%*%meat%*%solve(bread)[2:nrow(bread),2:nrow(bread)])))
        #print(paste("Second", diag(vcovSCC(plm1))))
    }

    D <- list()
    Dbar <- rep(0, length(Dhat))

    for(i in 1:B){

        yB <- rnorm(nrow(data), mu, sqrt(sigma))
        lm1B <- lm(yB ~ model.matrix(lm1)[,-1])
        muB <- X%*%lm1B$coefficients
        sigmaB <- sum(lm1B$residuals^2)/(nrow(model.matrix(lm1B))-ncol(model.matrix(lm1B)))
        grad <- apply(cbind(yB,model.matrix(lm1B)),1,function(x) maxLik::numericGradient(ll.normal.bsIM, lm1B$coefficients, y=x[1], X=x[2:length(x)], sigma=sigmaB))

        if(length(cluster)<2 & length(time)<2){

            meat <- grad%*%t(grad)
            bread <- -solve(vcov(lm1B))
            #D[[i]] <- diag(nrow(X)^(-1/2)*(meat + bread))
            matri <- nrow(X)^(-1/2)*(meat + bread)
            D[[i]]  <- as.vector(matri[lower.tri(matri, diag = TRUE)])
        }

        if(length(cluster)>=2){

            bread <- -solve(vcov(lm1B))
            meat <- meat.clust(lm1B, cluster)
            matri <- nrow(X)^(-1/2)*(meat + bread)
            D[[i]] <- as.vector(matri[lower.tri(matri, diag = TRUE)])


        }

        if(length(time)>1){

            bread <- -solve(vcov(lm1B))
            data$yB <- yB
            bread2 <- t(X)%*%X
            breadratio <- -bread/bread2
            plm1B <- plm(yB ~ model.matrix(lm1), data, model="within")
            meat <- vcovSCCchange(plm1B)*breadratio[1,1]^2
            matri <- (nrow(X)^(-1/2))*(meat + bread[2:nrow(bread),2:nrow(bread)])
            D[[i]] <- diag(matri)
        }

        Dbar <- D[[i]] + Dbar

        DBbar <- rep(0, length(Dhat))
        DB <- list()

        #Bootstrap for VB of B
        for(j in 1:B2){

            yB2 <- rnorm(nrow(data), muB, sqrt(sigmaB))
            lm1B2 <- lm(yB2 ~ model.matrix(lm1)[,-1])
            sigmaB2 <- sum(lm1B2$residuals^2)/(nrow(model.matrix(lm1B2))-ncol(model.matrix(lm1B2)))
            grad <- apply(cbind(yB2,model.matrix(lm1B2)),1,function(x) maxLik::numericGradient(ll.normal.bsIM, lm1B2$coefficients, y=x[1], X=x[2:length(x)], sigma=sigmaB2))

            if(length(cluster)<2 & length(time)<2){
                meat <- grad%*%t(grad)
                bread <- -solve(vcov(lm1B2))
                matri <- nrow(X)^(-1/2)*(meat + bread)
                DB[[j]] <- as.vector(matri[lower.tri(matri, diag = TRUE)])
                #DB[[j]] <- diag(nrow(X)^(-1/2)*(meat + bread))
            }

            if(length(cluster)>=2){

                bread <- -solve(vcov(lm1B2))
                meat <- meat.clust(lm1B2, cluster)
                matri <- nrow(X)^(-1/2)*(meat + bread)
                DB[[j]] <- as.vector(matri[lower.tri(matri, diag = TRUE)])


            }

            if(length(time)>1){
                bread <- -solve(vcov(lm1B2))
                data$yB2 <- yB2
                bread2 <- t(X)%*%X
                breadratio <- -bread/bread2
                plm1B2 <- plm(yB2 ~ model.matrix(lm1), data, model="within")
                meat <- vcovSCCchange(plm1B2)*breadratio[1,1]^2
                matri <- (nrow(X)^(-1/2))*(meat + bread[2:nrow(bread),2:nrow(bread)])
                DB[[j]] <- as.vector(lower.tri(matri, diag = TRUE))
                #DB[[j]] <- diag(matri)
            }

            DBbar <- DB[[j]] + DBbar
        }

        DBbar <- DBbar/B2
        VBb <- matrix(0, nrow=length(DBbar), ncol=length(DBbar))
        for(j in 1:B2){
            VBb <- VBb + (DB[[j]] - DBbar)%*%t(DB[[j]]-DBbar)
        }

        VBb <- VBb/(B2-1)
        invVBb <- MASS::ginv(VBb)
        T[i] <- t(D[[i]])%*%invVBb%*%D[[i]]
        print(i)
        print(T[i])
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
