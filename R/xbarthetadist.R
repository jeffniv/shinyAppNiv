#' xbarthetadist
#'
#' This function performs a bootstrap sample from the multivariate normal.
#'
#' @param n number of samples
#' @param iter number of bootstrap samples
#' @param mu mean vector
#' @param sigma covariance matrix
#'
#' @import mvtnorm
#' @import ggplot2
#'
#'

xbarthetadist = function(n,iter,mu,sigma){

    # library(mvtnorm)
    # library(ggplot2)
    mat = matrix(NA, nr= iter, nc=3)
    colnames(mat)= c("xbar1","xbar2","theta")
    for(i in 1:iter){
        x = rmvnorm(n,mu,sigma)
        mat[i,c(1,2)] <- colMeans(x)
        s=cov(x)
        eig=eigen(s)
        theta =  acos(eig$vectors[,1][1])
        mat[i,3]<-theta
    }

    df=as.data.frame(mat)
    g=ggplot(df, aes(x=xbar1,y=xbar2))  + coord_equal()
    a = ggplot(df, aes(x=theta))

    gp = g + geom_point()+labs(x=expression(bar(x)[1]),y=expression(bar(x)[2])) + geom_vline(aes(xintercept=mean(xbar1)), color='red',linetype='dashed') + geom_hline(aes(yintercept=mean(xbar2)), color='red',linetype='dashed')
    gd = g + stat_density2d(aes(fill = ..density..), contour = F, geom = 'tile') +  theme(legend.position="none") +labs(x=expression(bar(x)[1]),y=expression(bar(x)[2]))+ geom_vline(aes(xintercept=mean(xbar1)), color='red',linetype='dashed') + geom_hline(aes(yintercept=mean(xbar2)), color='red',linetype='dashed')
    ah = a + geom_histogram() +labs(x=expression(theta[12])) + geom_vline(aes(xintercept=mean(theta)), color='red',linetype='dashed')
    ad = a + geom_density(fill="cyan") + labs(x=expression(theta[12])) + geom_vline(aes(xintercept=mean(theta)), color='red',linetype='dashed')
    #mods: added mean lines, made labels nicer with expression operator

    multiplot(gp, gd, ah, ad, cols=2)
}
