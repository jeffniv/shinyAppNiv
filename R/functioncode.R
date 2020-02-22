
#' Title
#'
#' @param ...
#' @param plotlist
#' @param file
#' @param cols
#' @param layout
#'
#' @return
#' @export
#'

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

xbarthetadist = function(n,iter,mu,sigma){
  library(mvtnorm)
  library(ggplot2)
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



# end of function
