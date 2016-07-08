#' Make a random plot
#' 
#' This function creates a random histogram plot.
#' 
#' @export
#' @param n numer of random values 
#' @param dist one of "normal" or "uniform".
randomplot1 <- function(n, dist=c("normal", "uniform")){
  #input validation
  dist <- match.arg(dist)
  stopifnot(n < 1e6)
  
  if(dist == "normal"){
    #plot(rnorm(n), col="red")
    hist(rnorm(n))
    #plot(lm(mpg~disp, data=mtcars))
    #test
  }
  
  if(dist == "uniform"){
    #plot(rnorm(n), col="green")
    hist(runif(n))
    #plot(lm(mpg~disp, data=mtcars))
  }

  #return nothing
  invisible();  
}

randomplot <- function(n, dist=c("normal", "uniform")){
  #input validation
  dist <- match.arg(dist)
  stopifnot(n < 1e6)
  
  if(dist == "normal"){
    library(plotly)
    set.seed(100)
    d <- diamonds[sample(nrow(diamonds), 1000), ]
    plotlyOutput(plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
            mode = "markers", color = carat, size = carat))
    #hist(rnorm(n))
    #plot(lm(mpg~disp, data=mtcars))
  }
  
  if(dist == "uniform"){
    library(ggplot2)
    dat <- data.frame(cond = rep(c("A", "B"), each=10),
                  xvar = 1:20 + rnorm(20,sd=3),
                  yvar = 1:20 + rnorm(20,sd=3))
                  
    ggplot(dat, aes(x=xvar, y=yvar)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth()            # Add a loess smoothed fit curve with confidence region
    
    
    #plot(rnorm(n), col="green")
    #hist(runif(n))
    #plot(lm(mpg~disp, data=mtcars))
  }
  
  #return nothing
  #invisible();  
}
