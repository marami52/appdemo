bostonhist <- function(variable){
	library(ggplot2)
	#ggplot(Boston, aes(variable)) + geom_histogram()
	hist(Boston[[variable]], xlab=variable, main="Example plot with lazyload data")
}
