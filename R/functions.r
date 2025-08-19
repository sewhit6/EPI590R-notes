x <- c(1, 3, 5, 7, 9)
# know the mean is 5


n <- length(x)
mean <- sum(x) / n
print(mean)

# Writing a function
new_mean <- function(x) {
	n <- length(x)
	sum(x) / n
