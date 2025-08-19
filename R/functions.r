
# Writing a function (only thing users need to provide is x, so that is the only argument in the function)
new_mean <- function(x) {
	n <- length(x)
	mean_value <- sum(x) / n
	return(mean_value)
}

new_mean(x)
y<- c(34563, 234, 2352, 7457, 865, 2534)

new_mean(y)

# start out with a number to test
z <- 3

# you'll want your function to return this number
x^2

square <- function(z) {
	squared_num <- z^2
	return(squared_num)
}
square(z)
square(53)

add_two_numbers <- function(w, r) {
	summed_num <- w+r
	sentence <- paste("The result is", summed_num)
	return(sentence)
}
add_two_numbers(w=4, r=6)
add_two_numbers(w=10,r=5)
add_two_numbers(w=1234, r=9876)

# raising to a power, if not specifying, defaulting to 2
raise <- function(t, powers=2) {
	powered_num <- t^powers
	return(powered_num)
}
raise(3, 3)
raise(5)
raise(2, 6)
raise(10)


# seting vector of numbers
vector <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
vector2 <- c(12,22,23,42,25,26,29)
# calculating sd
xbar <- mean(vector) #xbar
numerator <- (vector-xbar)^2
numerator
n <- length(vector)
denominator <- n-1
denominator
variance <- sum(numerator) / denominator
sd <- sqrt(variance)
sd

std_dev<- function(x) {
	xbar <- mean(x) #xbar
	numerator <- (x-xbar)^2
	numerator
	n <- length(x)
	denominator <- n-1
	denominator
	variance <- sum(numerator) / denominator
	sd <- sqrt(variance)
	return(sd)
}

std_dev(x=vector)
std_dev(x=vector2)

#modifying function to remove NA values before calculating std dev
std_dev<- function(x, na.rm=TRUE) {
	if (na.rm == TRUE) {
		x <- na.omit(x)}
	xbar <- mean(x)
	numerator <- (x-xbar)^2
	numerator
	n <- length(x)
	if (length(x) <= 1)
	{return(NA)}
	else
	denominator <- n-1
	denominator
	variance <- sum(numerator) / denominator
	sd <- sqrt(variance)
	return(sd)
}

std_dev(x = nlsy$income)
std_dev(x = nlsy$income, na.rm=FALSE)
vector_less_1 <-  (c(2))
std_dev(x=vector_less_1)
vector_na <- c(NA, 1, 2, 33, 4, NA, 12, 42)
std_dev(x=vector_na, na.rm = FALSE)
