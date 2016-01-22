# Population values are just 1 to 9999

population <- 1:9999
N <- length(population)
(mu <- mean(population))

(sigma2 <- sum((population - mu)^2)/N)

# Sample size
n <- 100

# Simple random sample
srs <- sample(population, size = n, replace = FALSE)

# sample average
(y_bar <- mean(srs))

# theoretical variance of the sample average
var_y_bar <- sigma2/n*(N-n)/(N-1)

# To investigate distribution of y_bar let's simulate this sampling procedure. k
# is the number of replications and has no effect on the theory - but a larger k
# does make for nicer plots.

k <- 2000
srs_replicated <- replicate(k, sample(population, size = n, replace = FALSE))

# This gives a matrix with n=100 rows and k=2000 columns. The distribution of
# y_bar can be investigated empirically using the 2000 column means

y_bar_replicated <- apply(srs_replicated, 2, mean)

# Several ways to investigate the distribution. An obvious one is with a
# histogram.
hist(y_bar_replicated, nclass = 50)

# A variation on histogram is a so-called "density plot" which can be nicer to
# look at.
plot(density(y_bar_replicated))

## Let's add the correct theoretical normal density to assess actual normality
lines(4000:6000, dnorm(4000:6000, mu, sqrt(var_y_bar)), col="red")

# A normal quantile plot is a accurate way to assess normality.
qqnorm(y_bar_replicated)

# So everything's great! Let's increase n...

n <- 9000
var_y_bar <- sigma2/n*(N-n)/(N-1)
srs_replicated <- replicate(k, sample(population, size = n, replace = FALSE))
y_bar_replicated <- apply(srs_replicated, 2, mean)
hist(y_bar_replicated, nclass = 50, probability = TRUE)
x <- seq(mu - 10*var_y_bar, mu + 10*var_y_bar, length.out = 5000)
lines(x, dnorm(x, mu, sqrt(var_y_bar)), col="red")

# A normal quantile plot is a accurate way to assess normality.
qqnorm(y_bar_replicated)

