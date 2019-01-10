n <- 100
W <- matrix(rgamma(n^2, shape = 10, 10),  nrow = n, ncol = n)
W[lower.tri(W)] <- 0
W <- W %*% t(W)
diag(W) <- 0
W

## Row standardizes a matrix
row_stdz <- function(W){
  sums <- rowSums(W)
  return(W / sums)
}
(W <- row_stdz(W))
set.seed(123)
X <- cbind( 1, rnorm(n), rnorm(n) )
Z1 <- sample(1:10, n, replace = T)
Z2 <- sample(1:10, n, replace = TRUE)
RHO = -.5
BETA <- c(5, -2 , 2.5)
SIGMA = 0.03
Y <- solve(diag(n) - RHO * W) %*% (X %*%  BETA + ifelse(Z1<=3, 5 * X[, 1], 0) + 
                                     ifelse(Z2<=7, 3 * X[,3], 0) + rnorm(n, mean = 0,sd = SIGMA) )
library(spdep)
w.list <- mat2listw(W, style = 'W')
reg_df <- data.frame(Y = as.matrix(Y), X, Z1, Z2)
reg_df$X1Z1 <- reg_df$X1 * as.integer(Z1<=3)
reg_df$X3Z2 <- reg_df$X3 * as.integer(Z2<=7)
mod_true <- lagsarlm(Y ~ X2 + X3 + X1Z1 + X3Z2, data = reg_df, listw = w.list)

mod_filter <- lagsarlm(Y ~ X2 + X3, data = reg_df, listw = w.list)

reg_df$Ytilde <- reg_df$Y - coef(mod_filter)[1] * W %*% reg_df$Y

summary(mod_true)

source("reg_tree/8_model-fun.R")
(nodes <- get_nodes(reg_df, split_vars = c("Z1", "Z2"), 
          formula = "Ytilde ~ X2 + X3", verbose = TRUE, max_steps = 10, min_obs = 20, pval = 0.001))

plant_tree(nodes, lm, formula = "Ytilde ~ X2 + X3")

library(partykit)
lmtree(Ytilde ~ X2 + X3 | Z1 + Z2, data = reg_df, minsize = 30)
