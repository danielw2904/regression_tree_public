n <- 500
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
Z1 <- sample(1:10, n, replace = TRUE)
Z2 <- sample(1:10, n, replace = TRUE)
Z3 <- sample(1:10, n, replace = TRUE)
RHO = -.5
BETA <- c(5, -2 , 2.5)
SIGMA = 0.03
Y <- solve(diag(n) - RHO * W) %*% (X %*%  BETA + ifelse(Z1<=3, 5 * X[, 1], 0) + 
                                     ifelse(Z2<=7, 3 * X[,3], 0) + 
                                     ifelse(Z3<=4, 8 * X[,2], 0) +
                                     rnorm(n, mean = 0,sd = SIGMA) )
library(spdep)
w.list <- mat2listw(W, style = 'W')
reg_df <- data.frame(Y = as.matrix(Y), X, Z1, Z2, Z3)
reg_df$X1Z1 <- reg_df$X1 * as.integer(Z1<=3)
reg_df$X3Z2 <- reg_df$X3 * as.integer(Z2<=7)
reg_df$X2Z3 <- reg_df$X2 * as.integer(Z3<=4)
mod_true <- lagsarlm(Y ~ X2 + X3 + X1Z1 + X3Z2 + X2Z3, data = reg_df, listw = w.list)

mod_filter <- lagsarlm(Y ~ X2 + X3, data = reg_df, listw = w.list)

reg_df$Ytilde <- reg_df$Y - coef(mod_filter)[1] * W %*% reg_df$Y

summary(mod_true)

source("reg_tree/8_model-fun.R")
(nodes <- get_nodes(reg_df, split_vars = c("Z1", "Z2"), 
          formula = "Ytilde ~ X2 + X3", verbose = TRUE, max_steps = 10, min_obs = 20, pval = 0.001))


tree <- plant_tree(nodes, lm, formula = "Ytilde ~ X2 + X3")


print.node <- function(node, level){
  require(igraph)
  indent_print(node$coefs$coefficients, .indent = ifelse(level == 1, "", paste0(paste0(rep("-", level), collapse = ''), "  ", collapse = '')))
  nod <- data.frame(matrix(unlist(node$nodes), ncol = 3, byrow = TRUE))
  colnames(nod) <- names(node$nodes[[1]])
  rownames(nod) <- paste("level: ", rownames(nod))
  indent_print(nod, .indent = ifelse(level == 1, "", paste0(paste0(rep("-", level), collapse = ''), "  ", collapse='')))
}

get_last <- function(node, level){
  if(is.null(names(node))){
    return(Recall(node[[1]]))
  }
  if(is.null(names(node$nodes))){
    ret <- node$nodes[[2]]
    ret <- ret$value
  }else{
    ret <- node$nodes$value
  }
  return(ret)
}

summary.tree <- function(tree, level = 1){
  cat("\n")
  cat(ifelse(level == 1, "Root", paste("Split", level)))
  cat("\n\n")
  leq <- tree[[1]]
  gre <- tree[[2]]
  
  term_leq <- !is.null(names(leq))
  term_gre <- !is.null(names(gre))
  if(term_leq & term_gre){
    cat("<= ", get_last(leq, level),'\n' )
    print.node(leq, level)
    cat("\n")
    cat("> ", get_last(gre, level),'\n' )
    print.node(gre, level)
  }else if(term_leq){
    cat("<= ", get_last(leq, level),'\n' )
    print.node(leq, level)
    cat('\n', "> ", get_last(leq, level),":")
    levelx <- level + 1
    Recall(gre, level = levelx)
  }else if(term_gre){
    cat("> ", get_last(gre, level),'\n' )
    print.node(gre, level = level)
    levelx <- level + 1
    Recall(leq, level = levelx)
  }else{
    levelx <- level + 1
    return(list(Recall(leq, level = levelx), 
                Recall(gre, level = levelx)))
  }
}

summary.tree(tree)

library(partykit)
lmtree(Ytilde ~ X2 + X3 | Z1 + Z2, data = reg_df, minsize = 30)

