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
(nodes <- get_nodes(reg_df, split_vars = c("Z1", "Z2", "Z3"), 
          formula = "Ytilde ~ X2 + X3", verbose = TRUE, max_steps = 10, min_obs = 20, pval = 0.001))

l2df <- function(l, ...){
  return(data.frame(matrix(unlist(l), ...), stringsAsFactors = FALSE))
}

node_summary <- function(node, grp){
  nod <- l2df(node$node, ncol = 3, byrow=TRUE)
  #print(nod)
  grp <- l2df(grp, ncol = 1)
  colnames(grp) <- 'direction'
  grp[nrow(grp),] <- 'terminal'
  colnames(nod) <- names(node$node[[1]])
  rownames(nod) <- paste("level: ", rownames(nod))
  nod <- data.frame(cbind(nod, grp))
  
  return(nod)
}

simplify_nodes <- function(nodes, level = 1, grp = NULL){
  leq <- nodes[[1]]
  gre <- nodes[[2]]
  
  if(is.null(grp)){
    grpx <- "gre"
  }else{
    grpx <- list(grp, "gre")
  }
  if(is.null(grp)){
    grpy <- "leq"
  }else{
    grpy <- list(grp, "leq")
  }
  
  term_leq <- !is.null(names(leq))
  term_gre <- !is.null(names(gre))
  
  if(term_leq & term_gre){
    node_summary(leq, grp = grpy)
    node_summary(gre, grp = grpx)
  }else if(term_leq){
    node_summary(leq, grp = grpy)
    levelx <- level + 1
    Recall(gre, level = levelx, grp = grpx)
  }else if(term_gre){
    node_summary(gre, grp = grpx)
    levelx <- level + 1
    Recall(leq, level = levelx, grp = grpy)
  }else{
    levelx <- level + 1
    return(list(Recall(leq, level = levelx, grp = grpy), 
                Recall(gre, level = levelx, grp = grpx)))
  }
}
untree <- function(nodes, simplify = FALSE){
  out <- list()
  lumberjack <- function(nodes){
    leq <- nodes[[1]]
    gre <- nodes[[2]]
    term_leq <- !is.null(names(leq))
    term_gre <- !is.null(names(gre))
    parent <- parent.frame()
    pos <- length(parent$out) + 1
    if(term_leq & term_gre){
      parent$out[[pos]] <- leq
      parent$out[[pos + 1]] <- gre
      print('both')
    }else if(term_leq){
      parent$out[[pos]] <- leq
      Recall(gre)
      print('leq')
    }else if(term_gre){
      parent$out[[pos]] <- gre
      Recall(leq)
      print("gre")
    }else{
      print('none')
      Recall(leq)
      Recall(gre)
    }
  }
  lumberjack(nodes)
  if(simplify){
    out <- do.call("rbind", out)
  }
  return(out)
}

simnodes <- simplify_nodes(nodes)
unnodes <- untree(simnodes, FALSE)

make_plan <- function(nodes){
  require(stringr)
  plan_node <- function(node){
    combine_node <- function(node_row){
      dir <- ifelse(node_row[4] == 'leq', '<=', '>')
      condition <- paste(node_row[2], dir, node_row[3], collapse = '')
      return(condition)
    }
    apply(node, 1, combine_node)
  }
  plan <- lapply(nodes, plan_node)
  print(plan)
  return(plan)
}

add_terminal <- function(plan){
  plan <- rbind(plan, stringr::str_replace(plan[nrow(plan),], ">", "<="))
  return(plan)
}


plantt <- make_plan(unnodes)

cumpaste <- function(vec){
  sapply(vec, function(x)paste(vec[1:which(vec == x)], collapse = ' & '))
  
}
cumpaste(plantt[[1]])



tree <- plant_tree(nodes, lm, formula = "Ytilde ~ X2 + X3")



print.node <- function(node, level, grp){
  require(igraph)
  #indent_print(node$coefs$coefficients, .indent = ifelse(level == 1, "", paste0(paste0(rep("-", level), collapse = ''), "  ", collapse = '')))
  nod <- l2df(node$nodes, ncol = 3, byrow = TRUE)
  grp <- l2df(grp, ncol = 1)
  colnames(nod) <- names(node$nodes[[1]])
  rownames(nod) <- paste("level: ", rownames(nod))
  nod <- cbind(nod, grp)
  return(nod)
  #indent_print(nod, .indent = ifelse(level == 1, "", paste0(paste0(rep("-", level), collapse = ''), "  ", collapse='')))
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

summary.tree <- function(tree, level = 1, grp = NULL){
  cat("\n")
  cat(ifelse(level == 1, "Root", paste("Split", level)))
  cat("\n\n")
  leq <- tree[[1]]
  gre <- tree[[2]]
  
  if(is.null(grp)){
    grpx <- "gre"
  }else{
    grpx <- list(grp, "gre")
  }
  if(is.null(grp)){
    grpy <- "leq"
  }else{
    grpy <- list(grp, "leq")
  }
  
  term_leq <- !is.null(names(leq))
  term_gre <- !is.null(names(gre))
  if(term_leq & term_gre){
    cat("<= ", get_last(leq, level),'\n' )
    print.node(leq, level, grp = grpy)
    cat("\n")
    cat("> ", get_last(gre, level),'\n' )
    print.node(gre, level, grp = grpx)
  }else if(term_leq){
    cat("<= ", get_last(leq, level),'\n' )
    print.node(leq, level, grp = grpy)
    cat('\n', "> ", get_last(leq, level),":")
    levelx <- level + 1
    Recall(gre, level = levelx, grp = grpx)
  }else if(term_gre){
    cat("> ", get_last(gre, level),'\n' )
    print.node(gre, level = level, grp = grpx)
    levelx <- level + 1
    Recall(leq, level = levelx, grp = grpy)
  }else{
    levelx <- level + 1
    return(list(Recall(leq, level = levelx, grp = grpy), 
                Recall(gre, level = levelx, grp = grpx)))
  }
}

summary.tree(tree)

library(partykit)
lmtree(Ytilde ~ X2 + X3 | Z1 + Z2 + Z3, data = reg_df, minsize = 30)

