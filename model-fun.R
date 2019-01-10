# Have we reached a terminal node
enough_obs <- function(df_list, min_obs) {
  leq_works <- nrow(df_list$leq) > min_obs
  gre_works <- nrow(df_list$gre) > min_obs
  return(leq_works && gre_works)
}

# Split data
splitter <- function(df, split_var, split_val) {
  split_data <- df[[split_var]]
  sel_low <- which(split_data <= split_val)
  sel_high <- which(split_data > split_val)
  df_low  <- df[sel_low, ]
  df_high <- df[sel_high, ]
  return(list(leq = df_low, gre = df_high))
}

# Return model's coefficients
coefs <- function(df_list, mod, fun, ...) {
  mod_leq <- fun(mod, data = df_list$leq, ...)
  mod_gre <- fun(mod, data = df_list$gre, ...)
  cov_leq <- vcov(mod_leq)
  cov_gre <- vcov(mod_gre)
  return(list(coef_leq = coef(mod_leq), 
              coef_gre = coef(mod_gre), 
              cov_leq = cov_leq, 
              cov_gre = cov_gre))
}

# What's the p-value
obj_fun <- function(model_list) {
  leq <- as.matrix(model_list$coef_leq)
  gre <- as.matrix(model_list$coef_gre)
  covs <- as.matrix(model_list$cov_leq) + as.matrix(model_list$cov_gre)
  stat <- t((leq - gre)) %*% solve(covs) %*% (leq - gre)
  pval <- dchisq(stat, df = length(leq))
  return(list(stat = stat, p_val = pval))
}

# Takes in df & looks for nice splits, returns the best
find_split <- function(
  df, split_vars, formula, 
  fun = lm, predictors = 3, 
  min_obs = 3, n_splits = 10, ...) {
  
  best_vals <- matrix(NA, nrow = length(split_vars), ncol = 2)
  rownames(best_vals) <- split_vars
  colnames(best_vals) <- c("var_value", "chisq_value")

  j <- 1
  for(var in split_vars) {
    var_stat <- vector("double", length = length(split_vars))
    i <- 1
    split_vals <- seq(min(df[[var]]), max(df[[var]]), length.out = n_splits)
    for(z in split_vals) {
      df_list <- splitter(df, var, z)
      if(enough_obs(df_list, min_obs = min_obs)) {
        res <- obj_fun(coefs(df_list, formula, fun, ...))
        var_stat[i] <- as.numeric(res$stat)
      } else {
        var_stat[i] <- NA
      }
      i <- i + 1
    } # for(z in split_vals)
    if(all(is.na(var_stat))) {
      best_vals[j, 1] <- NA
      best_vals[j, 2] <- NA
    } else {
      best_vals[j, 1] <- split_vals[which(var_stat == max(var_stat, na.rm = TRUE))][1]
      best_vals[j, 2] <- max(var_stat, na.rm = TRUE)
    }
    j <- j + 1
  } # for(var in split_vars)
  
  split <- list()
  if(all(is.na(best_vals[, 2]))) {
    split$pval <- Inf
    return(split)
  }
  best_split <- which(best_vals[, 2] == max(best_vals[, 2], na.rm = TRUE))[1]
  split$pval <- dchisq(best_vals[best_split, 2], df = predictors)
  split$name <- rownames(best_vals)[best_split]
  split$value <- best_vals[best_split, 1]

  return(split)
}

# Finds splits until min_obs, max_steps or pval is breached
get_nodes <- function(
  df, split_vars, formula, predictors = 3, 
  n_splits = 10, min_obs = 3, max_steps = 4, pval = 0.05,
  step = 0, verbose = FALSE, state = NULL, ...) {
  
  split <- find_split(df, split_vars, formula, fun = lm, predictors, min_obs, n_splits, ...)
  
  if(split$pval < pval && step < max_steps) {
    if(is.null(state)){
      statex <- split
    }else{
      statex <- list(state, split)
    }
    nodes <- splitter(df, split_var = split$name, split_val = split$value)
    stepx <- step + 1
    return(list(Recall(df = nodes$leq, split_vars, formula, predictors, 
                          n_splits, min_obs, max_steps, step = stepx, verbose, pval = pval, state = statex), 
                Recall(df = nodes$gre, split_vars, formula, predictors, 
                          n_splits, min_obs, max_steps, step = stepx, verbose, pval = pval, state = statex)))
  } else {
    if(verbose) {
      # Prints reason and adds the split output
      if(split$pval == Inf) {
        cat("Encountered node below minimum size\n")
      } else if(split$pval >= pval) {
        cat("Encountered p-value of", split$pval, "\n")
      }
      if(step >= max_steps) cat("Maximum steps performed\n")
      return(list(df = df, split = split, node = state))
    } else {
      # Returns a plain df
      return(df)
    }
  }
}

do_reg <- function(node, fun, formula, ...){
  out <- fun(formula, data = node$df, ...)
  return(list(coefs = out))
}

plant_tree <- function(nodes, fun = lm, formula, ...){
  leq <- nodes[[1]]
  gre <- nodes[[2]]
  
  term_leq <- !is.null(names(leq))
  term_gre <- !is.null(names(gre))
  
  if(term_leq & term_gre){
    out_leq <- do_reg(leq, fun, formula, ...)
    out_leq$nodes <- leq$node
    out_gre <- do_reg(gre, fun, formula, ...)
    out_gre$nodes <- gre$node
    return(list(out_leq, out_gre))
  }else if(term_leq){
    out_leq <- do_reg(leq, fun, formula, ...)
    out_leq$nodes <- leq$node
    return(list(out_leq, Recall(gre, fun, formula, ...)))
  }else if(term_gre){
    out_gre <- do_reg(gre, fun, formula)
    out_gre$nodes <- gre$node
    return(list(out_gre, Recall(leq, fun, formula, ...)))
  }else{
    return(list(Recall(leq, fun, formula, ...), 
                Recall(gre, fun, formula, ...)))
  }
}



# E.g. run:
# tree <- get_nodes(df, split_vars, formula, verbose = TRUE)
