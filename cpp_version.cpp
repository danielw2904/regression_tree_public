#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List find_split(DataFrame df, DataFrame splitvars, char dep_var, CharacterVector indep_vars) {
  Rcpp::NumericMatrix best_vals;
  
}

find_split <- function(df, splitvars, formula, fun = lm, predictors = 3){
  best_vals <- matrix(NA, nrow = ncol(splitvars), ncol = 2)
  rownames(best_vals) <- colnames(splitvars)
  colnames(best_vals) <- c("Var_value", "Chisq_value")
  spliters <- ncol(splitvars)
  j <- 1
  for(var in splitvars){
    out <- vector(mode = 'double', length = spliters)
    i <- 1
    for(z in unique(sort(var))){ #unique(var[!var %in% sort(unique(var))[1:predictors] & 
#           !var %in% sort(unique(var), decreasing = TRUE)[1:predictors]]) ){
        df_list <- spliter(df, var, z)
                                           if(enough_obs(df_list, predictors = predictors)){
                                             model_list <- coefs(df_list, formula, fun)
                                             res <- obj_fun(model_list)
                                             out[i] <- as.numeric(res$stat)}
                                           else{
                                             out[i] <- NA
                                           }
                                           i <- i + 1
                                           }
                                           best_vals[j, 1] <- unique(sort(var))[which(out == max(out, na.rm = TRUE))]
                                           best_vals[j, 2] <- max(out, na.rm = TRUE)
                                           j <- j + 1
    }
    this <- which(best_vals[,2] == max(best_vals[,2]))
      pval <- dchisq(best_vals[this, 2], df = predictors)
      do_split <- list()
      do_split$name <- rownames(best_vals)[this]
    do_split$value <- best_vals[this, 1]
    do_split$pval <- pval
      return(do_split)
  }
  
// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
