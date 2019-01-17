require(partykit)
require(spdep)

row_stdz <- function(W){
  sums <- rowSums(W)
  return(W / sums)
}

load("data/processed_data.Rdata")

source("reg_tree/8_model-fun.R")

data <- data.frame(
  gdp_gr = depvar_list$gdp_pc_gr$GDP_gr_0015,
  gdp_init = depvar_list$gdp_pc$GDP_pc_2000,
  pop_den = split_list$pop_den$POP_den_2000,
  inv_agr = split_list$inv_agr$INV_agr_2000,
  inv_con = split_list$inv_con$INV_con_2000,
  inv_ind = split_list$inv_ind$INV_ind_2000,
  inv_mar = split_list$inv_mar$INV_mar_2000,
  inv_nms = split_list$inv_nms$INV_nms_2000,
  emp_agr = split_list$emp_agr$EMP_agr_2000,
  emp_con = split_list$emp_con$EMP_con_2000,
  emp_ind = split_list$emp_ind$EMP_ind_2000,
  emp_mar = split_list$emp_mar$EMP_mar_2000,
  emp_nms = split_list$emp_nms$EMP_nms_2000,
  thw_agr = split_list$thw_agr$THW_agr_2000,
  thw_con = split_list$thw_con$THW_con_2000,
  thw_ind = split_list$thw_ind$THW_ind_2000,
  thw_mar = split_list$thw_mar$THW_mar_2000,
  thw_nms = split_list$thw_nms$THW_nms_2000)

rownames(data) <- depvar_list$gdp_pc_gr$NUTS_ID
split_vars <- names(data)[3:ncol(data)]


W_negexp <- exp(-((dist/1000)^2))
diag(W_negexp) <- 0
W_negexp <- row_stdz(W_negexp)
listw_negexp <- mat2listw(W_negexp, style = "W")

sar_mod <- lagsarlm(gdp_gr~gdp_init, data = data, 
                    listw = listw_negexp)
data$gdp_gr_sar <- data$gdp_gr - coef(sar_mod)[1] * W_negexp %*% data$gdp_gr

sem_mod <- errorsarlm(gdp_gr~gdp_init, data = data, 
                      listw = listw_negexp)
data$gdp_gr_sem <- data$gdp_gr - coef(sem_mod)[1] * W_negexp %*% data$gdp_gr
data$gdp_init_sem <- data$gdp_init - coef(sem_mod)[1] * W_negexp %*% data$gdp_init




tree_lm <- get_nodes(data, 
                     split_vars = split_vars, 
                     formula = "gdp_gr ~ gdp_init",
                     max_steps = 5, n_splits = 1000, min_obs = 50, 
                     verbose = TRUE)

tree_sar <- get_nodes(data, 
                      split_vars = split_vars, 
                      formula = "gdp_gr_sar ~ gdp_init",
                      max_steps = 5, n_splits = 1000, min_obs = 50, 
                      verbose = TRUE)

tree_sem <- get_nodes(data, 
                      split_vars = split_vars, 
                      formula = "gdp_gr_sem ~ gdp_init_sem",
                      max_steps = 5, n_splits = 1000, min_obs = 50, 
                      verbose = TRUE)

save(tree_lm, tree_sar, tree_sem, file = "./output/run.Rda")

plot(lmtree(gdp_gr ~  gdp_init | 
              pop_den + inv_agr + inv_con + inv_ind + inv_mar + inv_nms + 
              emp_agr + emp_con + emp_ind + emp_mar + emp_nms + thw_agr + 
              thw_con + thw_ind + thw_mar + thw_nms, data = data))


