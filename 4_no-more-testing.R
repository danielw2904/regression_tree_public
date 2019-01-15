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

tree <- get_nodes(data, 
                  split_vars = names(data)[3:ncol(data)], 
                  formula = "gdp_gr ~ gdp_init",
                  max_steps = 5, n_splits = 100, min_obs = 50, verbose = TRUE)

saveRDS(tree, "output/run.rds")
