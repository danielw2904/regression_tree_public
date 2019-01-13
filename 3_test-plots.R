load("./data/processed_data.Rdata")

library(ggplot2)
library(viridis)
library(dplyr)
library(rgeos)


poly_ggplot <- gBuffer(poly, byid=TRUE, width=0)
poly_ggplot <- fortify(poly_ggplot, region="NUTS_ID")

gdp_pc <- depvar_list$gdp_pc

quant_gdp_pc_00 <- quantile(gdp_pc$GDP_pc_2000, probs = seq(0, 1, by = 1/6))
quant_gdp_pc_07 <- quantile(gdp_pc$GDP_pc_2007, probs = seq(0, 1, by = 1/6))
quant_gdp_pc_08 <- quantile(gdp_pc$GDP_pc_2008, probs = seq(0, 1, by = 1/6))
quant_gdp_pc_15 <- quantile(gdp_pc$GDP_pc_2015, probs = seq(0, 1, by = 1/6))

equal_gdp_pc_00 <- seq(min(gdp_pc$GDP_pc_2000), max(gdp_pc$GDP_pc_2000), 
                       by = (max(gdp_pc$GDP_pc_2000) - min(gdp_pc$GDP_pc_2000)))
equal_gdp_pc_07 <- seq(min(gdp_pc$GDP_pc_2007), max(gdp_pc$GDP_pc_2007), 
                       by = (max(gdp_pc$GDP_pc_2007) - min(gdp_pc$GDP_pc_2007)))
equal_gdp_pc_08 <- seq(min(gdp_pc$GDP_pc_2008), max(gdp_pc$GDP_pc_2008), 
                       by = (max(gdp_pc$GDP_pc_2008) - min(gdp_pc$GDP_pc_2008)))
equal_gdp_pc_15 <- seq(min(gdp_pc$GDP_pc_2015), max(gdp_pc$GDP_pc_2015), 
                       by = (max(gdp_pc$GDP_pc_2015) - min(gdp_pc$GDP_pc_2015)))

gdp_pc$quant_gdp_pc_00 <- cut(gdp_pc$GDP_pc_2000, breaks = quant_gdp_pc_00, include_lowest = T)
gdp_pc$quant_gdp_pc_07 <- cut(gdp_pc$GDP_pc_2007, breaks = quant_gdp_pc_07, include_lowest = T)
gdp_pc$quant_gdp_pc_08 <- cut(gdp_pc$GDP_pc_2008, breaks = quant_gdp_pc_08, include_lowest = T)
gdp_pc$quant_gdp_pc_15 <- cut(gdp_pc$GDP_pc_2015, breaks = quant_gdp_pc_15, include_lowest = T)

gdp_pc$equal_gdp_pc_00 <- cut(gdp_pc$GDP_pc_2000, breaks = equal_gdp_pc_00, include_lowest = T)
gdp_pc$equal_gdp_pc_07 <- cut(gdp_pc$GDP_pc_2007, breaks = equal_gdp_pc_07, include_lowest = T)
gdp_pc$equal_gdp_pc_08 <- cut(gdp_pc$GDP_pc_2008, breaks = equal_gdp_pc_08, include_lowest = T)
gdp_pc$equal_gdp_pc_15 <- cut(gdp_pc$GDP_pc_2015, breaks = equal_gdp_pc_15, include_lowest = T)


poly_plot_gdp_pc <- na.omit(left_join(poly_ggplot, gdp_pc[ , c(1, 38:ncol(gdp_pc))], by = c("id" = "NUTS_ID")))


gdp_pc_plot_list <- list()
plot_names <- names(poly_plot_gdp_pc)[8:ncol(poly_plot_gdp_pc)]

for(i in plot_names){
 gdp_pc_plot_list[[i]] <- ggplot() + 
   geom_polygon(data = poly_plot_gdp_pc, 
                aes(fill = poly_plot_gdp_pc[ , which(names(poly_plot_gdp_pc) == i)], 
                    x = long, y = lat, group = group)) + 
   geom_path(data = poly_plot_gdp_pc, aes(x = long, y = lat, group = group), 
             color = "black", size = 0.05) + 
   labs(x = NULL, y = NULL) + 
   theme(axis.line = element_blank(), 
         axis.text.x=element_blank(), axis.text.y = element_blank(), 
         axis.ticks = element_blank(), 
         axis.title.x = element_blank(), axis.title.y = element_blank()) + 
   coord_equal() + 
   scale_fill_viridis(option = "plasma", direction = -1, discrete = TRUE) + 
   theme(legend.position = "none", legend.justification = c(0,1), 
         legend.background = element_rect(fill="transparent"), 
         legend.title = element_blank())
}




gdp_gr <- depvar_list$gdp_pc_gr

quant_gdp_gr_0015 <- quantile(gdp_gr$GDP_gr_0015, probs = seq(0, 1, by = 1/6))
quant_gdp_gr_0007 <- quantile(gdp_gr$GDP_gr_0007, probs = seq(0, 1, by = 1/6))
quant_gdp_gr_0815 <- quantile(gdp_gr$GDP_gr_0815, probs = seq(0, 1, by = 1/6))

equal_gdp_gr_0015 <- seq(min(gdp_gr$GDP_gr_0015), max(gdp_gr$GDP_gr_0015), 
                       by = (max(gdp_gr$GDP_gr_0015) - min(gdp_gr$GDP_gr_0015)))
equal_gdp_gr_0007 <- seq(min(gdp_gr$GDP_gr_0007), max(gdp_gr$GDP_gr_0007), 
                       by = (max(gdp_gr$GDP_gr_0007) - min(gdp_gr$GDP_gr_0007)))
equal_gdp_gr_0815 <- seq(min(gdp_gr$GDP_gr_0815), max(gdp_gr$GDP_gr_0815), 
                       by = (max(gdp_gr$GDP_gr_0815) - min(gdp_gr$GDP_gr_0815)))

gdp_gr$quant_gdp_gr_0015 <- cut(gdp_gr$GDP_gr_0015, breaks = quant_gdp_gr_0015, include_lowest = T)
gdp_gr$quant_gdp_gr_0007 <- cut(gdp_gr$GDP_gr_0007, breaks = quant_gdp_gr_0007, include_lowest = T)
gdp_gr$quant_gdp_gr_0815 <- cut(gdp_gr$GDP_gr_0815, breaks = quant_gdp_gr_0815, include_lowest = T)

gdp_gr$equal_gdp_gr_0015 <- cut(gdp_gr$GDP_gr_0015, breaks = equal_gdp_gr_0015, include_lowest = T)
gdp_gr$equal_gdp_gr_0007 <- cut(gdp_gr$GDP_gr_0007, breaks = equal_gdp_gr_0007, include_lowest = T)
gdp_gr$equal_gdp_gr_0815 <- cut(gdp_gr$GDP_gr_0815, breaks = equal_gdp_gr_0815, include_lowest = T)


poly_plot_gdp_gr <- na.omit(left_join(poly_ggplot, gdp_gr[ , c(1, 10:ncol(gdp_gr))], by = c("id" = "NUTS_ID")))


gdp_gr_plot_list <- list()
plot_names <- names(poly_plot_gdp_gr)[8:ncol(poly_plot_gdp_gr)]

for(i in plot_names){
  gdp_gr_plot_list[[i]] <- ggplot() + 
    geom_polygon(data = poly_plot_gdp_gr, 
                 aes(fill = poly_plot_gdp_gr[ , which(names(poly_plot_gdp_gr) == i)], 
                     x = long, y = lat, group = group)) + 
    geom_path(data = poly_plot_gdp_gr, aes(x = long, y = lat, group = group), 
              color = "black", size = 0.05) + 
    labs(x = NULL, y = NULL) + 
    theme(axis.line = element_blank(), 
          axis.text.x=element_blank(), axis.text.y = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank()) + 
    coord_equal() + 
    scale_fill_viridis(option = "plasma", direction = -1, discrete = TRUE) + 
    theme(legend.position = "none", legend.justification = c(0,1), 
          legend.background = element_rect(fill="transparent"), 
          legend.title = element_blank())
}




save(poly_plot_gdp_pc, gdp_pc_plot_list, 
     poly_plot_gdp_gr, gdp_gr_plot_list, 
     file = "./data/plots.Rda")

load("./data/plots.Rda")


