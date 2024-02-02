# script to make partial dependent plots 

library(ggplot2)
library(nlme)
library(marginaleffects)
library(patchwork)



#### return marginal effects
#get marginal effects for sem_med
estim_med <-marginaleffects::slopes(sem_med[[3]])
estim_tssm_sem_med <- estim_med[estim_med$term == "tssm",]
#add model ID column
estim_tssm_sem_med$region <- c("Ecozone")
#get marginal effects for west_sem_med
estim_west_med <-marginaleffects::slopes(west_sem_med[[3]])
estim_tssm_west_med <- estim_west_med[estim_west_med$term == "tssm",]
estim_tssm_west_med$region <- c("Boreal Shield West")
#get marginal effects for east_sem_med
estim_east_med <-marginaleffects::slopes(east_sem_med[[3]])
estim_tssm_east_med <- estim_east_med[estim_east_med$term == "tssm",]
estim_tssm_east_med$region <- c("Boreal Shield East")

#rbind the data
med_results <- rbind(estim_tssm_sem_med, estim_tssm_east_med, estim_tssm_west_med)


#make plot entire
p_med_ent <- ggplot(data = estim_tssm_sem_med, aes(x = tssm, y = predicted)) + 
  geom_smooth(data = estim_tssm_sem_med,  colour = "black" , se = TRUE, stat = "smooth", method = "lm") +
  theme_bw() + 
  labs(y = "Median Burn Severity", x = "Snow-free duration") +
  ggtitle("Entire Boreal Shield") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(100,350)
p_med_ent



# make plot west
p_med_west <- ggplot(data = estim_tssm_west_med, aes(x = tssm, y = predicted)) + 
  geom_smooth(data = estim_tssm_west_med,  colour = "black" , se = TRUE, stat = "smooth", method = "lm") +
  theme_bw() + 
  labs(y = "Median Burn Severity", x = "Snow-free duration") +
  ggtitle("Western Boreal Shield Ecoregion") + 
  theme(plot.title = element_text(hjust = 0.5)) + ylim(100,350)
p_med_west

# make plot east
p_med_east <- ggplot(data = estim_tssm_east_med, aes(x = tssm, y = predicted)) + 
  geom_smooth(data = estim_tssm_east_med,  colour = "black" , se = TRUE, stat = "smooth", method = "lm") +
  theme_bw() + 
  labs(y = "Median Burn Severity", x = "Snow-free duration") +
  ggtitle("Eastern Boreal Shield Ecoregion") + 
  theme(plot.title = element_text(hjust = 0.5)) + ylim(100,350)
p_med_east


layer_scales(p_med_ent)$y$range$range


max(estim_tssm_east_med$predicted)
min(estim_tssm_east_med$predicted)

p_med = p_med_ent + p_med_west + p_med_east

g1 = ggplotGrob(p_med_ent)
g2= ggplotGrob(p_med_west)
g3= ggplotGrob(p_med_east)
g <- rbind(g1, g2, g3, size = "first")
g$widths <- gridExtra::unit.pmax(g1$widths, g2$widths, g3$widths)
grid.newpage()
grid.draw(g)
p_med = ggpubr::ggarrange(p_med_ent, p_med_west, p_med_east)



#### return marginal effects
#get marginal effects for sem_ext
estim_ext <-marginaleffects::slopes(sem_ext[[3]])
estim_tssm_sem_ext <- estim_ext[estim_ext$term == "tssm",] 
estim_tssm_sem_ext <-  estim_tssm_sem_ext |> 
  select(-c(rbr_qs))
#add model ID column
estim_tssm_sem_ext$region <- c("Ecozone")
#get marginal effects for west_sem_ext
estim_west_ext <-marginaleffects::slopes(west_sem_ext[[3]])
estim_tssm_west_ext <- estim_west_ext[estim_west_ext$term == "tssm",]
estim_tssm_west_ext$region <- c("Boreal Shield West")
#get marginal effects for east_sem_ext
estim_east_ext <-marginaleffects::slopes(east_sem_ext[[3]])
estim_tssm_east_ext <- estim_east_ext[estim_east_ext$term == "avgBio",]
estim_tssm_east_ext$region <- c("Boreal Shield East")
view(estim_east_ext)
#rbind the data
ext_results <- rbind(estim_tssm_sem_ext, estim_tssm_east_ext, estim_tssm_west_ext)


#make plot entire
p_ext_ent <- ggplot(data = estim_tssm_sem_ext, aes(x = tssm, y = predicted)) + 
  geom_smooth(data = estim_tssm_sem_ext,  colour = "black" , se = TRUE, stat = "smooth", method = "lm") +
  theme_bw() + 
  labs(y = "sqrt(Burn Severity Extremes)", x = "Snow-free duration") +
  ggtitle("Entire Boreal Shield") +
  theme(plot.title = element_text(hjust = 0.5)) +  ylim(8, 20)
p_ext_ent

# make plot west
p_ext_west <- ggplot(data = estim_tssm_west_ext, aes(x = tssm, y = predicted)) + 
  geom_smooth(data = estim_tssm_west_ext,  colour = "black" , se = TRUE, stat = "smooth", method = "lm") +
  theme_bw() + 
  labs(y = "sqrt(Burn Severity Extremes)", x = "Snow-free duration") +
  ggtitle("Western Boreal Shield Ecoregion") + 
  theme(plot.title = element_text(hjust = 0.5)) +ylim(8, 22)
p_ext_west

# make plot east
p_ext_east <- ggplot(data = estim_tssm_east_ext, aes(x = avgBio, y = predicted)) + 
  geom_smooth(data = estim_tssm_east_ext,  colour = "black" , se = TRUE, stat = "smooth", method = "lm") +
  theme_bw() + 
  labs(y = "sqrt(Burn Severity Extremes)", x = "Average Biomass (t/ha)") +
  ggtitle("Eastern Boreal Shield Ecoregion") + 
  theme(plot.title = element_text(hjust = 0.5)) +  ylim(8, 22)
p_ext_east

layer_scales(p_ext_east)$y$range$range
layer_scales(p_ext_west)$y$range$range
layer_scales(p_ext_ent)$y$range$range


max(estim_tssm_west_ext$predicted)
min(estim_tssm_west_ext$predicted)
max(estim_tssm_east_ext$predicted)
min(estim_tssm_east_ext$predicted)
max(estim_tssm_sem_ext$predicted)
min(estim_tssm_sem_ext$predicted)


p_ext = p_ext_ent + p_ext_west + p_ext_east
p_ext = ggpubr::ggarrange( p_ext_ent , p_ext_west , p_ext_east)


g <- ggplot_build(p_dc_ext)
unique(g$data[[1]]["fill"])
fill
1   #440154FF
81  #21908CFF
161 #FDE725FF

## arrange plots

p = p_med + p_ext 
p
cowplot::plot_grid(p_med_ent, p_med_west, p_med_east, p_ext_ent , p_ext_west , p_ext_east, nrow = 2)

ggpubr::ggarrange(p_med, p_ext, nrow = 2)



#make plot entire
 ggplot(data = estim_tssm_sem_med) + 
   geom_point(aes(tssm, RBR_median))+
   geom_line(data = estim_tssm_sem_med, aes(x = tssm, y = predicted))+ 
   theme_bw() + 
  labs(y = "Median Burn Severity", x = "Snow-free duration") +
  ggtitle("Entire Boreal Shield") +
  theme(plot.title = element_text(hjust = 0.5)) 
df = estim_tssm_sem_med

 ggplot(data = df, aes(x = tssm, RBR_median)) + 
   geom_ribbon(aes(ymin=predicted-1.96*std.error, ymax=predicted+1.96*std.error), alpha=0.2) +
   geom_line(aes(tssm, predicted, colour = predicted)) +
   theme_bw()
 
 p <- plot(ggeffects::ggpredict(sem_ext[[3]], terms = "tssm"))
 p +