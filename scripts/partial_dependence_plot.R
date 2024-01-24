# script to make partial dependent plots 

library(ggplot2)
library(nlme)
library(marginaleffects)
library(patchwork)



#### return marginal effects
#get marginal effects for sem_med
estim_med <-marginaleffects::slopes(sem_med[[3]])
estim_tssm_sem_med <- estim[estim$term == "tssm",]
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


#make plot
p_med <- ggplot(data = med_results, aes(x = tssm, y = predicted,  fill = region)) + 
  geom_smooth(data = med_results, aes(group = region) , colour = "grey" , se = TRUE, stat = "smooth", method = "lm") +
  viridis::scale_fill_viridis(name = "Region", discrete = TRUE) + theme_bw() + 
  labs(y = "Median Burn Severity", x = "Snow-free duration") + ylim(100, 550)
p_med
layer_scales(p_med)$y$range$range


p_med_a = p_med + theme(legend.position="none")
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
estim_tssm_east_ext <- estim_east_ext[estim_east_ext$term == "tssm",]
estim_tssm_east_ext$region <- c("Boreal Shield East")
view(estim_east_ext)
#rbind the data
ext_results <- rbind(estim_tssm_sem_ext, estim_tssm_east_ext, estim_tssm_west_ext)

#back transform predicted values
ext_results <- ext_results |> 
  mutate(predicted_bt = ((max_q+1) - predicted^2))


#make plot
p_ext <- ggplot(data = ext_results, aes(x = tssm, y = predicted, fill = region)) + 
  geom_smooth(data = ext_results, aes(group = region) , colour = "grey" , se = TRUE, stat = "smooth", method = "lm") +
  viridis::scale_fill_viridis(name = "Region", discrete = TRUE) + theme_bw() +
  labs( x = "Snow-free duration", y = "sqrt(Burn Severity Extremes)") 
p_ext

p_ext_b = p_ext + theme(legend.position="none")

#get ylims
layer_scales(p_ext)$y$range$range



#drought code med
estim_dc_med <- estim_med[estim_med$term == "dc",] 
estim_dc_med$region <- c("Ecozone")
#get marginal effects for west_sem_med
estim_dc_west_med <- estim_west_med[estim_west_med$term == "dc",]
estim_dc_west_med$region <- c("Boreal Shield West")
#get marginal effects for east_sem_med
estim_dc_east_med <- estim_east_med[estim_east_med$term == "dc",]
estim_dc_east_med$region <- c("Boreal Shield East")

#rbind the data
med_dc_results <- rbind(estim_dc_med, estim_dc_east_med, estim_dc_west_med)


#make plot
p_dc_med <- ggplot(data = med_dc_results, aes(x = dc, y = predicted,  fill = region)) + 
  geom_smooth(data = med_results, aes(group = region) , colour = "grey" , se = TRUE, stat = "smooth", method = "lm") +
  viridis::scale_fill_viridis(name = "Region", discrete = TRUE) + theme_bw() + 
  labs(y = "Median Burn Severity", x = "Drought") + ylim(100, 550)
p_dc_med

p_dc_med_c = p_dc_med + theme(legend.position="none")


#### return marginal effects
#get marginal effects for sem_ext
estim_dc_ext <- estim_ext[estim_ext$term == "dc",] 
estim_dc_ext <-  estim_dc_ext |> 
  select(-c(rbr_qs))
#add model ID column
estim_dc_ext$region <- c("Ecozone")
#get marginal effects for west_sem_ext
estim_dc_west_ext <- estim_west_ext[estim_west_ext$term == "dc",]
estim_dc_west_ext$region <- c("Boreal Shield West")
#get marginal effects for east_sem_ext
estim_dc_east_ext <- estim_east_ext[estim_east_ext$term == "dc",]
estim_dc_east_ext$region <- c("Boreal Shield East")
view(estim_east_ext)
#rbind the data
ext_dc_results <- rbind(estim_dc_ext, estim_dc_east_ext, estim_dc_west_ext)

#back transform predicted values
# calculate predicted back transform
# get max value for all of the region
max_q = max(new_data$RBR_quant)
ext_dc_results <- ext_dc_results |> 
  mutate(predicted_bt = ((max_q+1) - predicted^2))



#make plot
p_dc_ext <- ggplot(data = ext_dc_results, aes(x = dc, y = predicted, fill = region)) + 
  geom_smooth(data = ext_dc_results, aes(group = region) , colour = "grey" , se = TRUE, stat = "smooth", method = "lm") +
  viridis::scale_fill_viridis(name = "Region", discrete = TRUE) + theme_bw() + 
  labs( x = "Drought", y = "sqrt(Burn Severity Extremes)")
p_dc_ext

p_dc_ext_d = p_dc_ext

g <- ggplot_build(p_dc_ext)
unique(g$data[[1]]["fill"])
fill
1   #440154FF
81  #21908CFF
161 #FDE725FF

## arrange plots

p_med_a + p_dc_med_c + p_ext_b + p_dc_ext_d + plot_layout(guides = "collect") & theme(legend.position = 'bottom')





# get marginal effects for cv
#get marginal effects for sem_ext
estim_cv <-marginaleffects::slopes(sem_cv[[3]])
estim_tri_cv <- estim_cv[estim_cv$term == "tri",] 

#add model ID column
estim_tri_cv$region <- c("Ecozone")

#back transform log10
estim_tri_cv <- estim_tri_cv |> 
  mutate(predicted_bt = (10^predicted))

#make plot
cv_tri <- ggplot(data = estim_tri_cv, aes(x = tri, y = predicted_bt)) + 
  geom_smooth(data = estim_tri_cv, colour = "grey" , se = TRUE, stat = "smooth", method = "lm") +
  viridis::scale_fill_viridis(name = "Region", discrete = TRUE) + theme_bw() +
  scale_y_continuous(labels = scales::percent)+
  labs( x = "Topographic Ruggedness Index", y = "Variability in Burn Severity (%)") 

cv_tri

#tssm
estim_cv <-marginaleffects::slopes(sem_cv[[3]])
estim_tssm_cv <- estim_cv[estim_cv$term == "tri",] 

#add model ID column
estim_tssm_cv$region <- c("Ecozone")

#back transform log10
estim_tssm_cv <- estim_tssm_cv |> 
  mutate(predicted_bt = (10^predicted))

#make plot
tri_cv <- ggplot(data = estim_tssm_cv, aes(x = tri, y = predicted_bt)) + 
  geom_smooth(data = estim_tssm_cv, colour = "grey" , se = TRUE, stat = "smooth", method = "lm") +
  viridis::scale_fill_viridis(name = "Region", discrete = TRUE) + theme_bw() +
  scale_y_continuous(labels = scales::percent)+
  labs( x = "Topography", y = "Variability in Burn Severity (%)") 

tri_cv

#
estim_cv <-marginaleffects::slopes(sem_cv[[3]])
estim_tssm_cv <- estim_cv[estim_cv$term == "tssm",] 

#add model ID column
estim_tssm_cv$region <- c("Ecozone")

#back transform log10
estim_tssm_cv <- estim_tssm_cv |> 
  mutate(predicted_bt = (10^predicted))

#make plot
tssm_cv <- ggplot(data = estim_tssm_cv, aes(x = tssm, y = predicted_bt)) + 
  geom_smooth(data = estim_tssm_cv, colour = "grey" , fill = "#21908CFF" ,se = TRUE, stat = "smooth", method = "lm")  + theme_bw() +
  scale_y_continuous(labels = scales::percent)+
  labs( x = "Snow-free duration", y = "Variability in Burn Severity (%)") 

tssm_cv


###age
estim_cv <-marginaleffects::slopes(sem_cv[[3]])
estim_age_cv <- estim_cv[estim_cv$term == "age",] 

#add model ID column
estim_age_cv$region <- c("Ecozone")

#back transform log10
estim_age_cv <- estim_age_cv |> 
  mutate(predicted_bt = (10^predicted))

#make plot
age_cv <- ggplot(data = estim_age_cv, aes(x = age, y = predicted_bt)) + 
  geom_smooth(data = estim_age_cv , "grey", fill = "#21908CFF",  se = TRUE, stat = "smooth", method = "lm") +
  viridis::scale_colour_viridis() + theme_bw() +
  scale_y_continuous(labels = scales::percent)+
  labs( x = "Stand Age", y = "Variability in Burn Severity (%)") 

age_cv
