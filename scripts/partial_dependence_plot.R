# script to make partial dependent plots 

library(ggplot2)
library(nlme)
library(effects)



# 
summary(lme(rbr_qs ~ dc + tssm + sdd +tri +age + cc + avgBio, 
    random = ~1 | Fire_Year/size_class, na.action = na.omit, data = new_data, method = "ML"))
summary(sem_ext)
coef_table = sem_ext |> 
  piecewiseSEM::coefs(intercepts = TRUE) |> 
  as.data.frame() |> 
  select(c(Response:Std.Estimate)) |> 
  mutate(region = rep("ecozone", 17))
  
coef_table_dc = coef_table[coef_table$Response == 'rbr_qs' & (coef_table$Predictor == "tssm" | coef_table$Predictor =="(Intercept)"),]


  



coef_table2 <- west_sem_ext |> 
  piecewiseSEM::coefs(intercepts = TRUE) |> 
  as.data.frame() |>
  select(c(Response:Std.Estimate)) |> 
  mutate(region = rep("west", 16))

coef_table2 <- coef_table2[coef_table2$Response == 'rbr_qs' & (coef_table2$Predictor == "tssm" | coef_table2$Predictor =="(Intercept)"),]
  


coef_table3 <- east_sem_ext |> 
  piecewiseSEM::coefs(intercepts = TRUE) |> 
  as.data.frame() |>
  select(c(Response:Std.Estimate)) |> 
  mutate(region = rep("east", 16))

coef_table3 <- coef_table3[coef_table3$Response == 'rbr_qs' & (coef_table3$Predictor == "tssm" | coef_table3$Predictor =="(Intercept)"),]


binded <- rbind(coef_table_dc, coef_table2, coef_table3)

dc_plot <- ggplot() + geom_point(data=new_data, aes(x = tssm, y= rbr_qs)) + 
                    geom_abline(slope = coef_table_dc$Estimate[[2]], intercept = coef_table_dc$Estimate[[1]], colour = "red")+
                    geom_abline(slope = coef_table2$Estimate[[2]], intercept = coef_table2$Estimate[[1]],  colour = "blue")+
                    geom_abline(slope = coef_table3$Estimate[[2]],intercept = coef_table3$Estimate[[1]], colour = "green")+
                      labs(x="Snow-free date", y="Severity")

dc_plot
################

coef_table_med = sem_med |> 
  piecewiseSEM::coefs(intercepts = TRUE) |> 
  as.data.frame() |> 
  select(c(Response:Std.Estimate)) |> 
  mutate(region = rep("ecozone", 17))

coef_table_med = coef_table_med[coef_table_med$Response == 'RBR_median' & (coef_table_med$Predictor == "tssm" | coef_table_med$Predictor =="(Intercept)"),]

coef_table_med <- coef_table_med |> 
  mutate(ci_lwr = (Estimate - 1.96 * Std.Error), 
         ci_upr = (Estimate + 1.96 *Std.Error))


new_data_all <-  new_data |> 
  select(c(tssm , rbr_qs)) |>
  mutate(ci_upr = rep(coef_table_med$ci_upr[[2]], 284),
        ci_lwr = rep(coef_table_med$ci_lwr[[2]], 284), 
        slope = rep(coef_table_med$Estimate[[2]], 284),
        intercept = rep(coef_table_med$Estimate[[1]], 284))

plot <-ggplot(data = new_data_all, aes(x = tssm , y = rbr_qs))+
  geom_point()+
  geom_ribbon(data = new_data_all, aes(ymin = ci_lwr, ymax = ci_upr, color = NULL), alpha = .15) +
  geom_line(data = new_data_all, aes(y = slope), size = 1)


coef_tablem2 <- west_sem_med |> 
  piecewiseSEM::coefs(intercepts = TRUE) |> 
  as.data.frame() |>
  select(c(Response:Std.Estimate)) |> 
  mutate(region = rep("west", 16)) |> 
mutate(ci_lwr = (Estimate - 1.96 * Std.Error), 
       ci_upr = (Estimate + 1.96 *Std.Error))

coef_tablem2 <- coef_tablem2[coef_tablem2$Response == 'RBR_median' & (coef_tablem2$Predictor == "tssm" | coef_tablem2$Predictor =="(Intercept)"),]



coef_tablem3 <- east_sem_med |> 
  piecewiseSEM::coefs(intercepts = TRUE) |> 
  as.data.frame() |>
  select(c(Response:Std.Estimate)) |> 
  mutate(region = rep("east", 16)) |>  
  mutate(ci_lwr = (Estimate - 1.96 * Std.Error), 
         ci_upr = (Estimate + 1.96 * Std.Error))

coef_tablem3 <- coef_tablem3[coef_tablem3$Response == 'RBR_median' & (coef_tablem3$Predictor == "tssm" | coef_tablem3$Predictor =="(Intercept)"),]


binded_m <- rbind(coef_table_med, coef_tablem2, coef_tablem3)



### good plot
#need to figure out how to colour in the plots 
p <- ggplot() + geom_point(data = new_data, aes(x = tssm, y= RBR_median), alpha = 0.3) + 
  scale_color_manual(values = c("entire" = "red"))+
  geom_abline(slope = coef_table_med$Estimate[[2]], intercept = coef_table_med$Estimate[[1]], color = "entire")+
  geom_abline(slope = coef_table_med$ci_lwr[[2]], intercept = coef_table_med$Estimate[[1]], color = "entire", linetype ="dashed") +
  geom_abline(slope = coef_table_med$ci_upr[[2]], intercept = coef_table_med$Estimate[[1]], color = "entire",  linetype ="dashed") 
 

p <- p + geom_abline(slope = coef_tablem2$Estimate[[2]], intercept = coef_tablem2$Estimate[[1]], colour = "green")+
    geom_abline(slope = coef_tablem2$ci_lwr[[2]], intercept = coef_tablem2$Estimate[[1]], colour = "green", linetype ="dashed") +
    geom_abline(slope = coef_tablem2$ci_upr[[2]], intercept = coef_tablem2$Estimate[[1]], colour = "green", , linetype ="dashed") 
  


p <- p + geom_abline(slope = coef_tablem3$Estimate[[2]], intercept = coef_tablem3$Estimate[[1]], colour = "blue")+
    geom_abline(slope = coef_tablem3$ci_lwr[[2]], intercept = coef_tablem3$Estimate[[1]], colour = "blue", linetype ="dashed") +
    geom_abline(slope = coef_tablem3$ci_upr[[2]], intercept = coef_tablem3$Estimate[[1]], colour = "blue", , linetype ="dashed") 

p + theme_classic()
  
  
  summary(sem_med[[3]])
new_pred = new_data |> 
  select(c(rbr_qs, tssm))
test <- predict(sem_med[[3]], new_pred, level = 0)
  
  summary(sem_ext)



  estim <- modelbased::estimate_prediction(sem_med[[3]], at = "tssm", ci = c(0.50, 0.69, 0.89, 0.97))
  
estim <-marginaleffects::slopes(sem_med[[3]])

estim_tssm <- estim[estim$term == "tssm",]

dc_plot2
  
  geom_abline(slope = seq(coef_table_med$ci_upr[[2]], coef_table_med$Estimate[[2]], 
                          coef_table_med$ci_lwr[[2]]), color = "grey60", 
              intercept = coef_table_med$Estimate[[1]])






  geom_abline(slope = coef_tablem2$Estimate[[2]], intercept = coef_tablem2$Estimate[[1]],  colour = "blue")+
  geom_abline(slope = coef_tablem3$Estimate[[2]],intercept = coef_tablem3$Estimate[[1]], colour = "green")+
  labs(x="Snow-free date", y="Median BurnSeverity") + theme_bw() 

dc_plot2

pre.x <- rnorm(70, mean = 24, sd = 4)
post.x <- rnorm(70, mean = 12, sd = 4)
clinical.data <- data.frame(pre.x, post.x)
error.measurement <- 3.2

ribbondata<- data.frame(x=c(-50, pre.x, 50),
                        ymin=c(  -50 - error.measurement,
                                 pre.x - error.measurement,
                                 50 - error.measurement),
                        ymax=c(  -50 + error.measurement,
                                 pre.x + error.measurement,
                                 50 + error.measurement)
)
