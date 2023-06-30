# clean sem table output

require(tidyverse)
require(kableExtra)



sem_table <- function(x, region)
{
  
x2<- summary(x)$coefficients
x2$Estimate <- round(x2$Estimate, 3)
x2$Std.Error <- round(x2$Std.Error, 3)
x2$Crit.Value <- round(x2$Crit.Value, 3)
x2$P.Value <- round(x2$P.Value, 3)
x2$P.Value <- ifelse(x2$P.Value<0.001, "<0.001", x2$P.Value)
x2$Std.Estimate <- round(x2$Std.Estimate, 3)
x2$Response <- gsub("_", " ", x2$Response)
x2$Predictor <- gsub("_", " ",x2$Predictor)
colnames(x2)[4:8] <- c("SE", "DF", "t-Value", "P-Value", "Std. Estimate")
x2<- data.frame(x2)
x3<- x2 %>% mutate(Response = case_when(
  Response == "RBR median" ~"Severity",
  Response == "rbr qs" ~ "Severity",
  Response == "sdd" ~ "SFD",
  Response == "tssm" ~ "SFDr"),
  Predictor = case_when(
    Predictor == "tri" ~ "Topo",
    Predictor == "dc" ~ "Drought",
    Predictor == "cc" ~ "Canopy Closure",
    Predictor == "age" ~ "Stand Age",
    Predictor == "avgBio" ~ "Biomass", 
    Predictor == "sdd" ~ "SFD",
    Predictor == "tssm" ~ "SFDR",
  ))
colnames(x3)[9] <-""

vector1 <- c("rbr qs")
vector2 <- c("RBR median")

if (vector1 %in% x2$Response & (missing(region))){
  x_table <- x3 %>% 
    kbl(caption = "piecewiseSEM results for extreme burn severity for boreal shield of Ontario", 
        booktabs = T) %>% 
    kable_classic(full_width = F, html_font = "Cambria")
} else if (vector2 %in% x2$Response & (missing(region))){
  x_table <- x3 %>% 
    kbl(caption = "piecewiseSEM results for median burn severity for boreal shield of Ontario", 
        booktabs = T) %>% 
    kable_classic(full_width = F, html_font = "Cambria")
} else if( vector1 %in% x2$Response & region == "w"){
x_table <- x3 %>% 
  kbl(caption = "piecewiseSEM results for extreme burn severity in western boreal shield of Ontario", 
      booktabs = T) %>% 
  kable_classic(full_width = F, html_font = "Cambria") 
}else if ( vector1 %in% x2$Response & region == "e"){
  x_table <- x3 %>% 
    kbl(caption = "piecewiseSEM results for extreme burn severity in eastern boreal shield of Ontario", 
        booktabs = T) %>% 
    kable_classic(full_width = F, html_font = "Cambria") 
} else if (vector2 %in% x2$Response & region == "w") {
    x_table <- x3 %>% 
      kbl(caption = "piecewiseSEM results for median burn severity in western boreal shield of Ontario", 
          booktabs = T) %>% 
      kable_classic(full_width = F, html_font = "Cambria") 
} else if(vector2 %in% x2$Response & region == "e") {
    x_table <- x3 %>% 
      kbl(caption = "piecewiseSEM results for median burn severity in eastern boreal shield of Ontario", 
          booktabs = T) %>% 
      kable_classic(full_width = F, html_font = "Cambria") 
}



return(x_table)
}
