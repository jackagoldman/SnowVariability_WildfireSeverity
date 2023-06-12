# clean sem table output

sem_table <- function(x)
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

return(x2)
}
