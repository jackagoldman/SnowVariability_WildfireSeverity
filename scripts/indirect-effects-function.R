### Indirect effect function

# Author: Jack A. Goldman

# Date: 2023-04-24

# This function calculates the indirect effects of forest structure on 
# wildfire burn severity when given the following table structure:
# Predictor, Response, SE, DF, t-Value, P-Value, Std. Estimate
# when subsetting each row in the function (x, x1, y, y1) the row x responds to 
# path of indirect effect of interest into mediator and y responds to path from mediator
# to response. These needs to be adjusted according to variables of interest
# names of columns also need to be adjusted

# notes: model structure must be specified this way
# rbr_qs ~ dc + tssm + sdd +tri +age + avgBio 
# tssm ~ sdd + tri + dc + age + avgBio
# sdd ~  avgBio + age + tri

# this function is currently set up to measure the indirect effects of forest structure on 
# wildfire burn severity through the mediators: snow free date and snow free duration

#function

ind.eff <- function(df, response){
  
  df <- summary(df)$coefficients
  df$Estimate <- round(df$Estimate, 3)
  df$Std.Error <- round(df$Std.Error, 3)
  df$Crit.Value <- round(df$Crit.Value, 3)
  df$P.Value <- round(df$P.Value, 3)
  df$P.Value <- ifelse(df$P.Value<0.001, "<0.001", df$P.Value)
  df$Std.Estimate <- round(df$Std.Estimate, 3)
  df$Response <- gsub("_", " ", df$Response)
  df$Predictor <- gsub("_", " ", df$Predictor)
  colnames(df)[4:8] <- c("SE", "DF", "t-Value", "P-Value", "Std. Estimate")
  df <- data.frame(df)
  
  if (response =="extreme"){
    x = df[df$Predictor =="age" & df$Response == "sdd",8] # 
    y = df[df$Predictor == "sdd" & df$Response == "tssm", 8]
    z = df[df$Predictor == "tssm" & df$Response == "rbr qs", 8]  
    age = (x*y*z) # age pathway
    x1 = df[df$Predictor == "avgBio" & df$Response == "sdd",8] 
    y1 = df[df$Predictor == "sdd" & df$Response =="tssm", 8]
    z1 = df[df$Predictor == "tssm" & df$Response == "rbr qs", 8]
    abio = (x1 * y1 *z1) # avgbio pathway
    x2 = df[df$Predictor == "cc" & df$Response == "sdd",8] 
    y2 = df[df$Predictor == "sdd" & df$Response =="tssm", 8]
    z2 = df[df$Predictor == "tssm" & df$Response == "rbr qs", 8]
    cc = (x2 *y2* z2) #cc pathway
    x3 = df[df$Predictor == "dc" & df$Response == "tssm", 8]
    y3 = df[df$Predictor == "tssm" & df$Response == "rbr qs", 8]
    dc = (x3 * y3)
    x4 = df[df$Predictor == "tri" & df$Response == "sdd", 8]
    y4 = df[df$Predictor == "sdd" & df$Response =="tssm", 8]
    z4 = df[df$Predictor == "tssm" & df$Response == "rbr qs", 8]
    tri = (x4 * y4 *z4 )
    cols = c("Pathway",  "Indirect Effect")
    matrix1 = matrix(c("Stand age", "Average Biomass", "Canopy Closure", "Drought", "Topography",
                       age, abio, cc,dc, tri), ncol =2, )
    table1 = as.table(matrix1)
    colnames(table1) = cols
    table1 = as.data.frame.matrix(table1)
    rownames(table1) = NULL
    table1<- tidyr::as_tibble(table1)
    
    table1 <- dplyr::mutate(table1, Response = rep("Extreme Burn Severity"))
    
    table2 <- dplyr::relocate(table1, Response, .before = "Indirect Effect")
    table2$`Indirect Effect` = as.numeric(table2$`Indirect Effect`)
  }else if(response == "median"){
    x = df[df$Predictor =="age" & df$Response == "sdd",8] # 
    y = df[df$Predictor == "sdd" & df$Response == "tssm", 8]
    z = df[df$Predictor == "tssm" & df$Response == "RBR median",8]  
    age = (x * y *z) # age pathway
    x1 = df[df$Predictor == "avgBio" & df$Response == "sdd",8] 
    y1 = df[df$Predictor == "sdd" & df$Response =="tssm", 8]
    z1 = df[df$Predictor == "tssm" & df$Response == "RBR median", 8]
    abio = (x1 * y1 *z1) # avgbio pathway
    x2 = df[df$Predictor == "cc" & df$Response == "sdd",8] 
    y2 = df[df$Predictor == "sdd" & df$Response =="tssm", 8]
    z2 = df[df$Predictor == "tssm" & df$Response == "RBR median", 8]
    cc = (x2 *y2  * z2) #cc pathway
    x3 = df[df$Predictor == "dc" & df$Response == "tssm", 8]
    y3 = df[df$Predictor == "tssm" & df$Response == "RBR median", 8]
    dc = (x3 * y3)
    x4 = df[df$Predictor == "tri" & df$Response == "sdd", 8]
    y4 = df[df$Predictor == "sdd" & df$Response =="tssm", 8]
    z4 = df[df$Predictor == "tssm" & df$Response == "RBR median", 8]
    tri = (x4 * y4 *z4 )
    cols = c("Pathway", "Indirect Effect")
    matrix2 = matrix(c("Stand age", "Average Biomass", "Canopy Closure", "Drought", "Topography" ,
                       age, abio, cc, dc, tri), ncol =2)
    table2 = as.table(matrix2)
    colnames(table2) = cols
    table2 = as.data.frame.matrix(table2)
    rownames(table2) = NULL
    table2<- tidyr::as_tibble(table2)
    
    table2 <- dplyr::mutate(table2, Response = rep("Median Burn Severity"))
    
    table2 <- dplyr::relocate(table2, Response, .before = "Indirect Effect")
    table2$`Indirect Effect` = as.numeric(table2$`Indirect Effect`)
  }else if(response == "heterogeneity"){
    x = df[df$Predictor =="age" & df$Response == "sdd",8] # 
    y = df[df$Predictor == "sdd" & df$Response == "tssm", 8]
    z = df[df$Predictor == "tssm" & df$Response == "rbr cv",8]  
    age = (x * y *z) # age pathway
    x1 = df[df$Predictor == "avgBio" & df$Response == "sdd",8] 
    y1 = df[df$Predictor == "sdd" & df$Response =="tssm", 8]
    z1 = df[df$Predictor == "tssm" & df$Response == "rbr cv", 8]
    abio = (x1 * y1 *z1) # avgbio pathway
    x2 = df[df$Predictor == "cc" & df$Response == "sdd",8] 
    y2 = df[df$Predictor == "sdd" & df$Response =="tssm", 8]
    z2 = df[df$Predictor == "tssm" & df$Response == "rbr cv", 8]
    cc = (x2 *y2  * z2) #cc pathway
    x3 = df[df$Predictor == "dc" & df$Response == "tssm", 8]
    y3 = df[df$Predictor == "tssm" & df$Response == "rbr cv", 8]
    dc = (x3 * y3)
    x4 = df[df$Predictor == "tri" & df$Response == "sdd", 8]
    y4 = df[df$Predictor == "sdd" & df$Response =="tssm", 8]
    z4 = df[df$Predictor == "tssm" & df$Response == "rbr cv", 8]
    tri = (x4 * y4 *z4 )
    cols = c("Pathway", "Indirect Effect")
    matrix2 = matrix(c("Stand age", "Average Biomass", "Canopy Closure", "Drought", "Topography" ,
                       age, abio, cc, dc, tri), ncol =2)
    table2 = as.table(matrix2)
    colnames(table2) = cols
    table2 = as.data.frame.matrix(table2)
    rownames(table2) = NULL
    table2<- tidyr::as_tibble(table2)
    
    table2 <- dplyr::mutate(table2, Response = rep("Burn Severity Heterogeneity"))
    
    table2 <- dplyr::relocate(table2, Response, .before = "Indirect Effect")
    table2$`Indirect Effect` = as.numeric(table2$`Indirect Effect`)
    
  }
  return(table2)
}

# Specific indirect effects

# the effect of age, cc, bio on bs through sfd

spec.ind.eff <- function(df, response){
  df <- summary(df)$coefficients
  df$Estimate <- round(df$Estimate, 3)
  df$Std.Error <- round(df$Std.Error, 3)
  df$Crit.Value <- round(df$Crit.Value, 3)
  df$P.Value <- round(df$P.Value, 3)
  df$P.Value <- ifelse(df$P.Value<0.001, "<0.001", df$P.Value)
  df$Std.Estimate <- round(df$Std.Estimate, 3)
  df$Response <- gsub("_", " ", df$Response)
  df$Predictor <- gsub("_", " ", df$Predictor)
  colnames(df)[4:8] <- c("SE", "DF", "t-Value", "P-Value", "Std. Estimate")
  df <- data.frame(df)
  
  if(response == "extreme"){
    x = df[df$Predictor =="age" & df$Response == "sdd",8] # 
    y = df[df$Predictor == "sdd" & df$Response == "rbr qs", 8]  
    age = (x*y) # age pathway
    x1 = df[df$Predictor == "avgBio" & df$Response == "sdd",8] 
    y1 = df[df$Predictor == "sdd" & df$Response == "rbr qs", 8]
    abio = (x1 * y1) # avgbio pathway
    x2 = df[df$Predictor == "cc" & df$Response == "sdd",8] 
    y2 = df[df$Predictor == "sdd" & df$Response == "rbr qs", 8]
    cc = (x2 *y2) #cc pathway
    cols = c("Pathway", "Mediator", "Specific Indirect Effect")
    matrix1 = matrix(c("Stand age", "Average Biomass", "Canopy Closure",
                       "Snow Free Date", "Snow Free Date", "Snow Free Date", 
                       age, abio, cc ), ncol =3 )
    table1 = as.table(matrix1)
    colnames(table1) = cols
    table1 = as.data.frame.matrix(table1)
    rownames(table1) = NULL
    table1<- tidyr::as_tibble(table1)
    
    table1 <- dplyr::mutate(table1, Response = rep("Extreme Burn Severity"))
    
    table2 <- dplyr::relocate(table1, Response, .before = "Specific Indirect Effect")
    table2$`Specific Indirect Effect` = as.numeric(table2$`Specific Indirect Effect`)
    
  }else if(response == "median"){
    x = df[df$Predictor =="age" & df$Response == "sdd",8] # 
    y = df[df$Predictor == "sdd" & df$Response == "RBR median", 8]  
    age = (x*y) # age pathway
    x1 = df[df$Predictor == "avgBio" & df$Response == "sdd",8] 
    y1 = df[df$Predictor == "sdd" & df$Response == "RBR median", 8]
    abio = (x1 * y1) # avgbio pathway
    x2 = df[df$Predictor == "cc" & df$Response == "sdd",8] 
    y2 = df[df$Predictor == "sdd" & df$Response == "RBR median", 8]
    cc = (x2 *y2) #cc pathway
    cols = c("Pathway", "Mediator", "Specific Indirect Effect")
    matrix2 = matrix(c("Stand age", "Average Biomass", "Canopy Closure", 
                       "Snow Free Date", "Snow Free Date", "Snow Free Date", 
                       age, abio, cc ), ncol =3, )
    table2 = as.table(matrix2)
    colnames(table2) = cols
    table2 = as.data.frame.matrix(table2)
    rownames(table2) = NULL
    table2<- tidyr::as_tibble(table2)
    
    table2 <- dplyr::mutate(table2, Response = rep("Median Burn Severity"))
    
    table2 <- dplyr::relocate(table2, Response, .before = "Specific Indirect Effect")
    table2$`Specific Indirect Effect` = as.numeric(table2$`Specific Indirect Effect`)
    
  }
  return(table2)
}
