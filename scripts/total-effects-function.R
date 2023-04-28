### total effect function

# Author: Jack A. Goldman

# Date: 2023-04-25

# This function calculates the total effects of forest structure on 
# wildfire burn severity when given the following table structure:
# Predictor, Response, SE, DF, t-Value, P-Value, Std. Estimate
# when subsetting each row in the function (x, x1, y, y1) the row x responds to 
# path of indirect effect of interest into mediator and y responds to path from mediator
# to response. These needs to be adjusted according to variables of interest
# names of columns also need to be adjusted

#function

tot.eff <- function(df, response){
  if(response == "extreme"){
    x = df[df$Predictor =="age" & df$Response == "sdd",8] # 
    y = df[df$Predictor == "sdd" & df$Response == "tssm", 8]
    z = df[df$Predictor == "tssm" & df$Response == "rbr qs", 8]
    #total indirect effect
    age = (x+y+z) # age pathway
    x1 = df[df$Predictor == "avgBio" & df$Response == "sdd",8] 
    y1 = df[df$Predictor == "sdd" & df$Response =="tssm", 8]
    z1 = df[df$Predictor == "tssm" & df$Response == "rbr qs", 8]
    #total indirect effect
    abio = (x1 + y1 + z1) # avgbio pathway
    x2 = df[df$Predictor == "cc" & df$Response == "sdd",8] 
    y2 = df[df$Predictor == "sdd" & df$Response =="tssm", 8]
    z2 = df[df$Predictor == "tssm" & df$Response == "rbr qs", 8]
    #total indirect effect
    cc = (x2 + y2 + z2) #cc pathway
    x3 = df[df$Predictor == "dc" & df$Response == "tssm", 8]
    y3 = df[df$Predictor == "tssm" & df$Response == "rbr qs", 8]
    dc = (x3 + y3)
    
    
    #direct effect of age on extreme
    tot_age <- df[df$Predictor == "age" & df$Response == "rbr qs", 8]
    
    #total direct effect of age on extreme
    sum.tot_age <- (age + tot_age)
    
  
    #direct effect of avgbio on extreme
    tot_bio <- df[df$Predictor == "avgBio" & df$Response == "rbr qs", 8]
    
    #total direct effect of avgbio on extreme
    sum.tot_bio <- (abio + tot_bio)
    
    #direct effect of avgbio on extreme
    tot_cc <- df[df$Predictor == "cc" & df$Response == "rbr qs", 8]
    
    #total direct effect of avgbio on extreme
    sum.tot_cc <- (cc + tot_bio)
    
    #direct effect of dc on extreme
    tot_dc <- df[df$Predictor == "dc" & df$Response == "rbr qs", 8]
    
    #total direct effect of avgbio on extreme
    sum.tot_dc <- (dc + tot_dc)
    
    #assign col names
    cols = c("Pathway", "Total Indirect Effect", "Total Effect")
    
    #develop matrix
    matrix1 = matrix(c("Stand age", "Average Biomass", "Canopy Closure", "Drought Code",
                       age, abio, cc, dc, sum.tot_age, sum.tot_bio, sum.tot_cc, sum.tot_dc), ncol =3, nrow =4)
    
    #table to matrix
    table1 = as.table(matrix1)
    
    #assign colnames
    colnames(table1) = cols
    
    #matrix to dataframe
    table1 = as.data.frame.matrix(table1)
    
    #remove rownames
    rownames(table1) = NULL
    
    #df to tibble
    table1<- tidyr::as_tibble(table1)
    
    #add response column
    table1 <- dplyr::mutate(table1, Response = rep("Extreme Burn Severity"))
    
    #move response column
    table1 <- dplyr::relocate(table1, Response, .before = "Total Indirect Effect")
    
    #change total effect to numeric
    table1$`Total Indirect Effect` = as.numeric(table1$`Total Indirect Effect`)
    table1$`Total Effect` = as.numeric(table1$`Total Effect`)
    
    return(table1)
  }
  if(response == "median"){
    x = df[df$Predictor =="age" & df$Response == "sdd",8] # 
    y = df[df$Predictor == "sdd" & df$Response == "tssm", 8]
    z = df[df$Predictor == "tssm" & df$Response == "RBR median", 8]
    #total indirect effect
    age = (x+y+z) # age pathway
    x1 = df[df$Predictor == "avgBio" & df$Response == "sdd",8] 
    y1 = df[df$Predictor == "sdd" & df$Response =="tssm", 8]
    z1 = df[df$Predictor == "tssm" & df$Response == "RBR median", 8]
    #total indirect effect
    abio = (x1 + y1 + z1) # avgbio pathway
    x2 = df[df$Predictor == "cc" & df$Response == "sdd",8] 
    y2 = df[df$Predictor == "sdd" & df$Response =="tssm", 8]
    z2 = df[df$Predictor == "tssm" & df$Response == "RBR median", 8]
    #total indirect effect
    cc = (x2 + y2 + z2) #cc pathway
    x3 = df[df$Predictor == "dc" & df$Response == "tssm", 8]
    y3 = df[df$Predictor == "tssm" & df$Response == "RBR median", 8]
    dc = (x3 + y3)
    
    
    #direct effect of age on extreme
    tot_age <- df[df$Predictor == "age" & df$Response == "RBR median", 8]
    
    #total direct effect of age on extreme
    sum.tot_age <- (age + tot_age)
    
    
    #direct effect of avgbio on extreme
    tot_bio <- df[df$Predictor == "avgBio" & df$Response == "RBR median", 8]
    
    #total direct effect of avgbio on extreme
    sum.tot_bio <- (abio + tot_bio)
    
    #direct effect of avgbio on extreme
    tot_cc <- df[df$Predictor == "cc" & df$Response == "RBR median", 8]
    
    #total direct effect of avgbio on extreme
    sum.tot_cc <- (cc + tot_bio)
    
    #direct effect of dc on extreme
    tot_dc <- df[df$Predictor == "dc" & df$Response == "RBR median", 8]
    
    #total direct effect of avgbio on extreme
    sum.tot_dc <- (dc + tot_dc)
    
    #assign col names
    cols = c("Pathway", "Total Indirect Effect", "Total Effect")
    
    #develop matrix
    matrix2 = matrix(c("Stand age", "Average Biomass", "Canopy Closure", "Drought Code",
                       age, abio, cc, dc, sum.tot_age, sum.tot_bio, sum.tot_cc, sum.tot_dc), ncol =3, nrow =4)
    
    # matrix as table
    table2 = as.table(matrix2)
    
    #set colnames
    colnames(table2) = cols
    
    #matrix to df
    table2 = as.data.frame.matrix(table2)
    
    #remove reownames
    rownames(table2) = NULL
    
    #dataframe as tibble
    table2<- tidyr::as_tibble(table2)
    
    #add response column
    table2 <- dplyr::mutate(table2, Response = rep("Median Burn Severity"))
    
    #relocate response before indirect effect
    table2 <- dplyr::relocate(table2, Response, .before = "Total Indirect Effect")
    
    #total indirect and total direct effect as numeric
    table2$`Total Indirect Effect` = as.numeric(table2$`Total Indirect Effect`)
    table2$`Total Effect`= as.numeric(table2$`Total Effect`)
    
    
    return(table2)
  }
}

