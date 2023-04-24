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

# this function is currently set up to measure the indirect effects of forest structure on 
# wildfire burn severity

ind.eff <- function(df, response){
  if(Response == "extreme"){
    x = df[12,8]
    y = df[3,8]
    age_q = (x * y)
    x1 = df[11,8]
    y1 = df[3, 8]
    abio = (x1 * y1)
    cols = c("Pathway", "Indirect Effect")
    matrix1 = matrix(c("Stand age", "Average Biomass", age_q, abio), ncol =2)
    table1 = as.table(matrix1)
    colnames(tablel1) = cols
    return(table1)
  }
  if(Response == "median"){
    x = df[12,8]
    y = df[11,8]
    age_q = (x * y)
    x1 = df[6,8]
    y1 = df[3, 8]
    abio = (x1 * y1)
    cols = c("Pathway", "Indirect Effect")
    matrix2 = matrix(c("Stand age", "Average Biomass", age_q, abio), ncol =2)
    table2 = as.table(matrix2)
    colnames(table2) = cols
    return(table2)
  }
}