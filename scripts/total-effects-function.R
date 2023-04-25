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

# notes: model structure must be specified this way
# rbr_qs ~ dc + tssm + sdd +tri +age + avgBio 
# tssm ~ sdd + tri + dc + age + avgBio
# sdd ~  avgBio + age + tri

# this function is currently set up to measure the indirect effects of forest structure on 
# wildfire burn severity through the mediators: snow free date and snow free duration

#function

tot.eff <- function(df, response){
  if(response == "extreme"){
    x = df[13,8] # 13 is age -> sdd
    y = df[3,8]  # sdd -> rbr
    age_sf = (x * y) # age snow free as mediator
    x1 = df[12,8] # 12 is avgBio -> sdd
    y1 = df[3, 8] # sdd -> rbr
    abio_sf = (x1 * y1) # snow free as mediator
    x2 = df[10, 8] # age -> tssm
    y2 = df[2, 8] # tssm -> rbr
    age_sfd = (x2 * y2) # snow free duration as mediator
    x3 = df[11, 8] # abio -> tssm
    y3 = df[2, 8] # tssm -> rbr
    abio_sfd = (x3 * y3) # snow free duration as mediator
    
    #total indirect sum age on extreme
    ind_sum_age <- (age_sf + age_sfd)
    
    #direct effect of age on extreme
    tot_age <- df[13,8]
    
    #total direct effect of age on extreme
    sum.tot_age <- (ind_sum_age + tot_age)
    
    #total indirect sum avgbio on extreme
    ind_sum_bio <- (abio_sf + abio_sfd)
    
    #direct effect of avgbio on extreme
    tot_bio <- df[12,8]
    
    #total direct effect of avgbio on extreme
    sum.tot_bio <- (ind_sum_bio + tot_bio)
    
    #assign col names
    cols = c("Pathway", "Total Indirect Effect", "Total Effect")
    
    #develop matrix
    matrix1 = matrix(c("Stand age", "Average Biomass",
                       ind_sum_age, ind_sum_bio, sum.tot_age, sum.tot_bio), ncol =3, nrow =2)
    
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
    x = df[13,8] # 13 is age -> sdd
    y = df[3,8]  # sdd -> rbr
    age_sf = (x * y) # age snow free as mediator
    x1 = df[12,8] # 12 is avgBio -> sdd
    y1 = df[3, 8] # sdd -> rbr
    abio_sf = (x1 * y1) # snow free as mediator
    x2 = df[10, 8] # age -> tssm
    y2 = df[2, 8] # tssm -> rbr
    age_sfd = (x2 * y2) # snow free duration as mediator
    x3 = df[11, 8] # abio -> tssm
    y3 = df[2, 8] # tssm -> rbr
    abio_sfd = (x3 * y3) # snow free duration as mediator
    
    #total indirect sum age on median
    ind_sum_age <- (age_sf + age_sfd)
    
    #direct effect of age on median
    tot_age <- df[13,8]
    
    #total direct effect of age on median
    sum.tot_age <- (ind_sum_age + tot_age)
    
    #total indirect sum avgbio on median
    ind_sum_bio <- (abio_sf + abio_sfd)
    
    #direct effect of avgbio on median
    tot_bio <- df[12,8]
    
    #total direct effect of avgbio on median
    sum.tot_bio <- (ind_sum_bio + tot_bio)
    
    #make column names
    cols = c("Pathway", "Total Indirect Effect", "Total Effect")
    
    #create matrix
    matrix2 = matrix(c("Stand age", "Average Biomass", 
                       ind_sum_age, ind_sum_bio, sum.tot_age, sum.tot_bio), ncol =3, nrow = 2)
    
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

