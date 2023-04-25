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
    cols = c("Pathway", "Mediator" , "Indirect Effect")
    matrix1 = matrix(c("Stand age", "Average Biomass", "Stand age", "Average Biomass",
                       "snow free date", "snow free date" , "snow free duration", "snow free duration",
                       age_sf, abio_sf, age_sfd, abio_sfd), ncol =3)
    table1 = as.table(matrix1)
    colnames(table1) = cols
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
    cols = c("Pathway", "Mediator" , "Indirect Effect")
    matrix2 = matrix(c("Stand age", "Average Biomass", "Stand age", "Average Biomass",
                       "snow free date", "snow free date" , "snow free duration", "snow free duration",
                       age_sf, abio_sf, age_sfd, abio_sfd), ncol =3)
    table2 = as.table(matrix2)
    colnames(table2) = cols
    return(table2)
  }
}
  
  