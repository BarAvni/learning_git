# Thornthwaite Model
# Developed by Warren Thornthwaite in 1948 to estimate PET:

#' @param     day
#' @param     year    
#' @param     Tair    (deg C) air temperature
#' @param     TMDH    Total monthly daylight hours

#' @author Bar Avni 
#' @return monthly potential ET 

Thornthwaite = 
  function(day, year, Tair, TMDH){
    
    #       Internal Variables
    
    #       day_diff = the diffrences between months
    #       day_diff0 = find the first day of the month
    #       Tmonth = Mean temperature of the month (deg C)
    #       MH_index = Monthly heat index (deg C)
    #       year_diff = find the first day of the year
    #       year_diff1 = find the first day of the year
    #       year_intersect = thake the index of the first day of the year
    #       AH_factor = multiple the month index to create the annual index
    #       AH_index = annual index
    #       a = Thornthwaite parameter
    #       b = Thornthwaite parameter
    #       Ep = monthly potential ET 
    
    
    # Devide the manths in order to calculate the mean monthly tempature:
    day_diff = diff(day)
    for (i in 1:length(day_diff)) {
      if (day_diff[i] < 1) {
        day_diff[i] = 0 
      }
    }
    
    day_diff0 = which(day_diff %in% 0)
    
    for (j in 1:length(day_diff0)) {
      if (j == 1){
        Tmonth = mean(Tair[1:day_diff0[j]])  
      } else {
        Tmonth[j] = mean(Tair[(day_diff0[j-1]+1):day_diff0[j]])  
      }
    }
    # monthly heat index:
    MH_index = (Tmonth/5)^(1.514)
    
    # Sum the 12 monthly heat index into an annual heat index (I):
    year_diff = diff(year)
    
    year_diff1 = which(year_diff %in% 1)
    year_intersect = intersect(year_diff1, day_diff0)
    
    year_index = which(day_diff0 %in% year_intersect)
    
    AH_factor = (MH_index/5)^(1.514)
    
    for (n in 1:length(year_index)) {
      if (n == 1){
        AH_index = sum(AH_factor[1:year_index[n]]) 
      } else {
        AH_index[n] = sum(AH_factor[(year_index[n-1]+1):year_index[n]])  
      }
    }
    
    # Monthly potential ET - Ep:
    Ep = 0
    for (m in 1:length(AH_index)) {
      a = 0.49+1.79*10^(-2)*AH_index[m] + 7.711*10^(-5)*AH_index[m]^2 + 6.751*10^(-7)*AH_index[m]^3
      for(l in 1:length(MH_index)){
        b =  TMDH[l]/360
        Ep[l] = 1.6*b*((10*Tmonth[l])/(AH_index[m]))^a
      }
    }
    # return
    Ep
  }
