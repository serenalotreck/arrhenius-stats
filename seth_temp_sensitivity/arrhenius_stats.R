#' Generate statistics for Arrhenius plots
#' 
#' Adds the resulting statistics to the original input dataframe and saves as 
#' a new .csv file
#' 
#' @param path A string, path to file to use. File ext must be .csv
#' 
#' @return None 
#' 
#' @examples 
#' arrhenius_stats('path/to/my/file.csv')
arrhenius_stats <- function(path) {
  
  # Read in csv 
  df <- read.csv(path)
  cat('Head of data:')
  print(head(df))
  
  # Drop NA rows
  df <- na.omit(df)
  
  # Formate DATE column as date objects
  df$DATE <- as.Date(df$DATE, format = "%m/%d/%y")
  
  # Get statistics 
  for(j in list(5,10,15)){

    cat(sprintf('\n\nGetting stats for %s day range', j))    
    df <- calculate_stats(df, j)
    cat('\nHead of data with stats added:')
    print(head(df))
    
    # Save df as csv
    save_name <- paste(substr(path, 1, nchar(path)-4), j, 'DAY_RANGE', 
                       'arrhenius_data.csv', sep = '_')
    write.csv(df, save_name, row.names = FALSE)
    
  }
    
  print('Done!')
}


#' Calculate arrhenius stats for date windows 
#' 
#' Helper for \code{arrhenius_stats} that does the heavy lifting. For each
#' of the date windows (5, 10 and 15 days), calculates a linear regression and
#' appends the relevant statistics to the end of each row.
#' 
#' @param df, DataFrame with original data 
#' @param dayrange, int for the number of days around sample to look at
#' 
#' @return df, DataFrame with new data appended 
#' 
#' @examples 
#' calculate_stats(df)
calculate_stats <- function(df, dayrange) {
  
  # Get the number of rows in order to preallocate a matrix for storing data
  nrows = nrow(df)
  
  # Define statistic matrix column names 
  stats_names <- c("slope_InvGPP", "yint_InvGPP", "Fstatistic_InvGPP", 
                   "dF_InvGPP", "p_InvGPP", 
                   "slope_InvER", "yint_InvER", "Fstatistic_InvER", 
                   "dF_InvER", "p_InvER")
 
  # Preallocate matrix 
  stats <- matrix(nrow=nrows, ncol=10)
  colnames(stats) <- stats_names 
  
  # Loop over the sites
  for (site in unique(df$SITE)){
    
    # Get the rows in the site 
    rownums <- which(df$SITE==site)
    cat(sprintf('\n\nCalculating regressions for site %s', site))
    
    # Get the site as its own df
    site_df <- subset(df, SITE == site)
    
    # Loop over rows in site
    for(i in rownums){
       
      # Get start and end dates for the range 
      start_date = df[i,"DATE"] - dayrange
      end_date = df[i,"DATE"] + dayrange
      
      # Get the data in the date range
      inrange <- site_df[site_df$DATE >= start_date &
                      site_df$DATE <= end_date, ]
     
      # Skip linear regression if there's only one sample or if the measurement is missing
      if (nrow(inrange)==1){
        
        cat(sprintf('\n Row %s being excluded, only one sample', i))
        GPP_stats <- rep(NA, 5)
        ER_stats <- rep(NA, 5)
        
      } else if (is.na(df[i,'InvGPP'])) { 
        
        GPP_stats <- rep(NA, 5)
        
        lmInvER <- lm(InvER~STANDTEMP, data = inrange)
        ER_stats <- c(summary(lmInvER)$coefficients["STANDTEMP", "Estimate"],
                      summary(lmInvER)$coefficients["(Intercept)", "Estimate"],
                      summary(lmInvER)$fstatistic["value"],
                      summary(lmInvER)$fstatistic["numdf"],
                      suppressWarnings(anova(lmInvER)$'Pr(>F)'[1]))
      
      } else if (is.na(df[i,'InvER'])) {
        
        lmInvGPP <- lm(InvGPP~STANDTEMP, data = inrange)
        GPP_stats <- c(summary(lmInvGPP)$coefficients["STANDTEMP", "Estimate"],
                       summary(lmInvGPP)$coefficients["(Intercept)", "Estimate"],
                       summary(lmInvGPP)$fstatistic["value"],
                       summary(lmInvGPP)$fstatistic["numdf"],
                       suppressWarnings(anova(lmInvGPP)$'Pr(>F)'[1]))
        
        ER_stats <- rep(NA, 5)
        
      } else if (is.na(df[i,'InvGPP']) & is.na(df[i,'InvER'])) {
        
        GPP_stats <- rep(NA, 5)
        ER_stats <- rep(NA, 5)
        
      } else {
        # Calculate linear regressions 
        lmInvGPP <- lm(InvGPP~STANDTEMP, data = inrange)
        lmInvER <- lm(InvER~STANDTEMP, data = inrange)
        
        # Get statistics
        GPP_stats <- c(summary(lmInvGPP)$coefficients["STANDTEMP", "Estimate"],
                       summary(lmInvGPP)$coefficients["(Intercept)", "Estimate"],
                       summary(lmInvGPP)$fstatistic["value"],
                       summary(lmInvGPP)$fstatistic["numdf"],
                       suppressWarnings(anova(lmInvGPP)$'Pr(>F)'[1]))
        
        ER_stats <- c(summary(lmInvER)$coefficients["STANDTEMP", "Estimate"],
                      summary(lmInvER)$coefficients["(Intercept)", "Estimate"],
                      summary(lmInvER)$fstatistic["value"],
                      summary(lmInvER)$fstatistic["numdf"],
                      suppressWarnings(anova(lmInvER)$'Pr(>F)'[1]))
      }
      
      # Add stats to matrix
      stats[i, 1:5] <- GPP_stats
      stats[i, 6:10] <- ER_stats
      
    }
  }
    
  # Convert matrix to dataframe
  stats_df <- as.data.frame(stats)
  
  # Horiontally concat with original df 
  overall_df <- cbind(df, stats_df)
  
  return(overall_df)
  
}

## COMMENT out these lines to run the tests
#path = "Shiu_lab/for-others/seth_temp_sensitivity/Final_Data_Serena.csv"
#arrhenius_stats(path)





