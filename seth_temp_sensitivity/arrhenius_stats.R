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
  print('Head of data:')
  print(head(df))
  
  # Drop NA rows
  df <- na.omit(df)
  
  # Formate DATE column as date objects
  df$DATE <- as.Date(df$DATE, format = "%m/%d/%y")
  
  # Get statistics 
  df <- calculate_stats(df)
  print('\nHead of data with stats added:')
  print(head(df))
  
  # Save df as csv
  save_name <- paste(substr(path, 1, nchar(path)-4), 'arrhenius_data.csv', 
                     sep = '_')
  write.csv(df, save_name, row.names = FALSE)
  
  print('Done!')
}


#' Calculate arrhenius stats for date windows 
#' 
#' Helper for \code{arrhenius_stats} that does the heavy lifting. For each
#' of the date windows (5, 10 and 15 days), calculates a linear regression and
#' appends the relevant statistics to the end of each row.
#' 
#' @params df, DataFrame with original data 
#' 
#' @returns df, DataFrame with new data appended 
#' 
#' @examples 
#' calculate_stats(df)
calculate_stats <- function(df) {
  
  # Get the number of rows in order to preallocate a matrix for storing data
  nrows = nrow(df)
  
  # Define statistic matrix column names 
  stats_names <- c("slope_5_days_InvGPP", "yint_5_days_InvGPP", "Fstatistic_5_days_InvGPP", 
                   "dF_5_days_InvGPP", "p_5_days_InvGPP", "slope_10_days_InvGPP", "yint_10_days_InvGPP", 
                   "Fstatistic_10_days_InvGPP", "dF_10_days_InvGPP", "p_10_days_InvGPP", 
                   "slope_15_days_InvGPP", "yint_15_day_InvGPPs", "Fstatistic_15_days_InvGPP", 
                   "dF_15_days_InvGPP", "p_15_days_InvGPP",
                   "slope_5_days_InvER", "yint_5_days_InvER", "Fstatistic_5_days_InvER", 
                   "dF_5_days_InvER", "p_5_days_InvER", "slope_10_days_InvER", "yint_10_days_InvER", 
                   "Fstatistic_10_days_InvER", "dF_10_days_InvER", "p_10_days_InvER", 
                   "slope_15_days_InvER", "yint_15_days_InvER", "Fstatistic_15_days_InvER", 
                   "dF_15_days_InvER", "p_15_days_InvER")
 
  # Preallocate matrix 
  stats <- matrix(nrow=nrows, ncol=30)
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
      
      # Calculate one set of statistics for each day window
      for(j in list(5,10,15)){
       
        # Get start and end dates for the range 
        start_date = df[i,"DATE"] - j
        end_date = df[i,"DATE"] + j
        
        # Get the data in the date range
        inrange <- site_df[site_df$DATE >= start_date &
                        site_df$DATE <= end_date, ]
       
        # Skip linear regression if there's only one sample
        if (nrow(inrange)==1){
          
          cat(sprintf('\n Row %s being excluded, only one sample', i))
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
                         anova(lmInvGPP)$'Pr(>F)'[1])
          
          ER_stats <- c(summary(lmInvER)$coefficients["STANDTEMP", "Estimate"],
                        summary(lmInvER)$coefficients["(Intercept)", "Estimate"],
                        summary(lmInvER)$fstatistic["value"],
                        summary(lmInvER)$fstatistic["numdf"],
                        anova(lmInvER)$'Pr(>F)'[1])
        }
        
        # Add to matrix 
        if (j == 5) {
          stats[i, 1:5] <- GPP_stats
          stats[i, 16:20] <- ER_stats
        } else if (j == 10) {
          stats[i, 6:10] <- GPP_stats
          stats[i, 21:25] <- ER_stats
        } else if (j == 15) {
          stats[i, 11:15] <- GPP_stats
          stats[i, 26:30] <- ER_stats
        }
      }
    }
  }
    
  # Convert matrix to dataframe
  stats_df <- as.data.frame(stats)
  
  # Horiontally concat with original df 
  overall_df <- cbind(df, stats_df)
  
  return(overall_df)
  
}

path = "Shiu_lab/for-others/seth_temp_sensitivity/Final_Data_Serena.csv"
arrhenius_stats(path)





