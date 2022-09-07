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
  
  # Formate DATE column as date objects
  df$DATE <- as.Date(df$DATE, format = "%m/%d/%y")
  
  # Get statistics 
  for(j in list(5,10,15)){

    cat(sprintf('\n\nGetting stats for %s day range', j))    
    
    # Get stats
    df_copy <- data.frame(df) # Make a copy, otherwise naming gets mega messed up
    df_w_stats <- calculate_stats(df_copy, j)
    
    cat('\nHead of data with stats added:')
    print(head(df_w_stats))
    
    # Save df as csv
    save_name <- paste(substr(path, 1, nchar(path)-4), j, "DAY_RANGE", 'arrhenius_data.csv', sep = '_')
    write.csv(df_w_stats, save_name, row.names = FALSE)
    
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
#' calculate_stats(df, dayrange)
calculate_stats <- function(df, dayrange) {
 
  # Get the number of rows in order to preallocate a matrix for storing data
  nrows = nrow(df)
  
  # Define statistic matrix column names 
  stats_names <- c("slope_LNGPP", "yint_LNGPP", "Fstatistic_LNGPP", 
                   "dF_LNGPP", "p_LNGPP", 
                   "slope_LNER", "yint_LNER", "Fstatistic_LNER", 
                   "dF_LNER", "p_LNER")
 
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
      target_date <- df[i,"DATE"]
      start_date = target_date - dayrange
      end_date = target_date + dayrange
      
      # Get the data in the date range
      inrange <- site_df[site_df$DATE >= start_date &
                      site_df$DATE <= end_date, ]
    
      # Split by datatype (GPP vs ER) and drop NA rows
      inrange_GPP <- na.omit(inrange[, c("DATE", "STANDTEMP", "LNGPP")]) # Will drop a row if it has a missing date, etc
      inrange_ER <- na.omit(inrange[,c("DATE", "STANDTEMP", "LNER")])
      
      # Calculate linear regressions for each
      rowstats <- c()
      for(frame in list(inrange_GPP, inrange_ER)){
      
        # Skip if only one sample
        if(!(target_date %in% frame[,"DATE"])){
          
          cat(sprintf(
            '\nRow %s being excluded for dtype %s, target date missing data', i, colnames(frame)[4]))
          rowstats <- append(rowstats, rep(NA, 5))
          
        } else if (nrow(frame)==1){
          
          cat(sprintf(
            '\nRow %s being excluded for dtype %s, only one sample', i, colnames(frame)[4]))
          rowstats <- append(rowstats, rep(NA, 5))
          
        } else if (length(unique(frame[,"STANDTEMP"]))==1) {
          
          cat(sprintf(
            '\nRow %s being excluded for dtype %s, temperatures are all identical', i, colnames(frame)[4]))
          rowstats <- append(rowstats, rep(NA, 5))
          
        } else {
          
          mylm <- lm(frame[,3] ~ frame[,2], data = frame)
          add_to_stats <- c(summary(mylm)$coefficients["frame[, 2]", "Estimate"],
                        summary(mylm)$coefficients["(Intercept)", "Estimate"],
                        summary(mylm)$fstatistic["value"],
                        summary(mylm)$fstatistic["numdf"],
                        suppressWarnings(anova(mylm)$'Pr(>F)'[1]))
          rowstats <- append(rowstats, add_to_stats)
        }
      }
      
      # Add stats to matrix
      stats[i, 1:10] <- rowstats
    }
  }
    
  # Convert matrix to dataframe
  stats_df <- as.data.frame(stats)
  
  # Add dayrange suffix on column headers
  colnames(stats_df) <- paste(colnames(stats_df), dayrange, "DAY_RANGE", sep = '_')
  
  # Horiontally concat with original df 
  overall_df <- cbind(df, stats_df)
  
  return(overall_df)
  
}

## COMMENT out these lines to run the tests
path = "Shiu_lab/arrhenius-stats/final_data/updated_analysis/Final_Data_SL.csv"
arrhenius_stats(path)





