# function takes a dataframe suitable for icc calculation (e.g., columns = raters, rows = subjects)
permu_ICC <- function(df, i) {
  library(tidyverse)
  library(irr)
  list <- names(df) # gets names of all raters
  comblist <- combn(list, i) # creates every possible combination of two
  listdf <- as_tibble(t(comblist)) %>% rownames_to_column() # transposes this list and creates an index variable "rownames"
  permu_df <- tibble() # empty dataframe; will fill later
  for (j in listdf$rowname) { # starts loop
    subset_list <- listdf[which(listdf$rowname == j),] # goes down index variable, selects a row with two names
    subset_df <- df %>% select(paste0(subset_list[,-1])) # selects the raters, drops rowname
    
    #### change ICC here ####
    # can change model, type, and/or unit
    subset_model <- icc(subset_df, model = "twoway", type = "consistency", unit = "average") 
    #~~~~~~~~~~~~~~~~~~~~~~~#
    
    number <- 1:ncol(subset_list[1,-1])
    subset_raters <- subset_list[1,-1] %>% rename_with(.fn = ~paste0("rater_", number))
    subset_result <- tibble("ICC" = subset_model$value,
                            "p" = subset_model$p.value,
                            "CI_95_LL" = subset_model$lbound,
                            "CI_95_UL" = subset_model$ubound)
    subset_result <- cbind(subset_raters, subset_result)
    permu_df <- rbind(permu_df, subset_result) 
  }
  permu_df <- permu_df[order(permu_df$ICC, decreasing = TRUE),]
  row.names(permu_df) <- NULL
  print(permu_df)
}

### you can now use the function to load results! ###

# Examples:
# permu_df(df, 2) ## ICCs for every possible permutation of two
# permu_df(df, 3) ## for three

# if you need more specifics, use the icc() function for the full results summary
# for icc() you will need to specify which columns (e.g., which raters) you need from your dataframe


