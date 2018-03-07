# Functions for PP data

repo_data_to_Supp_data <- function(data){
  data <- data %>% 
    filter(period > 0, period < 436, #remove negative periods and periods after plot switch
           year > 1987, #remove before first plot switch
           plot > 0)
  
  ## make dataframe look like Sarah's raw data
  
  # new columns
  data$Treatment_Number = NA
  data$east = NA
  data$north = NA
  
  # remove unneeded columns
  data <- select(data, -recordID, -day, -note1, -age, -testes, -lactation, -hfl, 
                 -(prevrt:note4))
  
  # reorganize columns
  data <- data[, c("year", "month", "period", "Treatment_Number", 
                   "plot", "stake", "east", "north", "species", "sex", 
                   "reprod", "vagina", "nipples", "pregnant", "wgt",
                   "tag", "note2", "ltag", "note3", "note5")]
  
  for (i in 1:length(data$period)){
    if (data$plot[i] %in% c(1, 2, 4, 8, 9, 11, 12, 14, 17, 22)){
      data$Treatment_Number[i] = 1
    } else if (data$plot[i] %in% c(3, 6, 13, 15, 18, 19, 20, 21 )){
      data$Treatment_Number[i] = 2
    } else {
      data$Treatment_Number[i] = 3 # 5, 7, 10, 16, 23, 24
    }
  }
  return(data)
}

clean_data_for_capture_histories <- function(data){
  
  # from Sarah Supp's code
  #   - uses functions found in movement_fxns.r
  
  # make sure text is text and not atomic
  data$tag = as.character(data$tag) 
  data$sex = as.character(data$sex)
  data$species = as.character(data$species)
  
  # give untagged indivs unique tag numbers (7 digits)
  # small_rodents = id_unknowns(small_rodents)        
  
  # get list of unique tags
  tags = unique(data$tag)   
  
  # output list of flagged data
  flags = find_bad_spp_data(data, tags, 9)

  # get list of unique "bad tags"
  badtags=unique(flags$tag)
  
  # delete bad tags from dataset for analysis
  for (i in 1:length(badtags)) {
    data = subset(data, tag != badtags[i])
  }
  return(data)
}
