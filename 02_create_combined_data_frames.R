
################################################################################
# Process simulation data and create combined data files for plotting
# Marius Roesti, Kieran Samuk, Kim Gilbert
# April 2022
################################################################################

library("tidyverse")
library("parallel")
library("inline")
options(scipen = 999)


haplo_files <- list.files("data/slim_output/haplotypes", full.names = TRUE)
fitness_files <- list.files("data/slim_output/fitness", full.names = TRUE)

################################################################################
# function for reading in a single simulation file
# and adding its metadata (from the file name) as columns
# handles haplotype ("hap") and fitness files separately
################################################################################

add_sim_info_as_columns <- function(x){
  
  # simulation type (polygenic or basic)
  
  if(grepl("polygenic", x)){
    
    sim_type <- "polygenic"
    
  } else if (grepl("clim", x)){
    
    sim_type <- ("clim_change")

  } else{
    
    sim_type <- ("novel_environment")

  } 
         
  
  # migration rate
  migR <- gsub(".*.migR=", "", x) %>% gsub(".popS.*.", "", .)
  
  # population size
  popS <- gsub(".*.popS=", "", x) %>% gsub(".mutR.*.", "", .)
  
  # mutation rate
  mutR <- gsub(".*.mutR=", "", x) %>% gsub(".recR.*.", "", .)
  
  # recombination rate
  recR <- gsub(".*.recR=", "", x) %>% gsub(".selC.*.", "", .)
  
  # strength of selection
  selC <- gsub(".*.selC=", "", x) %>% gsub(".inv.*.", "", .)
  
  # inversion active (suppresses recombination?)
  ifelse(length(grep(".inv=1_",x))==1,
         inv_active <- "inversion.present", inv_active <- "inversion.absent")
  
  # seed value
  if(length(grep("haps",x))==1){
    
    seed <- gsub("*_haps.txt", "", x) %>% gsub(".*_", "", .)}
  
  if(length(grep("fitness",x))==1){
    
    seed <- gsub("*_fitness.txt", "", x) %>% gsub(".*_", "", .)}
  
  # read in data and add metadata as columns
  out_df <- read.table(x)
  out_df <- data.frame(sim_type, seed, inv_active, mutR, migR, 
                       popS, recR, selC, out_df)
  
  
  if (grepl("hap", x)){
    
    names(out_df) <- c("sim_type", "seed", "inv_active", "mut_rate", "mig_rate", "pop_size", 
                       "rec_rate", "sel_str", "gen", "pop", "hap_class", "haplotype", 
                       "frequency")
    
  } else if (grepl("fitness", x)){
    
    names(out_df) <- c("sim_type", "seed", "inv_active", "mut_rate", "mig_rate", "pop_size", 
                       "rec_rate", "sel_str", "gen", "pop", "mean_fitness", 
                       "optimal_adaptation")
    
  }
  
  out_df
  
}

################################################################################
# Apply the add_sim_info_as_columns to the SLiM simulation data
################################################################################

# read in haplotype frequencies and apply the processing function
haplo_df <- lapply(haplo_files, add_sim_info_as_columns) %>%
  bind_rows() %>%
  type_convert()


# normalize fitness values for polygenic case

haplo_df <- haplo_df %>%
  mutate(total_sel_str = ifelse(sim_type == "polygenic", 
                                round(sel_str * 40, digits = 2), sel_str * 2)) %>%
  select("sim_type", "seed", "inv_active", "mut_rate", "mig_rate", "pop_size", 
         "rec_rate", "sel_str","total_sel_str", "gen", "pop", "hap_class", "haplotype", 
         "frequency")

write.table(haplo_df, file = "data/slim_combined_haplo_df.txt", 
            row.names = FALSE, quote = FALSE)
system("gzip data/slim_combined_haplo_df.txt")

# same for fitness data
fitness_df <- lapply(fitness_files, add_sim_info_as_columns) %>%
  bind_rows()%>%
  type_convert()

fitness_df  <- fitness_df  %>%
  mutate(total_sel_str = ifelse(sim_type == "polygenic", 
                                round(sel_str * 40, digits = 2), sel_str * 2)) %>%
  select("sim_type", "seed", "inv_active", "mut_rate", "mig_rate", "pop_size", 
         "rec_rate", "sel_str", "total_sel_str", "gen", "pop", "mean_fitness", 
         "optimal_adaptation")

write.table(fitness_df, file = "data/slim_combined_fitness_df.txt", 
            row.names = FALSE, quote = FALSE)


  