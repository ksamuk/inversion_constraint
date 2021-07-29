# script for analysing the output of SLiM simulations
# 29-07-2021

library("tidyverse")

# read in simulation data -------------------------------------------------

haplo_files <- list.files("../01_simulations/output/haplotypes", full.names = TRUE)
fitness_files <- list.files("../01_simulations/output/fitness", full.names = TRUE)

# function for reading in a single simulation file
# and adding its meta data (from the file name) as columns
# handles haplotype ("hap") and fitness files separately

add_sim_info_as_columns <- function(x){
  
  seed <- gsub(".*_", "", x) %>% gsub("[^0-9]*", "", .)
  sim_type <- gsub(".*/", "", x) %>% gsub("_mu=.*", "", .)
  mu <- gsub(".*_mu=", "", x) %>% gsub("_.*", "", .) 
  
  out_df <- read.table(x)
  out_df <- data.frame(sim_type, seed, mu, out_df)
  
  
  if (grepl("hap", x)){
    
    names(out_df) <- c("sim_type", "seed", "mu", "gen", "pop", "hap_class", "haplotype", "frequency")
    
  } else if (grepl("fitness", x)){
    
    names(out_df) <- c("sim_type", "seed", "mu", "gen", "pop", "mean_fitness")
    
  }
  
  out_df
  
}

# read in haplotype frequencies
haplo_df <- lapply(haplo_files, add_sim_info_as_columns) %>%
  bind_rows() 

# read in fitness data
fitness_df <- lapply(fitness_files, add_sim_info_as_columns) %>%
  bind_rows() 

# plot simulation data ----------------------------------------------------

# plot of mean fitness vs generations 
fit_plot <- fitness_df %>%
  mutate(sim_type = gsub("three_pop_", "", sim_type)) %>%
  mutate(mu = paste0("mu=", mu)) %>%
  ggplot(aes(x = gen, y = mean_fitness, group = interaction(pop, seed), color = pop)) +
  geom_line(size = 0.2) +
  facet_grid(mu~sim_type)+
  theme_bw()+
  xlab("Generation")+
  ylab("Mean fitness")+
  scale_color_brewer(palette = "Set1") 

ggsave("plots/fitness_plot.pdf", plot = fit_plot, device = "pdf", width = 7, height = 5)

# plot of haplotype freq vs generations

haplo_palatte <- colorRampPalette(c("blue", "red"))(8)
haplo_palatte <- haplo_palatte[c(1,1,4,4,4,4,8,8)]


hap_plot <- haplo_df %>%
  mutate(sim_type = gsub("three_pop_", "", sim_type)) %>%
  mutate(mu = paste0("mu=", mu)) %>%
  filter(hap_class == "hap") %>%
  filter(sim_type == "inversions") %>%
  ggplot(aes(x = gen, y = frequency, group = interaction(pop, seed, haplotype), color = haplotype)) +
  geom_line(size = 0.2) +
  facet_grid(mu~pop)+
  theme_bw()+
  xlab("Generation")+
  ylab("Mean fitness")+
  scale_color_manual(values = haplo_palatte) 

ggsave("plots/haplo_plot.pdf", plot = hap_plot, device = "pdf", width = 7, height = 5)


