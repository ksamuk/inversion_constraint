# script for analysing the output of SLiM simulations
# 29-07-2021

library("tidyverse")

# read in simulation data -------------------------------------------------
setwd("/Users/Marius/Dropbox/My Mac (Marius’s MacBook Pro)/Desktop/slim_out")
#haplo_files <- list.files("../01_simulations/output/haplotypes", full.names = TRUE)
haplo_files.all <- list.files("output/haplotypes", full.names = TRUE)
#fitness_files <- list.files("../01_simulations/output/fitness", full.names = TRUE)
fitness_files.all <- list.files("output/fitness", full.names = TRUE)

# check from how many simulations there are output files, and plot output for every individual simulation
seeds<-NULL
info<-NULL
for(i in 1:length(haplo_files.all)){
    seeds<-c(seeds,unlist(strsplit(haplo_files.all[i],split="__"))[2])
    info<-c(info,unlist(strsplit(unlist(strsplit(haplo_files.all[i],split="__"))[1],split="/"))[3])
}

#####################
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
###################

for(s in 1:length(seeds)){ # file-loop opens
    haplo_files<-haplo_files.all[s]
    fitness_files<-fitness_files.all[s]
    
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

    ggsave(paste0("plots/fitness_plot_",info[s],"_",seeds[s],".pdf"), plot = fit_plot, device = "pdf", width = 7, height = 5)

    # plot of haplotype freq vs generations
    
    haplo_palatte <- colorRampPalette(c("blue", "red"))(8)
    haplo_palatte <- haplo_palatte[c(1,1,4,4,4,4,8,8)]

    hap_plot <- haplo_df %>%
        mutate(sim_type = gsub("three_pop_", "", sim_type)) %>%
        mutate(mu = paste0("mu=", mu)) %>%
        filter(hap_class == "hap") %>%
        filter(sim_type == "inversion.present") %>% # switch off if inversion was turned off in the simulation
        ggplot(aes(x = gen, y = frequency, group = interaction(pop, seed, haplotype), color = haplotype)) +
        geom_line(size = 0.2) +
        facet_grid(mu~pop)+
        theme_bw()+
        xlab("Generation")+
        ylab("Frequency")+
        scale_color_manual(values = haplo_palatte)

    ggsave(paste0("plots/haplo_plot_",info[s],"_",seeds[s],".pdf"), plot = hap_plot, device = "pdf", width = 7, height = 5)

} # file-loop closes

