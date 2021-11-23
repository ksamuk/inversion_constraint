# script for analysing the output of SLiM simulations
# 29-07-2021 / altered on 18-11-2021

library("tidyverse")
library("gridExtra") # for plotting

# read in simulation data -------------------------------------------------
setwd("/Users/Marius/Dropbox/My Mac (Marius’s MacBook Pro)/Desktop/inversion_constraint-main")

haplo_files.all <- list.files("01_simulations/data/output/haplotypes", full.names = TRUE)
fitness_files.all <- list.files("01_simulations/data/output/fitness", full.names = TRUE)

# check from how many simulations there are output files, and plot output for every individual simulation
seeds<-NULL
info<-NULL
for(i in 1:length(haplo_files.all)){
    seeds<-c(seeds,unlist(strsplit(haplo_files.all[i],split="_"))[4]) # retreive the unique seed-ID of each simulation file
    info<-c(info,paste0(unlist(strsplit(unlist(strsplit(haplo_files.all[i],split="_"))[2],split="/"))[5],"_",unlist(strsplit(haplo_files.all[i],split="_"))[3])) # simulation settings
}

#####################
# function for reading in a single simulation file
# and adding its meta data (from the file name) as columns
# handles haplotype ("hap") and fitness files separately

add_sim_info_as_columns <- function(x){
  
  # seed
  if(length(grep("haps",x))==1){
      
      seed <- gsub("*_haps.txt", "", x) %>% gsub(".*_", "", .)}
  
  if(length(grep("fitness",x))==1){
      
      seed <- gsub("*_fitness.txt", "", x) %>% gsub(".*_", "", .)}
  
  # simulation type (inversion present or absent?)
  ifelse(length(grep(".inv=1_",x))==1,sim_type <- "inversion.present",sim_type <- "inversion.absent")
  
  # mutation Rate
  mu <- gsub(".*.mutR=", "", x) %>% gsub(".recR.*.", "", .)
  
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
    
    # header for the plot containing all infos of the simulation
    header<-unlist(strsplit(gsub("*.inv*.*", "", fitness_files),"_"))[3]

    # (1) plot simulation data ----------------------------------------------------

    # plot of mean fitness vs generations
    fit_plot <- fitness_df %>%
        mutate(mu = paste0("mu=", mu)) %>%
        ggplot(aes(x = gen, y = mean_fitness, group = interaction(pop, seed), color = pop)) + # "pop" is just set so all panels are in line 
        geom_line(size = 0.3) +
        #facet_grid(mu~sim_type) + # option 1
        facet_grid(mu~pop)+
        theme_bw() +
        ylim(0.89,1.11) +
        xlab("Generation")+
        ylab("Mean fitness")+
        scale_color_brewer(palette = "Set1") +
        labs(title=header,color="population")
        
    #ggsave(paste0("02_analysis/plots/fitness_plot_",info[s],"_",seeds[s],".pdf"), plot = fit_plot, device = "pdf", width = 7, height = 5)

    # (2) plot of haplotype freq vs generations -----------------------------------
    
    haplo_palatte <- colorRampPalette(c("blue", "red"))(8)
    haplo_palatte <- haplo_palatte[c(1,1,4,4,4,4,8,8)]

    hap_plot <- haplo_df %>%
        mutate(sim_type = gsub("three_pop_", "", sim_type)) %>%
        mutate(mu = paste0("mu=", mu)) %>%
        filter(hap_class == "hap") %>%
        ggplot(aes(x = gen, y = frequency, group = interaction(pop, seed, haplotype), color = haplotype)) +
        geom_line(size = 0.2) +
        ylim(0,1) +
        facet_grid(mu~pop)+
        theme_bw() +
        xlab("Generation")+
        ylab("Frequency")+
        scale_color_manual(values = haplo_palatte) +
        labs(title="Detailed haplotype for the selected loci")

    #ggsave(paste0("02_analysis/plots/haplo_plot_",info[s],"_",seeds[s],".pdf"), plot = hap_plot, device = "pdf", width = 7, height = 5)

    
    # (3) plot haplotype-genotypes vs generations ----------------------------------
    hap_plot_B <- haplo_df %>%
        mutate(sim_type = gsub("three_pop_", "", sim_type)) %>%
        mutate(mu = paste0("mu=", mu)) %>%
        filter(hap_class == "hap")
     
     pops<-unique(hap_plot_B$pop)
     generations<-unique(hap_plot_B$gen)
     
     hap_plot_B_genotypes<-NULL
     for(g in 1:length(generations)){
        
        # This is "farmer-style coding", but it works ;-)
        # Collapse info into understandable haplotypes
        for(p in 1:length(pops)){
            sub<-hap_plot_B[intersect(which(hap_plot_B$gen==generations[g]),which(hap_plot_B$pop==pops[p])),]
            AA<-cbind.data.frame(sub[1,c(1:5)],"m1/m1",sum(sub$frequency[which(sub$haplotype == "113" | sub$haplotype == "114")]))
            names(AA)[6]<-"haplotype.genotype"
            names(AA)[7]<-"frequency"
            
            BB<-cbind.data.frame(sub[1,c(1:5)],"m2/m2",sum(sub$frequency[which(sub$haplotype == "223" | sub$haplotype == "224")]))
            names(BB)<-names(AA)
            
            AB<-cbind.data.frame(sub[1,c(1:5)],"m1/m2",sum(sub$frequency[which(sub$haplotype == "123" | sub$haplotype == "124")]))
            names(AB)<-names(AA)
            
            BA<-cbind.data.frame(sub[1,c(1:5)],"m2/m1",sum(sub$frequency[which(sub$haplotype == "213" | sub$haplotype == "214")]))
            names(BA)<-names(AA)
            
            hap_plot_B_genotypes<-rbind(hap_plot_B_genotypes,rbind(AA,AB,BA,BB))
            }
        }
        
        hap_geno_plot<-ggplot(hap_plot_B_genotypes,aes(x = gen, y = frequency, group = interaction(pop, seed, haplotype.genotype), color = haplotype.genotype)) +
        geom_line(size = 0.3) +
        ylim(0,1) +
        facet_grid(mu~pop) +
        theme_bw() +
        scale_color_manual(values=c("blue", "darkorchid1", "aquamarine3","red")) +
        xlab("Generation") +
        ylab("Frequency") +
        labs(title="Summarized haplotype for the selected loci", color="haplotype")

    #ggsave(paste0("02_analysis/plots/haplo_plot_",info[s],"_",seeds[s],".pdf"), plot = hap_plot, device = "pdf", width = 7, height = 5)
    
    # For plotting the inversion frequency within each population: it doesn't seem to be very informative
    hap_plot_inversion <- haplo_df %>%
        mutate(sim_type = gsub("three_pop_", "", sim_type)) %>%
        mutate(mu = paste0("mu=", mu)) %>%
        filter(haplotype == "inversion") %>%
        ggplot(aes(x = gen, y = frequency, group = interaction(pop, seed, hap_class), color = hap_class)) +
        geom_line(size = 0.2) +
        ylim(0,1) +
        facet_grid(mu~pop)+
        theme_bw() +
        xlab("Generation")+
        ylab("Frequency")+
        labs(title="Inversion genotype", color="genotype")

    # Plot the fitness-plot and th haplotype-plots underneath one another into the same plot
    pdf(paste0("02_analysis/plots/",info[s],"_",seeds[s],".pdf"), width = 10, height = 12)
    grid.arrange(fit_plot, hap_plot,hap_geno_plot, hap_plot_inversion, nrow=4, top = haplo_df[1,1])
    dev.off()
    
} # file-loop closes
