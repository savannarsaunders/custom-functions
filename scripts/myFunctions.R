# My functions

library(devtools)
library(rlang)
library(tidyverse)
library(patchwork)

## EMCRMP Data/functions from Jeremiah
pak::pak('jeremiaheb/rvc')
library(rvc)

# getting data
USVI <- getRvcData(years = 2017:2025, regions = c("STTSTJ", "STX"))
# 
# 
# bio <- getDomainBiomass(x = USVI, species = "EPI GUTT")
# den <- getDomainDensity(x = USVI, species = "EPI GUTT")
# occ <- getDomainOccurrence(x = USVI, species = "EPI GUTT")


# Separated Functions -----------------------------------------------------

# Biomass
bio_var_graph <- function(USVI_df, species) {
  
  bio <- getDomainBiomass(x = {{USVI_df}}, species = {{species}})
  
  bio_var <- bio |> 
    group_by(YEAR, REGION, SPECIES_CD, biomass) |> 
    summarise(
      SE = sqrt(var)) |> 
    ungroup()
  
  biograph <- ggplot(bio_var, aes(YEAR, biomass, color = REGION))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin = biomass - SE, 
                      ymax = biomass + SE), width = 0.2)+
    labs(
      x = "Year",
      y = bquote("Biomass (Kg/177" * m^2 * ")"),
      title = bquote(italic(.(species))))+
    theme_minimal()+
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
      legend.position = "bottom")
  biograph
}

bio_graph <- bio_var_graph(USVI, "EPI GUTT")

# Density
den_var_graph <- function(USVI_df, species) {
  den <- getDomainDensity(x = {{USVI_df}}, species = {{species}})
  
  den_var <- den |> 
    group_by( YEAR, REGION, SPECIES_CD, density) |> 
    summarise(
      SE = sqrt(var)) |> 
    ungroup()
  
  dengraph <- ggplot(den_var, aes(YEAR, density, color = REGION))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin = density - SE, 
                      ymax = density + SE), width = 0.2)+
    labs(
      x = "Year",
      y = bquote("Density (Ind./177" * m^2 * ")"),
      title = bquote(italic(.(species))))+
    theme_minimal()+
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
      legend.position = "bottom")
  dengraph
}

den_graph <- den_var_graph(USVI, "EPI GUTT")


# Occurrence 
occ_var_graph <- function(USVI_df, species) {
  occ <- getDomainOccurrence(x = {{USVI_df}}, species = {{species}})
  
  occ_var <- occ |> 
    group_by( YEAR, REGION, SPECIES_CD, occurrence) |> 
    summarise(
      SE = sqrt(var)) |> 
    ungroup()
  
  occgraph <- ggplot(occ_var, aes(YEAR, occurrence, color = REGION))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin = occurrence - SE, 
                      ymax = occurrence + SE), width = 0.2)+
    labs(
      x = "Year",
      y = "Occurrence",
      title = bquote(italic(.(species))))+
    theme_minimal()+
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
      legend.position = "bottom")
  occgraph
}

occ_graph <- occ_var_graph(USVI, "EPI GUTT")

# Combined Function -------------------------------------------------------
com_para_graph <- function(USVI_df, species) {

  USVI <- {{USVI_df}}
  species <- {{species}}
  
  #biomass graph
  bio_var_graph <- function(USVI_df, species) {
    
    bio <- getDomainBiomass(x = {{USVI_df}}, species = {{species}})
    
    bio_var <- bio |> 
      group_by(YEAR, REGION, SPECIES_CD, biomass) |> 
      summarise(
        SE = sqrt(var)) |> 
      ungroup()
    
    biograph <- ggplot(bio_var, aes(YEAR, biomass, color = REGION))+
      geom_line()+
      geom_point()+
      geom_errorbar(aes(ymin = biomass - SE, 
                        ymax = biomass + SE), width = 0.2)+
      labs(
        x = "Year",
        y = bquote("Biomass (Kg/177" * m^2 * ")"),
        title = bquote(italic(.(species))))+
      theme_minimal()+
      theme(
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        legend.position = "none")
    biograph
  }
    
  bio_graph <- bio_var_graph(USVI, "EPI GUTT")
    
  ## Density graph
  den_var_graph <- function(USVI_df, species) {
    den <- getDomainDensity(x = {{USVI_df}}, species = {{species}})
    
    den_var <- den |> 
      group_by( YEAR, REGION, SPECIES_CD, density) |> 
      summarise(
        SE = sqrt(var)) |> 
      ungroup()
    
    dengraph <- ggplot(den_var, aes(YEAR, density, color = REGION))+
      geom_line()+
      geom_point()+
      geom_errorbar(aes(ymin = density - SE, 
                        ymax = density + SE), width = 0.2)+
      labs(
        x = "Year",
        y = bquote("Density (Ind./177" * m^2 * ")"),
        title = bquote(italic(.(species))))+
      theme_minimal()+
      theme(
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        legend.position = "none")
    dengraph
  }
  
  den_graph <- den_var_graph(USVI, "EPI GUTT")
    
  
  ## Occurrence 
  occ_var_graph <- function(USVI_df, species) {
    occ <- getDomainOccurrence(x = {{USVI_df}}, species = {{species}})
    
    occ_var <- occ |> 
      group_by( YEAR, REGION, SPECIES_CD, occurrence) |> 
      summarise(
        SE = sqrt(var)) |> 
      ungroup()
    
    occgraph <- ggplot(occ_var, aes(YEAR, occurrence, color = REGION))+
      geom_line()+
      geom_point()+
      geom_errorbar(aes(ymin = occurrence - SE, 
                        ymax = occurrence + SE), width = 0.2)+
      labs(
        x = "Year",
        y = "Occurrence",
        title = bquote(italic(.(species))))+
    theme_minimal()+
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
      legend.position = "bottom")
  occgraph
  }
  
  occ_graph <- occ_var_graph(USVI, "EPI GUTT")
  
  
  com <- bio_graph / den_graph / occ_graph
  
  ggsave("outputs/parameter_graph.png", com, height = 7, width = 5)
  print(com)
}

com_para_graph(USVI, "EPI GUTT")

# Most Portable Function --------------------------------------------------

metric_graph <- function(dataframe, metric, species) {
  if(metric == "biomass"){
    metric_df <- getDomainBiomass(x = dataframe, species = species) |>  
      group_by( YEAR, REGION, SPECIES_CD, biomass) |> 
      summarise(
        SE = sqrt(var))
    
    yvar <- "biomass"
    ylabel <- bquote("Biomass (Kg/177" * m^2 * ")")
    
  }else if(metric == "density"){
    metric_df <- getDomainDensity(x = dataframe, species = species) |> 
      group_by(YEAR, REGION, SPECIES_CD, density) |> 
      summarise(
        SE = sqrt(var))
    
    yvar <- "density"
    ylabel <- bquote("Density (Ind./177" * m^2 * ")")
    
  }else if(metric == "occurrence"){
    metric_df <- getDomainOccurrence(x = dataframe, species = species) |> 
      group_by(YEAR, REGION, SPECIES_CD, occurrence) |> 
      summarise(
        SE = sqrt(var))
    
    yvar <- "occurrence"
    ylabel <- "Occurrence"
  }
  
  
  metricgraph <- ggplot(metric_df, aes(YEAR, .data[[yvar]], color = REGION))+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin = .data[[yvar]] - SE, 
                      ymax = .data[[yvar]] + SE), width = 0.2)+
    labs(
      x = "Year",
      y = ylabel,
      title = bquote(italic(.(species))))+
    theme_minimal()+
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
      legend.position = "bottom")
  
metricgraph
}


species_BioDenOcc_graph <- function(dataframe, species) {
  biograph <- metric_graph(dataframe, "biomass", species)+
    theme(
      legend.position = "none")
  
  dengraph <- metric_graph(dataframe, "density", species) +
    theme(
      legend.position = "none",
      plot.title = element_blank())
  
  occgraph <- metric_graph(dataframe, "occurrence", species)+
    theme(
    plot.title = element_blank())
  
  comb_graph <- biograph/dengraph/occgraph
  print(comb_graph)
}

species_BioDenOcc_graph(USVI, "BAL VETU")
