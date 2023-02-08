rm(list=ls())
library(ggplot2)
library(dplyr)
library(usmap)


setwd('/Users/ishitagopal/Box/science_backed_policy_diffusion/')

# read dataset with signing frequency data  
df <- read.csv("analysis/objects/legislator_by_state_updated.csv")

# match state abbreviations to fips code using 'fips' function in usmap
df$fips <- fips(df$Var1)

# subset data to just contain column with fips code and corresponding frequencies
df.plot <- df %>%
  select(fips, Freq)

# plot us map with frequencies 
p <- plot_usmap(regions = 'state', data = df.plot, values = "Freq")

# edit appearance
p <- p +
  scale_fill_continuous(name="", 
                        low = "lightgrey", high = "black", na.value = "white") +
  labs(title="") + 
  
  theme_bw() +
  
  #eliminates background, gridlines, and chart border, axis labels, 
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_blank()
  )
  
p

ggsave(p, file="analysis/Images/signing_legislators.pdf",
       width = 111, height = 77, units = 'cm', scale = .20)



# ggsave 
# those who are in the office; in the backbone network 
# Database of legislators; indicate when they retired; if they have retired 
# state legislators website 
# Search NCEL and find how many letters have been signed in the last year 
                        