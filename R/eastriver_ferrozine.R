# R script to read in and analyze East River ferrozine assay data

# Load libraries ----------------------
library(tidyverse)
library(readxl)

# Set wd ---------------------------
setwd("~/east-river/")

# Load your data  ---------------------------
# Load the ferrozine assay data
fe <- read_excel("Data/ferrozine/2019-08-29_ferrozine_ER2018_hcl_extracts.xlsx")[,c(1:7)]

# Load soil weights used in extraction
# [ insert code to load soil weights here ]

# Calculate umol Fe  ---------------------------
# Multiply by extraction volume (e.g. 10 mL HCl) and divide by grams of soil (e.g. 1 g)
# NOTE: If you have exact soil weights, load them above and merge them with your assay data, and divide by the exact weights
fe$fe_um_g_soil <- (fe$fe_um_corrected_for_dilution * 0.01) / 1 # 10 ml HCl, 1 g soil

# Remove columns we no longer need
colnames(fe)
fe_short <- fe[,c(1,2,6,8)]

# Read in sample key ---------------------------
key <- read.csv("Sample_lists/summer2018-ERsediment_all.csv")
colnames(key)[4] <- "id_2018"

# Change the date column to format "as.Date"
key$date <- as.Date(key$date, format = "%m/%d/%y")

# Add depth_category column
key$depth_category <- -999

key$depth_category <-
  ifelse(key$top_depth_cm < 10, "surface",
         ifelse(key$top_depth_cm > 25 & key$top_depth_cm < 35, "shallow",
                ifelse(key$top_depth_cm > 65 & key$top_depth_cm < 75, "middle", "deep")))

key$depth_category <- factor(key$depth_category, levels=c("surface", "shallow", "middle", "deep"))

# Merge
ferro_all <- merge(fe_short, key)

# Subset as needed  ---------------------------
# NOTE: you don't need to do this if you're using all the data
# E.g., subsetting for these dilutions: total = 1:10; fe2 = 1:2
ferro_total <- subset(ferro_all , fe_assay == "total" & dilution == "10")
ferro_2 <- subset(ferro_all, fe_assay == "fe2" & dilution == "2")
ferro_all_subset <- rbind(ferro_total, ferro_2)
ferro_all_subset <- ferro_all_subset[,-3] #removing dilution column

# Fraction of Fe2 over total iron  ---------------------------
# Long to wide
ferro_wide <- spread(ferro_all_subset, fe_assay, fe_um_g_soil)
ferro_wide$fraction_fe2_per_fe_total <- ferro_wide$fe2 / ferro_wide$total

# Convert back to long
ferro_long <- gather(ferro_wide, fe_assay, fe_um_g_soil, fe2, total, fraction_fe2_per_fe_total)

# Plotting  ---------------------------
ggplot(subset(ferro_long, fe_assay %in% c("fe2", "total"))) +
  aes(x=depth_category, y=fe_um_g_soil, color=fe_assay) +
  geom_boxplot()

ggplot(subset(ferro_long, fe_assay %in% c("fraction_fe2_per_fe_total"))) +
  aes(x=depth_category, y=fe_um_g_soil) +
  geom_boxplot() +
  ylab("fraction Fe(II) per total Fe")

ggplot(subset(ferro_long, fe_assay %in% c("fe2", "total"))) +
  aes(x=depth_category, y=fe_um_g_soil, color=fe_assay) +
  geom_boxplot() +
  facet_wrap(~as.factor(date))

ggplot(subset(ferro_long, fe_assay %in% c("fraction_fe2_per_fe_total"))) +
  aes(x=depth_category, y=fe_um_g_soil, color=fe_assay) +
  geom_boxplot() +
  facet_wrap(~as.factor(date)) +
  ylab("fraction Fe(II) per total Fe")
