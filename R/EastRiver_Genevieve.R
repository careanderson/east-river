# R script to
# - read in MP-ICP-AES data
# - read in soil key
# - tidy and merge data
# - preliminary figures

# Carolyn Anderson April 2018

# Install packages (you only need to install the packages once)
#install.packages("tidyverse")
#install.packages("readxl")

# Load necessary packages (do this every time you start an R session)
library(tidyverse)
library(readxl)

# -----------------------------------------------------------------------------
# Read in and tidy the MP-ICP-AES data
# -----------------------------------------------------------------------------
# Use "read_excel" function to open the csv files.
# Note: change the path of the file to wherever it is on your computer.
# Note: skip=2 tells R to skip the first two rows, which don't have data.
# Note: the last section of this code selects only columns 1, 5, and 8, which have the data we want.
mp1 <- read.csv("Data/MP-ICP-AES/3April2018_GG_EastRiver_BulkSeq.csv", skip=2)[, c(1,5,8)]
mp2 <- read.csv("Data/MP-ICP-AES/5April2018_GG_EastRiver_BulkSeq.csv", skip=2)[, c(1,5,8)]

# Row-bind the two sample data frames together
mp <- rbind(mp1, mp2)

# Change the "Concentration" column to numeric (R reads it in as a factor)
mp$Concentration <- as.numeric(as.character(mp$Concentration))

# If the standard checks look good (e.g., "1ppm_check"), then select only samples.
# Note: All samples have "2x" in them, so tell R to find all rows with "2x" in them.
mp_samples <- mp[grep("2x", mp$Label), ]

# Separate the "Label" column into three separate columns (ID, extraction, dilution)
mp_samples <- mp_samples %>%
  separate(Label, c("ID", "extraction", "dilution", "other"), "_")

# If the sample blanks look good (e.g., concentrations are near zero, except for Na and P for PP and dith extractions), then these can be removed too.
mp_samples <- mp_samples[!grepl("blank", mp_samples$ID), ]

# If "error" is in the "other" column, remove those data.
mp_samples <- mp_samples[!grepl("error", mp_samples$other), ]

# -----------------------------------------------------------------------------
# Read in the sample ID data, and merge with the MP-ICP-AES data
# -----------------------------------------------------------------------------
# Read in the sample key file
# Note: change the path of the file to wherever it is on your computer.
sample_key <- read.csv("Data/June2017_EastRiver_samplekey.csv")

# Separate the "sample" column into two separate columns (location, rep)
sample_key <- sample_key %>%
  separate(sample, c("location", "rep"), "-")

# Separate the "depth_cm" column into two separate columns (top_cm, btm_cm)
sample_key <- sample_key %>%
  separate(depth_cm, c("top_cm", "btm_cm"), "-")

# Convert the soil depths to numeric, so R treats them as numbers (and not characters).
sample_key$top_cm <- as.numeric(sample_key$top_cm)
sample_key$btm_cm <- as.numeric(sample_key$btm_cm)

# Merge the MP-ICP-AES data with the sample key, using the common column "ID".
total <- merge(mp_samples, sample_key, by="ID")

# Write this file as a new csv
#write.csv(total, file = "mp-icp-aes_total_processed.csv")

# -----------------------------------------------------------------------------
# Prelim figures
# -----------------------------------------------------------------------------
# Convert the "extraction" and "location" to factors, so it's easier to plot.
total$extraction <- as.factor(total$extraction)
total$location <- as.factor(total$location)

# All data
ggplot(total) +
  aes(x=top_cm, y=Concentration, color=extraction) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Element.Label, scales = "free")

# Fe data
ggplot(subset(total, Element.Label %in% "Fe")) +
  aes(x=top_cm, y=Concentration) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~extraction, scales= "free")

# Al data
ggplot(subset(total, Element.Label %in% "Al")) +
  aes(x=top_cm, y=Concentration) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~extraction, scales= "free")

# Si data
ggplot(subset(total, Element.Label %in% "Si")) +
  aes(x=top_cm, y=Concentration) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~extraction, scales= "free")

# Mn data
ggplot(subset(total, Element.Label %in% "Mn")) +
  aes(x=top_cm, y=Concentration) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~extraction, scales= "free")


# gradient plot
ggplot(subset(total, Element.Label %in% "Fe")) +
  aes(x=top_cm, y=Concentration, color=location) +
  geom_point() +
#  geom_line() +
#  geom_smooth(method="lm") +
  facet_wrap(~extraction, scales= "free")


# -----------------------------------------------------------------------------
# Depth classes, plots, stats
# -----------------------------------------------------------------------------
# We want to encode "surface" and "subsurface" depth classes.
# These depths differ across the meander, due to differences in sedimentation/microtopography.
# Additionally, MC3 has three depths (surface, middle, subsurface).

# Surface depths (top < 30cm, for all)
# Mid-depths (45-75cm, MC3 only)
# Subsurface depths
  # 45-90cm, MC1 and MC2
  # 90-120cm, MC3

total$depth_class <- -999 #initiate a column for "depth_class"

total$depth_class <- ifelse(total$top_cm < 30, "surface",
                            ifelse(total$location == "MC3" & total$top_cm > 45 & total$top_cm < 75, "middle", "subsurface"))

# Make "depth_class" a factor, and order the depth classes
total$depth_class <- factor(total$depth_class, levels = c("surface", "middle", "subsurface"))


ggplot(subset(total, Element.Label %in% "Al")) +
  aes(x=depth_class, y=Concentration, color=location) +
  geom_point() +
  #  geom_line() +
  # geom_smooth(method="lm") +
  facet_wrap(~extraction, scales= "free")

ggplot(subset(total, Element.Label %in% "Fe")) +
  aes(x=location, y=Concentration, color=depth_class) +
  geom_point() +
# geom_line() +
#  geom_smooth(method="lm") +
  facet_wrap(~extraction, scales= "free")


# -----------------------------------------------------------------------------
# Test hypotheses about selective dissolution
# -----------------------------------------------------------------------------

# Subset by extraction
total_fe_pp <- subset(total, Element.Label %in% "Fe" & extraction %in% "pp")
total_al_pp <- subset(total, Element.Label %in% "Al" & extraction %in% "pp")

total_fe_hcl <- subset(total, Element.Label %in% "Fe" & extraction %in% "hcl")
total_al_hcl <- subset(total, Element.Label %in% "Al" & extraction %in% "hcl")

total_fe_dith <- subset(total, Element.Label %in% "Fe" & extraction %in% "dith")
total_al_dith <- subset(total, Element.Label %in% "Al" & extraction %in% "dith")

#1. We expect to see more HCl-extractable metals (e.g. ferrihydrite) in deeper soils that experience
#water table fluctuations. In other words, we expect more ferrihydrite in zones with rapid redox 
#fluctuations.

#HCl-Fe or Al vs. soil depth (continuous cm or categorical classes)

ggplot(subset(total, Element.Label %in% "Fe")) +
  aes(x=top_cm, y=Concentration, color=location) +
  geom_point() +
  # geom_line() +
  #  geom_smooth(method="lm") +
  facet_wrap(~extraction, scales= "free")

ggplot(subset(total, Element.Label %in% "Al")) +
  aes(x=top_cm, y=Concentration, color=location) +
  geom_point() +
  # geom_line() +
  #  geom_smooth(method="lm") +
  facet_wrap(~extraction, scales= "free")


# pyrophosphate
anova(lm(Concentration ~ top_cm, total_fe_pp))
anova(lm(Concentration ~ top_cm, total_al_pp))

anova(lm(Concentration ~ depth_class, total_fe_pp))
anova(lm(Concentration ~ depth_class, total_al_pp))


# hcl
anova(lm(Concentration ~ top_cm, total_fe_hcl))
anova(lm(Concentration ~ top_cm, total_al_hcl))

anova(lm(Concentration ~ depth_class, total_fe_hcl))
anova(lm(Concentration ~ depth_class, total_al_hcl))



#2. We expect to see more dithionite-extractable metals where there are more static redox conditions
#(in mostly oxic zones = soil surface). In other words, we expect more crystalline phases with less 
#redox fluctuations.

#dith-Fe or Al vs. soil depth (continuous cm or categorical classes)

# dith
anova(lm(Concentration ~ top_cm, total_fe_dith))
anova(lm(Concentration ~ top_cm, total_al_dith))

anova(lm(Concentration ~ depth_class, total_fe_dith))
anova(lm(Concentration ~ depth_class, total_al_dith))



# -----------------------------------------------------------------------------
# Combine with Total C and N data (bulk), test hypotheses
# -----------------------------------------------------------------------------
total_bulk_cn <- read.csv("Data/EA_data/12April2018_EA_bulk_processed.csv")[,-1]

# Manually changing sd of sample 60 to zero (will rerun on EA in replication to get actual sd)
# Double check data alignment
total_bulk_cn$C_sd[19] <- 0

# Prelim figure of total C profile across depth/meander, with standard deviations
ggplot(total_bulk_cn) +
  aes(x=top_cm, y=C_mean) + 
  xlab("Soil depth (cm)") + ylab("Total C in bulk soil (%)") +
  geom_errorbar(aes(ymin=C_mean-C_sd, ymax=C_mean+C_sd), width=0.8, color="grey") +
  geom_point(size = 2) +
  theme_bw() +
  theme(axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Merge data files
total_icp_cn <- merge(total_bulk_cn, total)


# Change extraction labels for the facet_wrap

extraction_names <- c(
  dith = "dithionite",
  h20 = "water",
  hcl = "HCl",
  pp = "pyrophosphate"
)

# Total C vs. extractions (Fe, Al)

ggplot(subset(total_icp_cn, Element.Label %in% "Fe")) +
  aes(x=Concentration, y=C_mean) +
  geom_point() +
  geom_smooth(method="lm") +
#  ggtitle("Total C in bulk soil vs. Fe ppm by extraction type") +
  xlab("Fe concentration (ppm), uncorrected") + ylab("Total C in bulk soil (%)") +
  facet_wrap(~extraction, scales="free", labeller = as_labeller(extraction_names)) +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=15))

ggplot(subset(total_icp_cn, Element.Label %in% "Al")) +
  aes(x=Concentration, y=C_mean) +
  geom_point() +
  geom_smooth(method="lm") +
#  ggtitle("Total C in bulk soil vs. Al ppm by extraction type") +
  xlab("Al concentration (ppm), uncorrected") + ylab("Total C in bulk soil (%)") +
  facet_wrap(~extraction, scales="free", labeller = as_labeller(extraction_names)) +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=15))



# Testing hypotheses:
#We expect to see more C in reactive metal surfaces
#-Using the total C data: if there's more C where we see more reactive minerals, then this is a 
#good indicator that we should see more C in reactive metal surfaces
#-Pyrophosphate- and HCl-extractable metals are a good predictor of C; use these data to predict C.

total_icp_cn_fe_hcl <- subset(total_icp_cn, Element.Label %in% "Fe" & extraction %in% "hcl")
total_icp_cn_fe_pp <- subset(total_icp_cn, Element.Label %in% "Fe" & extraction %in% "pp")

total_icp_cn_al_hcl <- subset(total_icp_cn, Element.Label %in% "Al" & extraction %in% "hcl")
total_icp_cn_al_pp <- subset(total_icp_cn, Element.Label %in% "Al" & extraction %in% "pp")


anova(lm(C_mean ~ Concentration, total_icp_cn_fe_hcl))
anova(lm(C_mean ~ Concentration, total_icp_cn_fe_pp))

anova(lm(C_mean ~ Concentration, total_icp_cn_al_hcl))
anova(lm(C_mean ~ Concentration, total_icp_cn_al_pp))



# -----------------------------------------------------------------------------
# Next steps
# -----------------------------------------------------------------------------
# Upload soil weights
# Divide by soil weight, divide by extraction volume (10ml for h2o, 16.7ml for others), multiply by dilution (2)
# Proportional MP data (total al, fe, etc.)

# Why doesn't water have triplicates??
# 3d plot with depth/distance
# 2d plot with depth/distance and size of point as amount of Fe