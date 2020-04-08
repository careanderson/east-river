# R script to read in and analyze East River ferrozine assay data

# Load libraries ----------------------
library(tidyverse)
library(readxl)
library(data.table)

# Set wd ---------------------------
setwd("~/east-river/")

# Load your data  ---------------------------
# Load the ferrozine assay data
fe_total <- read_excel("Data/ferrozine/2019-09-04_total_fe_ferrozine_ER2018_hcl_extracts.xlsx")[,c(1:9)]
fe2 <- read_excel("Data/ferrozine/2019-09-05_fe2_ferrozine_ER2018_hcl_extracts.xlsx")[,c(1:9)]

# Remove _1 or _2 from sample ids:
fe_total$id_2018 <- gsub("-.*", "", fe_total$id_2018)
fe2$id_2018 <- gsub("-.*", "", fe2$id_2018)

# id_2018 as numeric
fe_total$id_2018 <- as.numeric(fe_total$id_2018)
fe2$id_2018 <- as.numeric(fe2$id_2018)

# Fe2: Remove original samples we had to re-run at different dilutions
# Samples: 18, 126, 129, 81, 73, 134, 41, 37, 77, 51, 42, 23, 20, 54, 49, 26, 124, 35, 12
re_run_ids <- c(18, 126, 129, 81, 73, 134, 41, 37, 77, 51, 42, 23, 20, 54, 49, 26, 124, 35, 12)
fe2_reruns <- subset(fe2, id_2018 %in% re_run_ids)
fe2_reruns_keep <- subset(fe2_reruns, Batch %in% "highorlow")

# Remove re-runs
fe2_noreruns <- fe2[ ! fe2$id_2018 %in% re_run_ids, ]

# Add in the "keep" reruns
fe2_processed <- rbind(fe2_noreruns, fe2_reruns_keep)
  
# rbind the Fe total & Fe2 dataframes
fe_all <- rbind(fe_total, fe2_processed)
colnames(fe_all)
colnames(fe_all)[c(1,2)] <- c("id", "extraction")

# Load soil weights used in extraction
# [ insert code to load soil weights here ]

# Calculate umol Fe  ---------------------------
# Multiply by extraction volume (e.g. 10 mL HCl) and divide by grams of soil (e.g. 1 g)
# NOTE: If you have exact soil weights, load them above and merge them with your assay data, and divide by the exact weights
fe_all$fe_um_g_soil <- (fe_all$fe_um_corrected_for_dilution * 0.01) / 1 # 10 ml HCl, 1 g soil

# Remove columns we no longer need
colnames(fe_all)
fe_short <- fe_all[,c(1,2,10)]

# Take means of the repeats
keys <- colnames(fe_short)[!grepl("fe_um_g_soil", colnames(fe_short))]

fe_short <- as.data.table(fe_short)

fe_short_means <- fe_short[,list(fe_um_g_soil= mean(fe_um_g_soil)),keys]

# Read in sample key ---------------------------
key <- read.csv("Sample_lists/summer2018-ERsediment_all.csv")
colnames(key)[4] <- "id"

# Change the date column to format "as.Date"
key$date <- as.Date(key$date, format = "%m/%d/%Y")

# Add depth_category column
key$depth_category <- -999

key$depth_category <-
  ifelse(key$top_depth_cm < 10, "surface",
         ifelse(key$top_depth_cm > 25 & key$top_depth_cm < 35, "shallow",
                ifelse(key$top_depth_cm > 65 & key$top_depth_cm < 75, "subsurface", "deep")))

key$depth_category <- factor(key$depth_category, levels=c("surface", "shallow", "subsurface", "deep"))

# Merge
ferro_all <- merge(fe_short_means, key)

# Subset as needed  ---------------------------
# NOTE: you don't need to do this if you're using all the data
# E.g., subsetting for these dilutions: total = 1:10; fe2 = 1:2
#ferro_total <- subset(ferro_all , fe_assay == "total" & dilution == "10")
#ferro_2 <- subset(ferro_all, fe_assay == "fe2" & dilution == "2")
#ferro_all_subset <- rbind(ferro_total, ferro_2)
#ferro_all_subset <- ferro_all_subset[,-3] #removing dilution column

# Fraction of Fe2 over total iron  ---------------------------
# Long to wide
ferro_wide <- spread(ferro_all, extraction, fe_um_g_soil)
ferro_wide$fraction_fe2_per_fe_total <- ferro_wide$fe2 / ferro_wide$total
colnames(ferro_wide)
ferro_wide_short <- ferro_wide[,c(1,9:11)]

# Convert back to long
ferro_long <- gather(ferro_wide, extraction, fe_um_g_soil, total, fe2, fraction_fe2_per_fe_total)

# Aggregate to average means
colnames(ferro_long)
ferro_long_agg <- aggregate(fe_um_g_soil~location+date+depth_category+extraction, ferro_long, FUN=mean)


# Plotting  ---------------------------
ggplot(subset(ferro_long, extraction %in% c("fe2", "total"))) +
  aes(x=depth_category, y=fe_um_g_soil, color=extraction) +
  geom_boxplot()

ggplot(subset(ferro_long, extraction %in% c("fe2", "total"))) +
  aes(x=depth_category, y=fe_um_g_soil, color=extraction) +
  geom_boxplot() +
  facet_wrap(~location)

ggplot(subset(ferro_long, extraction %in% c("fraction_fe2_per_fe_total"))) +
  aes(x=depth_category, y=fe_um_g_soil) +
  geom_boxplot() +
  ylab("fraction Fe(II) per total Fe")

ggplot(subset(ferro_long, extraction %in% c("fraction_fe2_per_fe_total"))) +
  aes(x=depth_category, y=fe_um_g_soil) +
  geom_boxplot() +
  ylab("fraction Fe(II) per total Fe") +
  facet_wrap(~location)

ggplot(subset(ferro_long, extraction %in% c("fe2", "total"))) +
  aes(x=depth_category, y=fe_um_g_soil, color=extraction) +
  geom_boxplot() +
  facet_wrap(~as.factor(date))

ggplot(subset(ferro_long, extraction %in% c("fraction_fe2_per_fe_total"))) +
  aes(x=depth_category, y=fe_um_g_soil, color=extraction) +
  geom_boxplot() +
  facet_wrap(~as.factor(date)) +
  ylab("fraction Fe(II) per total Fe")

# for GCC poster
ferro_long$location_depth <- with(ferro_long, paste0(location, depth_category))

f <- ggplot(subset(ferro_long, location_depth %in% c("MCT2-1shallow", "MCT2-3surface") & extraction %in% c("fraction_fe2_per_fe_total"))) +
  aes(x=date, y=fe_um_g_soil) +
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~location+depth_category) +
  ylab("fraction Fe(II) per total Fe") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        #strip.text.x = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none",
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

f <- arrangeGrob(f) #generates g
ggsave(file="ferrozine_gcc.pdf", f, width = 5, height = 3, units = "in")

f1 <- ggplot(subset(ferro_long, location_depth %in% c("MCT2-1shallow") & extraction %in% c("fraction_fe2_per_fe_total"))) +
  aes(x=date, y=fe_um_g_soil) +
  geom_point() + geom_smooth(method="lm") +
  ylab("fraction Fe(II) per total Fe") +
  ylim(0.03,0.6) +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        #strip.text.x = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none",
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

f2 <- ggplot(subset(ferro_long, location_depth %in% c("MCT2-3surface") & extraction %in% c("fraction_fe2_per_fe_total"))) +
  aes(x=date, y=fe_um_g_soil) +
  geom_point() + geom_smooth() +
  ylab("fraction Fe(II) per total Fe") +
  ylim(0.03,0.6) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        #strip.text.x = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none",
        strip.text = element_blank(),
        axis.text.x = element_text(size = 15, angle = 45, vjust = 1, hjust=1))

# Put the plots in the same rows (mc1 and mc3
ferro_1row <- grid.arrange(f1, f2, nrow = 1)
g <- arrangeGrob(ferro_1row) #generates g
ggsave(file="ferro1row.pdf", g, width = 6, height = 3, units = "in")







# Stats
ferro_mc1_shallow <- subset(ferro_long, location_depth %in% c("MCT2-1shallow") & extraction %in% c("fraction_fe2_per_fe_total"))
plot(ferro_mc1_shallow$date, ferro_mc1_shallow$fe_um_g_soil)
ferro_mc1_shallow_model <- lm(fe_um_g_soil ~ date, data=ferro_mc1_shallow)
abline(ferro_mc1_shallow_model, col = "red")
summary(ferro_mc1_shallow_model)

install.packages("forecast")
library(forecast)
ferro_mc3_surface <- subset(ferro_long, location_depth %in% c("MCT2-3surface") & extraction %in% c("fraction_fe2_per_fe_total"))
plot(ferro_mc3_surface$date, ferro_mc3_surface$fe_um_g_soil)
ferro_mc3_surface_model <- lm(fe_um_g_soil ~ date, data=ferro_mc3_surface)
abline(ferro_mc3_surface_model, col = "red")
summary(ferro_mc3_surface_model)

ferro_mc3_surface$date <- as.numeric(ferro_mc3_surface$date)

loessMod10 <- loess(fe_um_g_soil ~ date, data=ferro_mc3_surface) # 10% smoothing span
summary(loessMod10)


ggplot(ferro_long) +
  aes(x=date, y=fe_um_g_soil, color=depth_category) +
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~location+extraction, scales="free")

ggplot(ferro_long) +
  aes(x=date, y=fe_um_g_soil, color=location) +
  geom_point() + geom_smooth(method="lm") +
  facet_grid(vars(depth_category), vars(extraction), scales="free")

# Using aggregated data (average of 3 reps)
ggplot(ferro_long_agg) +
  aes(x=date, y=fe_um_g_soil, color=depth_category) +
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~location+extraction, scales="free")
