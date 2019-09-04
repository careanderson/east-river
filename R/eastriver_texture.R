# R script to read in particle size distribution data from laser diffraction

# Load libraries ----------------------
library(tidyverse)
library(readxl)
library(soiltexture)

# Load data ---------------------------

## Import multiple xls files from Coulter (particle size distrubtion via laser diffraction) to R ---------------------------
# set working directory
setwd("~/east-river/Data/texture/er_june2017/")

# list all xls files from the current directory
list.files(pattern="xls") # use the pattern argument to define a common pattern for import files with regex. Here: xls

# create a list from these files
list.filenames<-list.files(pattern="xls")

# create an empty list that will serve as a container to receive the incoming files
list.data<-list()

# create a loop to read in your data
for (i in 1:length(list.filenames)){
  list.data[[i]]<-read.delim(list.filenames[i], header = FALSE, stringsAsFactors = FALSE, quote = "", sep = "\t", skip = 5)
  }

# Add filename each individual xls file in list, rbind all files, rename columns, extract ID
names(list.data) <- list.filenames
texture <- bind_rows(list.data, .id = "filename") #binds all docs

colnames(texture) <- c("filename", "row_number", "diam_lower_um", "diam_upper_um", "diff_volume_percent", "cumulative_volume_percent")

# get only number value for ID, to match up with key (e.g. extract 17 from "er_june2017_17_01.$ls.xls")
texture$ID <- sapply(strsplit(texture$filename, "_"), function(x) x[3])

# Label sand, silt, and clay fractions ---------------------------
texture$particle_size <- ifelse(texture$diam_lower_um <= 2, "CLAY", 
                             ifelse(texture$diam_lower_um > 50, "SAND", "SILT"))

# Read in sample key ---------------------------
setwd("~/east-river/")
sample_key <- read.csv("Data/June2017_EastRiver_samplekey.csv")
# Separate the "sample" column into two separate columns (location, rep)
sample_key <- sample_key %>%
  separate(sample, c("location", "rep"), "-")

# Separate the "depth_cm" column into two separate columns (top_cm, btm_cm)
sample_key <- sample_key %>%
  separate(depth_cm, c("top_cm", "btm_cm"), "-")

# Remove "b" from rep column
sample_key$rep <- gsub("[^0-9\\.]", "", sample_key$rep)
#sample_key$location <- gsub("[^0-9\\.]", "", sample_key$location) 

# Convert the soil depths to numeric, so R treats them as numbers (and not characters).
sample_key$top_cm <- as.numeric(sample_key$top_cm)
sample_key$btm_cm <- as.numeric(sample_key$btm_cm)

texture_merge <- merge(texture, sample_key)

# Aggregate to get texture classes ---------------------------
texture_agg <- aggregate(diff_volume_percent~particle_size+ID+location+rep+top_cm+btm_cm, 
                  texture_merge, FUN=sum, na.rm=TRUE)

texture_agg$particle_size <- factor(texture_agg$particle_size, levels=c("SAND", "SILT", "CLAY"))

# Convert from long to wide data
texture_agg_wide <- spread(texture_agg, particle_size, diff_volume_percent)
texture_agg_wide$location_number <- ifelse(texture_agg_wide$location == "MC1", 1,
                                           ifelse(texture_agg_wide$location == "MC2", 2, 3))


#Export as csv ---------------------------
#write.csv(texture_agg_wide, "Data/texture/particle_size_wide_June2017_processed.csv")
#write.csv(texture_agg, "Data/texture/particle_size_long_June2017_processed.csv")

# Plot data ---------------------------
ggplot(texture) +
  aes(x=diam_lower_um, y=diff_volume_percent, color=id) +
  geom_line(size=2)

ggplot(texture_agg_wide) +
  aes(x=top_cm, y=SILT, color=location) +
  geom_point() + geom_smooth(method="lm")

ggplot(texture_agg) +
  aes(x=particle_size, y=diff_volume_percent) +
  geom_boxplot() +
  facet_wrap(~location) +
  ylab("volume (%)") +
  theme_bw() +
  theme(axis.text = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 25))

ggplot(texture_agg) +
  aes(x=top_cm, y=diff_volume_percent, color=particle_size) +
  geom_point() + geom_smooth(method="lm") +
  facet_wrap(~location) +
  ylab("volume (%)") + xlab("soil depth (cm)") +
  theme_bw() +
  theme(axis.text = element_text(size = 25),
        axis.title = element_text(size = 25),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 25))

# Soil texture triangle
#https://cran.r-project.org/web/packages/soiltexture/vignettes/soiltexture_vignette.pdf
TT.plot(
  class.sys = "USDA-NCSS.TT",
  tri.data = texture_agg_wide,
  #z.name = "location_number",
  main = "East River floodplain texture"
)

# Recompute some internal values:
z.cex.range <- TT.get("z.cex.range")
def.pch <- par("pch")
def.col <- par("col")
def.cex <- TT.get("cex")
oc.str <- TT.str(
  texture_agg_wide[,"location_number"],
  z.cex.range[1],
  z.cex.range[2]
) #


# The legend:
legend(
  x = 80,
  y = 90,
  title =
    expression( bold('OC [g.kg'^-1 ~ ']') ),
  legend = formatC(
    c(
      min( texture_agg_wide[,"location_number"] ),
      quantile(texture_agg_wide[,"location_number"] ,probs=c(25,50,75)/100),
      max( texture_agg_wide[,"location_number"] )
    ),
    format = "f",
    digits = 1,
    width = 4,
    flag = "0"
  ), #
  pt.lwd = 4,
  col = def.col,
  pt.cex = c(
    min( oc.str ),
    quantile(oc.str ,probs=c(25,50,75)/100),
    max( oc.str )
  ), #,
  pch = def.pch,
  bty = "o",
  bg = NA,
  #box.col = NA, # Uncomment this to remove the legend box
  text.col = "black",
  cex = def.cex
)
