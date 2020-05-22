# R script to read in and analyze ECoGSS data

# Load libraries ----------------------
library(tidyverse)
library(readxl)
library(data.table)
library(RColorBrewer)

# Set wd ---------------------------
setwd("~/east-river/")

# Load your data  ---------------------------
# Load the ferrozine assay data
eco18 <- read_excel("Data/ecogss_survey/2018_ECoGSS_Evaluation.xlsx")[,c(3, 5:7, 26)]
eco19 <- read_excel("Data/ecogss_survey/2019_ECoGSS_Evaluation.xlsx")[-2,c(2, 4:7)] #removing faculty member who did not respond to survey

# Rename colunmns
colnames(eco18) <- c("student_fac_interact", "camaraderie", "comm_skills", "research_div", "id")
colnames(eco19) <- c("id", "student_fac_interact", "camaraderie", "comm_skills", "research_div")

# Add year column to each
eco18$year <- "2018"
eco19$year <- "2019"

# rbind the dataframes
eco_all <- rbind(eco18, eco19)
eco_all$year <- as.factor(eco_all$year)

# only grad & faculty
eco_all_grad <- subset(eco_all, id %in% c("Faculty member", "Graduate student"))

# Gather the data
eco_long <- eco_all_grad %>% gather(question, score, 1:4)

# total grads 2018 = 24
nrow(subset(eco_all_grad, id %in% "Graduate student" & year %in% 2018))
# total faculty 2018 = 9
nrow(subset(eco_all_grad, id %in% "Faculty member" & year %in% 2018))

# total grads 2019 = 17
nrow(subset(eco_all_grad, id %in% "Graduate student" & year %in% 2019))
# total faculty 2019 = 5
nrow(subset(eco_all_grad, id %in% "Faculty member" & year %in% 2019))

# total grads = 41
# total faculty = 14

# if separating by year
eco_long$total_number <- ifelse(eco_long$id %in% "Graduate student" & eco_long$year %in% 2018, "24",
                                ifelse(eco_long$id %in% "Faculty member" & eco_long$year %in% 2018, "9",
                                       ifelse(eco_long$id %in% "Graduate student" & eco_long$year %in% 2019, "17", "5")))
eco_long$total_number <- as.numeric(eco_long$total_number)

# if combining years
eco_long$total_number_noyear <- ifelse(eco_long$id %in% "Graduate student", "41", "14")
eco_long$total_number_noyear <- as.numeric(eco_long$total_number_noyear)


# Aggregate to get counts of each id/year/question/score combo & percentage
eco_agg <- aggregate(eco_long, by = list(eco_long$id, eco_long$year, eco_long$question, eco_long$total_number), FUN=mean)

#eco_agg <- aggregate(score~id+year+question+score+total_number,eco_long,FUN=NROW)

# Gets list with count column
eco_agg <- eco_long %>%
  group_by(id, year, question, score, total_number) %>% 
  mutate(count = n())%>%
  distinct(id, year, question, score, total_number, count)
eco_agg$percent <- eco_agg$count / eco_agg$total_number


eco_agg_noyear <- eco_long %>%
  group_by(id, question, score, total_number_noyear) %>% 
  mutate(count = n())%>%
  distinct(id, question, score, total_number_noyear, count)
eco_agg_noyear$percent <- eco_agg_noyear$count / eco_agg_noyear$total_number_noyear

# Change labels
eco_agg$likert <- ifelse(eco_agg$score %in% "1", "Strongly disagree",
                         ifelse(eco_agg$score %in% "2", "Disagree",
                                ifelse(eco_agg$score %in% "3", "Neither agree nor disagree",
                                       ifelse(eco_agg$score %in% "4", "Agree", "Strongly Agree"))))

eco_agg_noyear$likert <- ifelse(eco_agg_noyear$score %in% "1", "Strongly disagree",
                                ifelse(eco_agg_noyear$score %in% "2", "Disagree",
                                       ifelse(eco_agg_noyear$score %in% "3", "Neither agree nor disagree",
                                              ifelse(eco_agg_noyear$score %in% "4", "Agree", "Strongly Agree"))))

# order of likert
eco_agg$likert <- factor(eco_agg$likert,
                                  levels=rev(c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly Agree")))
eco_agg_noyear$likert <- factor(eco_agg_noyear$likert,
                         levels=rev(c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly Agree")))

# Change facet labels questions
question.labs <- c("...encourage camaraderie in the Department?", "...provide adequate opportunities to practice communication?", "...showcase the diversity of research in the ECo Department?", "...encourage student-faculty interactions?")
names(question.labs) <- c("camaraderie", "comm_skills", "research_div", "student_fac_interact")

# Change order of facets
eco_agg_noyear$question <- factor(eco_agg_noyear$question,
                                  levels=c("comm_skills", "research_div", "student_fac_interact", "camaraderie"))

# figures
display.brewer.all(colorblindFriendly = TRUE)

# with full question
ggplot(eco_agg_noyear) +
  aes(x=id, y=percent, fill=likert) +
  geom_bar(stat="identity", width=0.8) +
  coord_flip() +
  facet_wrap(~question, labeller = labeller(question = question.labs)) +
  scale_fill_brewer(palette = "RdYlBu")

# with no facet labels
f <- ggplot(eco_agg_noyear) +
  aes(x=id, y=percent, fill=likert) +
  geom_bar(stat="identity", width=0.8) +
  coord_flip() +
  facet_grid(.~question) +
  scale_fill_brewer(palette = "RdYlBu") +
  xlab("") + ylab("% of survey respondents") +
  scale_y_continuous(breaks=c(0, 0.5, 1), labels = scales::percent) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        axis.text.x = element_text(angle = 35, hjust=0.85),
        panel.spacing = unit(1, "lines"))

f <- arrangeGrob(f) #generates f
ggsave(file="ecogss_survey.pdf", f, width = 11, height = 2, units = "in")




# melt the data
eco_melt <- melt(eco_all_grad, id=c("year", "id"))

head(eco_melt$id)

# figures
ggplot(subset(eco_melt, id %in% c("Faculty member", "Graduate student"))) +
  aes(x=variable, y=value, color=id) +
  geom_violin() +
  facet_wrap(~year)