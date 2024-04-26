setwd("C:/Users/LucyBull/OneDrive - Health Navigator/Dokument/Admin/Interview_resources/SDS RWE Data Challenge Dataset/SDS RWE Data Challenge Dataset")
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)

# EXPLORATORY ANALYSIS
# import dataframes

clinical_data = read_csv("clinical_data.csv")
demographics = read_csv("demographics.csv")
bill_id = read_csv("bill_id.csv")
bill_amount = read_csv("bill_amount.csv")
clinical_data$patient_id

# dataframe join

full_data = clinical_data %>% left_join(demographics, 
                                        by=c('id'='patient_id')) %>% 
  mutate(
    gender = recode(gender, "f" = "Female", "m" = "Male"),
    date_of_admission = as.Date(as.POSIXct(date_of_admission, 
                                          format = "%d/%m/%y")),
    date_of_discharge =as.Date(as.POSIXct(date_of_discharge, 
                                          format = "%d/%m/%y")),
    date_of_birth = as.Date(as.POSIXct(date_of_birth, 
                                       format = "%Y-%M-%D")),
    gender = as.factor(gender),
    age_at_presentation = round(as.numeric(date_of_admission - date_of_birth, 
                                           units="days")/366,0),
    bed_days = date_of_discharge - date_of_admission,
    race = recode(race, "chinese" = "Chinese", "India" = "Indian"),
    resident_status = recode(resident_status, 
                             "Singapore citizen" = "Singaporean", 
                             "PR" = "Permanent Residency")) %>% distinct()

# time period coverage

full_data %>%
  count(date_of_admission = as.yearmon(date_of_admission, "%Y-%M-%D")) %>%
  mutate(count = n()) %>%
  ggplot(aes(date_of_admission, count)) +
  geom_line(size=1.5, colour="purple") + 
  scale_x_yearmon() + theme_bw() + xlab("Admission date (by month)") +
  ylab("Inpatien spell count")+ theme(aspect.ratio=1)

# completeness

sapply(full_data, function(x) sum(is.na(x)))
sapply(full_data, function(x) (sum(is.na(x))/length(x)*100))

# rows per inpatient stay. events are unique by patient id, admission date
# there are multiple bill_ids per patient
count_per_event_id = full_data %>% group_by(id, 
                                            admission_date) %>% summarise(count=n())
max(count_per_event_id$count)

# demographics (overall and by gender)

demographics_df = full_data %>% select(id, 
                                       date_of_birth, 
                                       gender,
                                       resident_status, 
                                       race) %>% distinct() %>%
  mutate(current_age = round(as.numeric(today() - date_of_birth, 
                                        units = "days")/366,0))

demographics_df %>% 
  summarise(count=n(), 
            mean_age = mean(current_age), 
            sd_age = sd(current_age))

demographics_df %>% 
  group_by(gender) %>% 
  summarise(count=n(), 
            mean_age = mean(current_age),
            sd_age = sd(current_age))

demographics_df %>% 
  group_by(gender, resident_status) %>% 
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

demographics_df %>% 
  group_by(gender, race) %>% 
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

demographics_df %>% 
  group_by(resident_status) %>% 
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

demographics_df %>% 
  group_by(race) %>% 
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

# Treatment frequency and combinations

cols = c('trt_anx', 'trt_con', 'trt_adt', 'trt_ssr', 'trt_oth', 'trt_the')
full_data = full_data %>% mutate(
  treatment_count = trt_anx+trt_con+trt_adt+trt_ssr+trt_the+trt_oth,
  treatment_combinations = apply(full_data[,cols ], 1 , paste0 , collapse = "" ))

combo_data = full_data %>% group_by(treatment_combinations) %>% 
  summarise(count = n()) %>%
  mutate(freq = count / sum(count)) %>% 
  arrange(desc(freq)) %>%
  mutate(cumsum = cumsum(freq))

full_data %>% ggplot(aes(x=treatment_count)) + 
  geom_histogram(binwidth=1, fill="purple") + 
  title("Treatment count distribution per patient inpatient spell") + 
  theme_bw() + xlab("Count of unique treatments") + ylab("Count") + 
  theme(aspect.ratio=1)

activity_per_patient = full_data %>% 
  group_by(id) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

activity_per_patient %>% ggplot(aes(x=count)) + 
  geom_boxplot(fill="purple") + 
  title("Count of unique inpatient spells per patient in dataframe") + 
  theme_bw() + xlab("Count of unique inpatient spells per patient") + 
  ylab("Count") + theme(aspect.ratio=1)

activity_per_patient %>% ggplot(aes(x=count)) + 
  geom_histogram(aes(y=..density..), binwidth=1, fill="purple") + 
  title("Count of unique inpatient spells per patient in dataframe") + 
  theme_bw() + xlab("Count of unique inpatient spells per patient") + 
  ylab("Count") + theme(aspect.ratio=1)


## STUDY ANALYSIS

# treatment cohort of interest 

cols = c('trt_anx', 'trt_con', 'trt_adt', 'trt_ssr', 'trt_oth', 'trt_the')
depressant_therapy = full_data %>% 
  filter((trt_adt == 1 & trt_the == 1) | (trt_ssr == 1 & trt_the == 1))

demographics_df = depressant_therapy %>% select(id, 
                                       date_of_birth, 
                                       gender,
                                       resident_status, 
                                       race) %>% distinct() %>%
  mutate(current_age = round(as.numeric(today() - date_of_birth, 
                                        units = "days")/366,0))

demographics_df %>% 
  summarise(count=n(), 
            mean_age = mean(current_age), 
            sd_age = sd(current_age))

demographics_df %>% 
  group_by(gender) %>% 
  summarise(count=n(), 
            mean_age = mean(current_age),
            sd_age = sd(current_age))

demographics_df %>% 
  group_by(gender, resident_status) %>% 
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

demographics_df %>% 
  group_by(gender, race) %>% 
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

demographics_df %>% 
  group_by(resident_status) %>% 
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

demographics_df %>% 
  group_by(race) %>% 
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

# prevalence of depressants and therapy standalone + additional therapies

combo_data_analysis = depressant_therapy %>% group_by(treatment_count) %>% 
  summarise(count = n()) %>%
  mutate(freq = count / sum(count)) %>% arrange(treatment_count) %>%
  mutate(cumsum = cumsum(freq))

combo_data_analysis %>% ggplot(aes(x=treatment_count, y=freq)) + 
  geom_bar(stat = 'identity', fill="darkblue") +
geom_text(aes(label=round(freq,2)), position=position_dodge(width=0.9), vjust=-0.2) +
  ylab("Proportion of inpatient episodes") +
  xlab("Unique treatment count") + theme_bw() + theme(aspect.ratio=1)

# mean length of stay inhospital 

depressant_therapy = depressant_therapy %>% mutate(
  patient_group = ifelse(treatment_count == 2, "only", "combination")
  )

depressant_therapy %>% ggplot(aes(x=bed_days)) +
  geom_boxplot(aes(fill=as.factor(patient_group))) + theme_bw() +
  scale_fill_manual(values=c("darkred", "darkblue")) +
  theme(aspect.ratio =1,
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) + 
  xlab("Inpatient spell duration (days)") +
  labs(fill="Patient group")

# Count of unique spells

depressant_therapy %>% filter(patient_group == "only") %>% group_by(id) %>%
  summarise(count = n()) %>% ggplot(aes(x = count)) + 
  geom_histogram(aes(y= ..density..), binwidth = 1, fill = "darkblue") +
  theme_bw() + xlab("Count of unique inpatient spells per patient") + 
  ylab("Count") + theme(aspect.ratio=1)

depressant_therapy %>% filter(patient_group == "combination") %>% 
  group_by(id) %>%
  summarise(count = n()) %>% ggplot(aes(x = count)) + 
  geom_histogram(aes(y= ..density..), binwidth = 1, 
                 fill = "darkred") +
  theme_bw() + xlab("Count of unique inpatient spells per patient") + 
  ylab("Count") + theme(aspect.ratio=1)

# improvement in CGIS 

depressant_therapy %>% mutate(improve_severity = cgis_adm - cgis_dis) %>% 
  filter(patient_group == "only") %>% 
  ggplot(aes(x = improve_severity)) + 
  geom_histogram(aes(y= ..density..), binwidth = 1, fill = "darkblue") +
  theme_bw() + xlab("Improvement in CGIS scale by discharge") + 
  ylab("Proportion") + theme(aspect.ratio=1) + xlim(c(-3,6))


depressant_therapy %>% mutate(improve_severity = cgis_adm - cgis_dis) %>% 
  filter(patient_group == "combination") %>% 
  ggplot(aes(x = improve_severity)) + 
  geom_histogram(aes(y= ..density..), binwidth = 1, fill = "darkred") +
  theme_bw() + xlab("Improvement in CGIS scale by discharge") + 
  ylab("Proportion") + theme(aspect.ratio=1) + xlim(c(-3,6))

# admission severity

depressant_therapy %>% 
  filter(patient_group == "only") %>% 
  ggplot(aes(x = cgis_adm)) + 
  geom_histogram(aes(y= ..density..), binwidth = 1, fill = "darkblue") +
  theme_bw() + xlab("MDD severity at admission (CGIS)") + 
  ylab("Proportion") + theme(aspect.ratio=1) + xlim(c(-3,6))


depressant_therapy %>% 
  filter(patient_group == "combination") %>% 
  ggplot(aes(x = cgis_adm)) + 
  geom_histogram(aes(y= ..density..), binwidth = 1, fill = "darkred") +
  theme_bw() + xlab("MDD severity at admission (CGIS)") + 
  ylab("Proportion") + theme(aspect.ratio=1) + xlim(c(-3,6))

