# Execution time tracking
start_time <- Sys.time()

library(tidyverse)

dat <- read_csv("stipends.csv")

## Abstract

print("This R code provides a visualization of PhD stipends in the US, offering 
insights into the variability and trends in financial support provided to 
doctoral students across different departments and years. The code generates 
two figures. The first figure is a horizontal bar chart that showcases the 
departments ranked by their average PhD stipends, highlighting the leading 
academic fields in terms of financial support. The second figure illustrates 
the trends in average PhD stipends across different years, capturing the 
evolution of financial support over time.")

## Data Cleaning

cat("Data Cleaning ... \n")
# Overall Pay
dat$`Overall Pay` <- gsub("\\$|,|-", "", dat$`Overall Pay`)
dat$`Overall Pay` <- as.numeric(dat$`Overall Pay`)

# Based on research, PhD stipends in the US range between $12,000 and $62,000.
# We will retain only those entries where the stipend falls within this range.

dat <- dat[dat$`Overall Pay` >= 12000 & dat$`Overall Pay` <= 62000,]

# Academic Year

table(dat$`Academic Year`)

# Given that the number of entries for each year before 2014 is less than 100, 
# I focus our analysis on the records from the years 2014 to 2023.
years <- 2014:2023
year_ranges <- sapply(1:(length(years) - 1), function(i) 
  paste(years[i], years[i + 1], sep = "-"))
dat <- dat[dat$`Academic Year` %in% year_ranges,]

# Department
dat$dept <- dat$Department
dat <- dat %>% 
  filter(!is.na(dept)) %>%
  filter(dept != "n/a")

# Convert to lower case
dat$dept <- tolower(dat$dept)

# Group dept
dat$dept <- str_replace_all(dat$dept, ".{0,}biology.{0,}", "biology")
dat$dept <- str_replace_all(dat$dept, ".{0,}microbi.{0,}", "biology")
dat$dept <- str_replace_all(dat$dept, ".{0,}molecular.{0,}", "biology")
dat$dept <- str_replace_all(dat$dept, ".{0,}biomed.{0,}", "biomed")
dat$dept <- str_replace_all(dat$dept, "bme", "biomed")
dat$dept <- str_replace_all(dat$dept, "dbbs", "biomed")
dat$dept <- str_replace_all(dat$dept, ".{0,}neuroscience.{0,}", "neuroscience")
dat$dept <- str_replace_all(dat$dept, ".{0,}chem.{0,}", "chemistry")
dat$dept <- str_replace_all(dat$dept, ".{0,}environment.{0,}", "environment")
dat$dept <- str_replace_all(dat$dept, ".{0,}earth.{0,}", "earth science")
dat$dept <- str_replace_all(dat$dept, ".{0,}physics.{0,}", "physics")
dat$dept <- str_replace_all(dat$dept, ".{0,}nuclear.{0,}", "physics")
dat$dept <- str_replace_all(dat$dept, ".{0,}business.{0,}", "business")
dat$dept <- str_replace_all(dat$dept, ".{0,}english.{0,}", "english")
dat$dept <- str_replace_all(dat$dept, ".{0,}geo.{0,}", "geoscience")
dat$dept <- str_replace_all(dat$dept, ".{0,}french.{0,}", "language")
dat$dept <- str_replace_all(dat$dept, ".{0,}hispanic.{0,}", "language")
dat$dept <- str_replace_all(dat$dept, ".{0,}german.{0,}", "language")
dat$dept <- str_replace_all(dat$dept, ".{0,}language.{0,}", "language")
dat$dept <- str_replace_all(dat$dept, ".{0,}spanish.{0,}", "language")
dat$dept <- str_replace_all(dat$dept, ".{0,}portuguese.{0,}", "language")
dat$dept <- str_replace_all(dat$dept, ".{0,}human.{0,}", "human development")
dat$dept <- str_replace_all(dat$dept, ".{0,}health.{0,}", "health")
dat$dept <- str_replace_all(dat$dept, ".{0,}education.{0,}", "education")
dat$dept <- str_replace_all(dat$dept, ".{0,}electrical.{0,}", "electrical engineering")
dat$dept <- str_replace_all(dat$dept, ".{0,}nutrition.{0,}", "nutrition")
dat$dept <- str_replace_all(dat$dept, ".{0,}food.{0,}", "nutrition")
dat$dept <- str_replace_all(dat$dept, ".{0,}immuno.{0,}", "imnunology")
dat$dept <- str_replace_all(dat$dept, ".{0,}informati.{0,}", "information science")
dat$dept <- str_replace_all(dat$dept, ".{0,}computer engineering.{0,}", "electrical engineering")
dat <- dat %>% mutate(dept = ifelse(dept == "ee", "electrical engineering", dept))
dat <- dat %>% mutate(dept = ifelse(dept == "ece", "electrical engineering", dept))
dat$dept <- str_replace_all(dat$dept, ".{0,}electrical.{0,}", "electrical engineering")
dat$dept <- str_replace_all(dat$dept, ".{0,}marine.{0,}", "marine sciences")
dat$dept <- str_replace_all(dat$dept, ".{0,}material.{0,}", "material science")
dat$dept <- str_replace_all(dat$dept, ".{0,}math.{0,}", "mathematics")
dat$dept <- str_replace_all(dat$dept, ".{0,}genetics.{0,}", "genetics and genomics")
dat$dept <- str_replace_all(dat$dept, ".{0,}genom.{0,}", "genetics and genomics")
dat$dept <- str_replace_all(dat$dept, ".{0,}mechanical.{0,}", "mechanical engineering")
dat$dept <- str_replace_all(dat$dept, ".{0,}medical.{0,}", "medical science")
dat$dept <- str_replace_all(dat$dept, ".{0,}school of medicine.{0,}", "medical science")
dat$dept <- str_replace_all(dat$dept, ".{0,}mstp.{0,}", "medical science")
dat$dept <- str_replace_all(dat$dept, ".{0,}film.{0,}", "film and media")
dat$dept <- str_replace_all(dat$dept, ".{0,}cinema.{0,}", "film and media")
dat$dept <- str_replace_all(dat$dept, ".{0,}media.{0,}", "film and media")
dat$dept <- str_replace_all(dat$dept, ".{0,}econ.{0,}", "economics")
dat$dept <- str_replace_all(dat$dept, ".{0,}psychology.{0,}", "psychology")
dat$dept <- str_replace_all(dat$dept, ".{0,}psychological.{0,}", "psychology")
dat$dept <- str_replace_all(dat$dept, ".{0,}comput.{0,}", "computer science")
dat <- dat %>% mutate(dept = ifelse(dept == "cs", "computer science", dept))
dat <- dat %>% mutate(dept = ifelse(dept == "eecs", "computer science", dept))
dat$dept <- str_replace_all(dat$dept, ".{0,}epidemi.{0,}", "epidemiology")
dat$dept <- str_replace_all(dat$dept, ".{0,}gender.{0,}", "gender studies")
dat$dept <- str_replace_all(dat$dept, ".{0,}operation.{0,}", "operations")
dat$dept <- str_replace_all(dat$dept, ".{0,}history.{0,}", "history")
dat$dept <- str_replace_all(dat$dept, ".{0,}philosophy.{0,}", "philosophy")
dat$dept <- str_replace_all(dat$dept, ".{0,}agricult.{0,}", "algruiculture")
dat$dept <- str_replace_all(dat$dept, ".{0,}plant.{0,}", "algruiculture")
dat$dept <- str_replace_all(dat$dept, ".{0,}soil.{0,}", "algruiculture")
dat$dept <- str_replace_all(dat$dept, ".{0,}journalism.{0,}", "journalism")
dat$dept <- str_replace_all(dat$dept, ".{0,}pharma.{0,}", "pharmaceutical science")
dat$dept <- str_replace_all(dat$dept, ".{0,}ocean.{0,}", "marine sciences")
dat$dept <- str_replace_all(dat$dept, ".{0,}nano.{0,}", "material science")
dat$dept <- str_replace_all(dat$dept, ".{0,}political.{0,}", "political science")
dat$dept <- str_replace_all(dat$dept, ".{0,}politics.{0,}", "political science")
dat$dept <- str_replace_all(dat$dept, ".{0,}policy.{0,}", "political science")
dat$dept <- str_replace_all(dat$dept, ".{0,}nursing.{0,}", "nursing")
dat$dept <- str_replace_all(dat$dept, ".{0,}public ad.{0,}", "public administration")
dat$dept <- str_replace_all(dat$dept, ".{0,}aerospace.{0,}", "aerospace")
dat$dept <- str_replace_all(dat$dept, ".{0,}aero.{0,}", "aerospace")
dat$dept <- str_replace_all(dat$dept, ".{0,}civil.{0,}", "civil engineering")
dat$dept <- str_replace_all(dat$dept, ".{0,}social.{0,}", "sociology")
dat$dept <- str_replace_all(dat$dept, ".{0,}sociology.{0,}", "sociology")
dat$dept <- str_replace_all(dat$dept, ".{0,}statistics.{0,}", "statistics")
dat$dept <- str_replace_all(dat$dept, ".{0,}communication.{0,}", "communication")
dat$dept <- str_replace_all(dat$dept, ".{0,}speech.{0,}", "communication")
dat$dept <- str_replace_all(dat$dept, ".{0,}theolog.{0,}", "theology")
dat$dept <- str_replace_all(dat$dept, ".{0,}theater.{0,}", "theatre & performance")
dat$dept <- str_replace_all(dat$dept, ".{0,}theatre.{0,}", "theatre & performance")
dat$dept <- str_replace_all(dat$dept, ".{0,}ecology.{0,}", "ecology")
dat$dept <- str_replace_all(dat$dept, ".{0,}criminology.{0,}", "criminology")
dat$dept <- str_replace_all(dat$dept, ".{0,}criminal.{0,}", "criminology")
dat$dept <- str_replace_all(dat$dept, ".{0,}biological science.{0,}", "biology")
dat$dept <- str_replace_all(dat$dept, ".{0,}bioscience.{0,}", "biology")
dat$dept <- str_replace_all(dat$dept, ".{0,}american.{0,}", "american studies")
dat$dept <- str_replace_all(dat$dept, ".{0,}music.{0,}", "music")
dat$dept <- str_replace_all(dat$dept, ".{0,}kinesiology.{0,}", "kinesiology")
dat$dept <- str_replace_all(dat$dept, ".{0,}urban.{0,}", "urban planning")
dat$dept <- str_replace_all(dat$dept, ".{0,}marketing.{0,}", "marketing")
dat$dept <- str_replace_all(dat$dept, ".{0,}management.{0,}", "management")
dat$dept <- str_replace_all(dat$dept, ".{0,}linguistics.{0,}", "linguistics")
dat$dept <- str_replace_all(dat$dept, ".{0,}bioengin.{0,}", "biology")
dat$dept <- str_replace_all(dat$dept, ".{0,}biological engin.{0,}", "biology")
dat$dept <- str_replace_all(dat$dept, ".{0,}industrial.{0,}", "industrial engineering")
dat$dept <- str_replace_all(dat$dept, ".{0,}accounting.{0,}", "accounting")
dat$dept <- str_replace_all(dat$dept, ".{0,}animal.{0,}", "animal science")
dat$dept <- str_replace_all(dat$dept, ".{0,}religio.{0,}", "religious studies" )
dat$dept <- str_replace_all(dat$dept, ".{0,}literature.{0,}", "literature" )
dat$dept <- str_replace_all(dat$dept, ".{0,}government.{0,}", "government" )
dat$dept <- str_replace_all(dat$dept, ".{0,}classics.{0,}", "classical studies" )
dat$dept <- str_replace_all(dat$dept, ".{0,}entomology.{0,}", "entomology" )

cat("Done. \n")
write.csv(dat, "stipends_cleaned.csv")

end_time <- Sys.time()
cat("#------------------------#")
cat("\n")
cat("Execution time")
cat("\n")
cat(end_time - start_time)
cat("\n")
cat("#------------------------#")
cat("\n")
cat(" Session Info")
print(sessionInfo())