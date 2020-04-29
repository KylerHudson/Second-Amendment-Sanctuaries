# ----------------------------------------------------
#   PS 904: New Approaches
#   Research Paper
#   Analysis of Second Amendment Sanctuaries
# ----------------------------------------------------

# ---- packages -----------------------

library("here")
library("tidyverse")
library("readxl")
library("broom")
library("pscl")
library("knitr")
library("kableExtra")
library("ggplot2")
library("reshape2")

# ---- reading data -----------------------

# raw sanctuary data
sanctuary_raw <- 
  read_excel(here("data", "SanctuaryData.xlsx"),
             col_types = c(
               "FIPS" = "text",
               "State" = "text",
               "County" = "text",
               "ResolutionPassed" = "numeric",
               "PassageDate" = "date",
               "Source" = "text",
               "ResolutionRepealed" = "numeric",
               "StatewideSanctuary" = "numeric"
             )) %>%
  print()

# raw rural atlas data
rural_atlas_people_raw <- 
  read_excel(here("data", "RuralAtlasData-People.xlsx")) %>% 
  print()

rural_atlas_veterans_raw <- 
  read_excel(here("data", "RuralAtlasData-Veterans.xlsx")) %>% 
  print()

rural_atlas_countyclass_raw <- 
  read_excel(here("data", "RuralAtlasData-CountyClass.xlsx")) %>% 
  print()

rural_atlas_income_raw <- 
  read_excel(here("data", "RuralAtlasData-Income.xlsx")) %>% 
  print()

rural_atlas_jobs_raw <- 
  read_excel(here("data", "RuralAtlasData-Jobs.xlsx")) %>% 
  print()

#raw TW ideology data
TW_ideology_raw <- 
  read_csv(here("data", "TWIdeology.csv")) %>% 
  print()

#state legislature data
state_leg <- 
  read_excel(here("data", "StateControlData.xlsx")) %>% 
  print()

# ---- select columns ---------------------

rural_atlas_people <- 
  select(rural_atlas_people_raw,
         FIPS,
         Under18Pct2010,
         Age65AndOlderPct2010,
         WhiteNonHispanicPct2010,
         HispanicPct2010,
         BlackNonHispanicPct2010,
         NonHispanicWhitePopChangeRate0010,
         Ed1LessThanHSPct,
         Ed2HSDiplomaOnlyPct,
         Ed4AssocDegreePct,
         Ed5CollegePlusPct,
         ForeignBornPct,
         PopDensity2010,
         TotalPopEstBase2010,
         LandAreaSQMiles2010)

rural_atlas_veterans <-
  select(rural_atlas_veterans_raw,
         FIPS,
         Vets18OPct)

rural_atlas_countyclass <- 
  select(rural_atlas_countyclass_raw,
         FIPS,
         RuralUrbanContinuumCode2013,
         Perpov_1980_0711,
         Type_2015_Manufacturing_NO,
         Type_2015_Farming_NO,
         Type_2015_Government_NO)

rural_atlas_income <- 
  select(rural_atlas_income_raw,
         FIPS,
         PovertyAllAgesPct,
         MedHHInc,
         PerCapitaInc)
         
rural_atlas_jobs <-
  select(rural_atlas_jobs_raw,
         FIPS,
         UnempRate2018,
         PctEmpServices,
         PctEmpManufacturing,
         PctEmpAgriculture,
         PctEmpConstruction,
         PctEmpMining)

TW_ideology <- 
  select(TW_ideology_raw,
         county_fips,
         mrp_ideology_mean,
         mrp_sd,
         mrp_lower,
         mrp_upper)

# ------ clean ----------------------

TW_ideology <- rename(TW_ideology, FIPS = county_fips)

TW_ideology$FIPS <- as.character(TW_ideology$FIPS)

TW_ideology$FIPS <- case_when(
  nchar(TW_ideology$FIPS) == 4 ~ sub("^", "0", TW_ideology$FIPS),
  TRUE ~ as.character(TW_ideology$FIPS)
)


# ------ merge -----------------------

library(plyr)

#merge county-level data
merge <- join_all(list(sanctuary_raw, 
              rural_atlas_people,
              rural_atlas_veterans,
              rural_atlas_countyclass,
              rural_atlas_income,
              rural_atlas_jobs,
              TW_ideology),
         by = "FIPS",
         type = "left") %>% 
  print()

#merge state-level data
merge2 <- left_join(merge, state_leg, by = "State")

# ----- filter rows ------------------

# remove states and Washington D.C.
counties <- merge2 %>% 
  mutate(
    FIPS_end = str_sub(FIPS, 3L, 5L)
  ) %>% 
  filter(!
    FIPS_end == "000"
  ) %>% 
  filter(!
    State == "DC"
  )
  

# -------- Map -----------
library(choroplethr)
library(choroplethrMaps)

counties_map <- merge2 %>% 
  mutate(
    FIPS_end = str_sub(FIPS, 3L, 5L)
  ) %>% 
  filter(!
           FIPS_end == "000" #remove states
  ) %>% 
  mutate(
    FIPS = gsub("^0", "", FIPS) #remove first 0 to match with county_cloropleth()
  ) %>% 
  mutate(
    FIPS = as.numeric(FIPS)
  )

#make new table to conform with county_cloropleth()
counties_map <- tibble(region = counties_map$FIPS, value = counties_map$ResolutionPassed)

counties_map <- counties_map %>% 
  mutate(
    region = case_when(
      region == 2158 ~ 2270, #replace Alaska's Wade Hampton Census Area for Kusilvak County
      region == 46102 ~ 46113, #replace South Dakota's Shannon County for Oglala Lakota County
      TRUE ~ region
    )
  ) %>% 
  mutate(
    value = case_when(
      is.na(value) == TRUE ~ 0, #make DC a non-sanctuary
      TRUE ~ value
    )
  )

#create the map
map <- county_choropleth(counties_map, num_colors = 1)

#save the map
png(filename = here::here("Workspace", "map.png"))
plot(map)
dev.off()

library(magick)
#import the map as an image
county_map <- image_read(here::here("Workspace", "map.png"))

#crop the map to get rid of the legend
map <- image_crop(county_map, "415x250+0+120")

#save the map again
png(filename = here::here("Workspace", "map.png"))
plot(map)
dev.off()


# ---- Summary Statistics ------------

library("dplyr")

#Summary of Ideology
summary_counties <- counties %>% 
  mutate(ResolutionPassed = 
           case_when(
             ResolutionPassed == 1 ~ "Sanctuary",
             ResolutionPassed == 0 ~ "Non-Sanctuary"
           ))

summary <- summary_counties %>% group_by(ResolutionPassed) %>% 
  dplyr::summarise(
    count = length(ResolutionPassed),
    minimum = min(mrp_ideology_mean, na.rm = TRUE),
    maximum = max(mrp_ideology_mean, na.rm = TRUE),
    mean = mean(mrp_ideology_mean, na.rm = TRUE),
    sd = sd(mrp_ideology_mean, na.rm = TRUE)
  )

summary

#Plot of Ideology
ideology_plot <- ggplot(summary_counties, 
                        aes(x=mrp_ideology_mean, 
                            fill=ResolutionPassed)) + 
  geom_histogram(alpha=0.5, 
                 position="identity") +
  scale_x_continuous(name = "County Ideology Estimate") +
  scale_y_continuous(name = "Count") +
  ggtitle("Ideology Distribution among Counties") +
  theme(plot.title = element_text(hjust = 0.5))

#Table of Ideology and State Government
government_table <- table(counties$ResolutionPassed, counties$StateControl2020) #Resolution by partisan control of government
rownames(government_table) <- c("Non-Sanctuary", "Sanctuary")
colnames(government_table) <- c("Nonpartisan", "Democrat", "Divided", "Republican")

# ----- T-tests ----------------------
#run ttests
tests <- list()
tests[[1]] <- t.test(
  counties$Under18Pct2010 ~ counties$ResolutionPassed) #Percentage Under Age 18
tests[[2]] <- t.test(
  counties$Age65AndOlderPct2010 ~ counties$ResolutionPassed) #Percentage Age 65 and Older
tests[[3]] <- t.test(
  counties$WhiteNonHispanicPct2010 ~ counties$ResolutionPassed) #Percentage White
tests[[4]] <- t.test(
  counties$HispanicPct2010 ~ counties$ResolutionPassed) #Percentage Hispanic
tests[[5]] <- t.test(
  counties$BlackNonHispanicPct2010 ~ counties$ResolutionPassed) #Percentage Black
tests[[6]] <- t.test(
  counties$Ed1LessThanHSPct ~ counties$ResolutionPassed) #Percentage Less than High School
tests[[7]] <- t.test(
  counties$Ed2HSDiplomaOnlyPct ~ counties$ResolutionPassed) #Percentage High School Only
tests[[8]] <- t.test(
  counties$Ed4AssocDegreePct ~ counties$ResolutionPassed) #Percentage Associate Degree
tests[[9]] <- t.test(
  counties$Ed5CollegePlusPct ~ counties$ResolutionPassed) #Percentage with College Degree
tests[[10]] <- t.test(
  counties$ForeignBornPct ~ counties$ResolutionPassed) #Percentage Foreign Born
tests[[11]] <- t.test(
  counties$PopDensity2010 ~ counties$ResolutionPassed) #Population Density
tests[[12]] <- t.test(
  counties$TotalPopEstBase2010 ~ counties$ResolutionPassed) #Total Population
tests[[13]] <- t.test(
  counties$Vets18OPct ~ counties$ResolutionPassed) #Percentage Veteran
tests[[14]] <- t.test(
  counties$PovertyAllAgesPct ~ counties$ResolutionPassed) #Percentage in Poverty
tests[[15]] <- t.test(
  counties$MedHHInc ~ counties$ResolutionPassed) #Median Household Income
tests[[16]] <- t.test(
  counties$PerCapitaInc ~ counties$ResolutionPassed) #Per Capita Income
tests[[17]] <- t.test(
  counties$UnempRate2018 ~ counties$ResolutionPassed) #2018 Unemployment Rate
tests[[18]] <- t.test(
  counties$PctEmpServices ~ counties$ResolutionPassed) #Percentage Employed in Services
tests[[19]] <- t.test(
  counties$PctEmpManufacturing ~ counties$ResolutionPassed) #Percentage Employed in Manufacturing
tests[[20]] <- t.test(
  counties$PctEmpAgriculture ~ counties$ResolutionPassed) #Percentage Employed in Agriculture
tests[[21]] <- t.test(
  counties$PctEmpConstruction ~ counties$ResolutionPassed) #Percentage Employed in Construction
tests[[22]] <- t.test(
  counties$PctEmpMining ~ counties$ResolutionPassed) #Percentage Employed in Mining
tests[[23]] <- t.test(
  counties$mrp_ideology_mean ~ counties$ResolutionPassed) #Estimated Mean Ideology

#extract values using `sapply`
ttest_table <- sapply(tests, function(x) {
  c(x$estimate[1] %>% round(digits=2),
    x$estimate[2] %>% round(digits=2),
    ci.lower = x$conf.int[1] %>% round(digits=2),
    ci.upper = x$conf.int[2] %>% round(digits=2),
    p.value = x$p.value %>% round(digits=3))
}) %>% 
  t() %>% 
  print()

#rename rows and columns
rownames(ttest_table) <- c("Under18Pct", 
                           "Age65AndOlderPct",
                           "WhiteNonHispanicPct",
                           "HispanicPct",
                           "BlackNonHispanicPct",
                           "LessThanHSPct",
                           "HSDiplomaOnlyPct",
                           "AssocDegreePct",
                           "CollegePlusPct",
                           "ForeignBornPct",
                           "PopDensity",
                           "TotalPopEstimate",
                           "Vets18Pct",
                           "PovertyAllAgesPct",
                           "MedHHInc",
                           "PerCapitaInc",
                           "UnempRate2018",
                           "PctEmpServices",
                           "PctEmpManufacturing",
                           "PctEmpAgriculture",
                           "PctEmpConstruction",
                           "PctEmpMining",
                           "TWIdeologyEst")

colnames(ttest_table) <- c("Mean of \n Non-Sanctuaries",
                           "Mean of \n Sanctuaries",
                           "Lower CI",
                           "Upper CI",
                           "P-Value")

#present results in a table
ttest_table %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c(
    "striped", "hover"),
    full_width = FALSE)

# ------ Logit Model -----------------
#remove counties in state with nonpartisan state legislature (Nebraska)
counties_new <- counties %>% 
  filter(StateControl2020 != ".")

#extract year resolution was passed
counties_new <- counties_new %>% 
  mutate(
    resolution_year = format(as.Date(PassageDate, format = "%d/%m/%Y"),"%Y")
  )

#assign StateControl variable by the year the resolution was passed or the most recent year
counties_new <- counties_new %>% 
  mutate(StateControl = case_when(
    resolution_year == 2020 ~ StateControl2020,
    resolution_year == 2019 ~ StateControl2019,
    resolution_year == 2018 ~ StateControl2018,
    resolution_year == 2017 ~ StateControl2017,
    resolution_year == 2016 ~ StateControl2016,
    resolution_year == 2015 ~ StateControl2015,
    resolution_year == 2014 ~ StateControl2014,
    resolution_year == 2013 ~ StateControl2013,
    TRUE ~ StateControl2020)
  )

#model
model <- glm(ResolutionPassed ~ StateControl + 
               mrp_ideology_mean + 
               Under18Pct2010 +
               Age65AndOlderPct2010 +
               WhiteNonHispanicPct2010 + 
               HispanicPct2010 +
               BlackNonHispanicPct2010 +
               Ed1LessThanHSPct +
               Ed2HSDiplomaOnlyPct + 
               Ed4AssocDegreePct +
               Ed5CollegePlusPct + 
               ForeignBornPct + 
               PopDensity2010 + 
               TotalPopEstBase2010 +
               Vets18OPct +
               PovertyAllAgesPct + 
               MedHHInc +
               PerCapitaInc + 
               UnempRate2018 +
               PctEmpServices + 
               PctEmpManufacturing + 
               PctEmpAgriculture + 
               PctEmpConstruction + 
               PctEmpMining,
             data = counties_new, 
             family = "binomial"(link = "logit"))

#model summary
summary(model)

prediction_data <- tibble(mrp_ideology_mean = rep(seq(-1.5, 1.5, by = .1), 3)) %>%
  crossing(StateControl = c("Democrat", "Divided", "Republican"),
           ResolutionPassed = c(0, 1),
           Under18Pct2010 = mean(counties_new$Under18Pct2010), 
           Age65AndOlderPct2010 = mean(counties_new$Age65AndOlderPct2010),
           WhiteNonHispanicPct2010 = mean(counties_new$WhiteNonHispanicPct2010),
           HispanicPct2010 = mean(counties_new$HispanicPct2010),
           BlackNonHispanicPct2010 = mean(counties_new$BlackNonHispanicPct2010),
           Ed1LessThanHSPct = mean(counties_new$Ed1LessThanHSPct),
           Ed2HSDiplomaOnlyPct = mean(counties$Ed2HSDiplomaOnlyPct),
           Ed4AssocDegreePct = mean(counties$Ed4AssocDegreePct),
           Ed5CollegePlusPct = mean(counties_new$Ed5CollegePlusPct),
           ForeignBornPct = mean(counties_new$ForeignBornPct),
           PopDensity2010 = mean(counties_new$PopDensity2010),
           TotalPopEstBase2010 = mean(counties_new$TotalPopEstBase2010),
           Vets18OPct = mean(counties_new$Vets18OPct),
           PovertyAllAgesPct = mean(counties_new$PovertyAllAgesPct, na.rm = TRUE),
           MedHHInc = mean(counties_new$MedHHInc, na.rm = TRUE),
           PerCapitaInc = mean(counties_new$PerCapitaInc),
           UnempRate2018 = mean(counties_new$UnempRate2018, na.rm = TRUE),
           PctEmpServices = mean(counties_new$PctEmpServices),
           PctEmpManufacturing = mean(counties_new$PctEmpManufacturing),
           PctEmpAgriculture = mean(counties_new$PctEmpAgriculture),
           PctEmpConstruction = mean(counties_new$PctEmpConstruction),
           PctEmpMining = mean(counties_new$PctEmpMining)
           )

#store the predicted probabilities for each value of state control and ideology
predictions <- augment(model, newdata = prediction_data) %>%
  mutate(
    .prob = plogis(.fitted),
    conf.low = plogis(.fitted - (qnorm(.975) * .se.fit)),
    conf.high = plogis(.fitted + (qnorm(.975) * .se.fit)),
  )


#calculate the mean probabilities of passage for each level of StateControl
predictions %>% 
  group_by(StateControl) %>% 
  dplyr::summarize(mean(.prob))

#plot predicted probabilities across ideology values for each level of StateControl
plot1 <- ggplot(predictions, aes(x = mrp_ideology_mean, 
                        y = .prob)) + 
  geom_smooth(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = StateControl,
                  colour = StateControl),
              alpha = 0.3,
              stat = "identity") +
  scale_color_manual(values = c("dodgerblue", "limegreen", "tomato")) +
  scale_fill_manual(values = c("dodgerblue", "limegreen", "tomato")) +
  ggtitle("Probability of Passing Sanctuary Resolution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Ideology") +
  ylab("Probability") +
  geom_vline(xintercept = min(counties_new$mrp_ideology_mean, na.rm = TRUE), linetype = "dashed") +
  geom_vline(xintercept = max(counties_new$mrp_ideology_mean, na.rm = TRUE), linetype = "dashed")

#Virginia Analysis
virginia <- counties %>%  group_by(State) %>% 
  filter(State == "VA")

virginia_passed <- virginia %>% filter(ResolutionPassed == 1) %>% 
  mutate(after_election = 
           case_when(
             PassageDate > "2019-11-05" & PassageDate <= "2019-12-31" ~ 1,
             TRUE ~ 0
           ))

proportion_after_election <- sum(virginia_passed$after_election)/nrow(virginia_passed)

# ------ Policy Diffusion ------------

#for each state, find the date that the first county passed its resolution
counties_new <- counties_new %>% dplyr::group_by(State) %>% 
  dplyr::mutate(state_first_passed = min(PassageDate, na.rm = TRUE))

#new variable = 1 if a county in your state has passed a sanctuary resolution
counties_new <- counties_new %>% 
  dplyr::mutate(other_passed = case_when(
    PassageDate == state_first_passed ~ 0, #if your county was the first in your state, you get a 0
    is.na(as.character(state_first_passed)) ~ 0, #if no county in your state passed a resolution, you get a 0
    TRUE ~ 1 # if a county in your state passed a resolution, you get a 1
  ))
                  
#add to new logistic regression model
model2 <- glm(ResolutionPassed ~ StateControl + 
               mrp_ideology_mean + 
               Under18Pct2010 +
               Age65AndOlderPct2010 +
               WhiteNonHispanicPct2010 + 
               HispanicPct2010 +
               BlackNonHispanicPct2010 +
               Ed1LessThanHSPct +
               Ed2HSDiplomaOnlyPct + 
               Ed4AssocDegreePct +
               Ed5CollegePlusPct + 
               ForeignBornPct + 
               PopDensity2010 + 
               TotalPopEstBase2010 +
               Vets18OPct +
               PovertyAllAgesPct + 
               MedHHInc +
               PerCapitaInc + 
               UnempRate2018 +
               PctEmpServices + 
               PctEmpManufacturing + 
               PctEmpAgriculture + 
               PctEmpConstruction + 
               PctEmpMining +
               other_passed,
             data = counties_new, 
             family = "binomial"(link = "logit"))


#model summary
summary(model2)

#create new dataframe of different values of state control and ideology
prediction_data2 <- tibble(mrp_ideology_mean = rep(seq(-1.5, 1.5, by = .1), 3)) %>%
  crossing(StateControl = c("Democrat", "Divided", "Republican"),
           ResolutionPassed = c(0, 1),
           other_passed = c(0, 1),
           Under18Pct2010 = mean(counties_new$Under18Pct2010), 
           Age65AndOlderPct2010 = mean(counties_new$Age65AndOlderPct2010),
           WhiteNonHispanicPct2010 = mean(counties_new$WhiteNonHispanicPct2010),
           HispanicPct2010 = mean(counties_new$HispanicPct2010),
           BlackNonHispanicPct2010 = mean(counties_new$BlackNonHispanicPct2010),
           Ed1LessThanHSPct = mean(counties_new$Ed1LessThanHSPct),
           Ed2HSDiplomaOnlyPct = mean(counties$Ed2HSDiplomaOnlyPct),
           Ed4AssocDegreePct = mean(counties$Ed4AssocDegreePct),
           Ed5CollegePlusPct = mean(counties_new$Ed5CollegePlusPct),
           ForeignBornPct = mean(counties_new$ForeignBornPct),
           PopDensity2010 = mean(counties_new$PopDensity2010),
           TotalPopEstBase2010 = mean(counties_new$TotalPopEstBase2010),
           Vets18OPct = mean(counties_new$Vets18OPct),
           PovertyAllAgesPct = mean(counties_new$PovertyAllAgesPct, na.rm = TRUE),
           MedHHInc = mean(counties_new$MedHHInc, na.rm = TRUE),
           PerCapitaInc = mean(counties_new$PerCapitaInc),
           UnempRate2018 = mean(counties_new$UnempRate2018, na.rm = TRUE),
           PctEmpServices = mean(counties_new$PctEmpServices),
           PctEmpManufacturing = mean(counties_new$PctEmpManufacturing),
           PctEmpAgriculture = mean(counties_new$PctEmpAgriculture),
           PctEmpConstruction = mean(counties_new$PctEmpConstruction),
           PctEmpMining = mean(counties_new$PctEmpMining)
  )


#store the predicted probabilities for each value of state control and ideology
predictions2 <- augment(model2, newdata = prediction_data2) %>%
  mutate(
    .prob = plogis(.fitted),
    conf.low = plogis(.fitted - (qnorm(.975) * .se.fit)),
    conf.high = plogis(.fitted + (qnorm(.975) * .se.fit)),
  )

#calculate the mean probabilities of passage for each level of StateControl
predictions2 %>% 
  group_by(StateControl) %>% 
  dplyr::summarize(mean(.prob))


#plot predicted probabilities across ideology values for each level of StateControl
#first create names for facets
facet_names <- c(
  '0' = "No Diffusion",
  '1' = "Diffusion"
)

plot2 <- ggplot(predictions2, aes(x = mrp_ideology_mean, 
                        y = .prob)) + 
  geom_smooth(aes(ymin = conf.low,
                  ymax = conf.high,
                  fill = StateControl,
                  colour = StateControl),
              alpha = 0.3,
              stat = "identity") +
  facet_grid(other_passed ~ ., labeller = as_labeller(facet_names)) +
  scale_color_manual(values = c("dodgerblue", "limegreen", "tomato")) +
  scale_fill_manual(values = c("dodgerblue", "limegreen", "tomato")) +
  ggtitle("Probability of Passing Sanctuary Resolution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Ideology") +
  ylab("Probability") +
  geom_vline(xintercept = min(counties_new$mrp_ideology_mean, na.rm = TRUE), linetype = "dashed") +
  geom_vline(xintercept = max(counties_new$mrp_ideology_mean, na.rm = TRUE), linetype = "dashed")

# ---------- save workspace -------------
save.image(file = here::here("Workspace", "paperdata.Rdata"))

