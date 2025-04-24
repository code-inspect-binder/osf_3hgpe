#### PREPARATIONS ###############################################################################################

#### Load libraries --------------------------------------------------------------------------------------------- 
library(tidyverse)
library(magrittr)
library(sjlabelled)
library(readxl)
library(reshape2)



#### Prepare data sets ------------------------------------------------------------------------------------------

# Data of the first wave of the panel survey, conducted in June/July 2019
ds19panel <- readRDS(file = "01 Data/ds19panel.rds")

# Data of the second wave of the panel survey, conducted in November 2020
ds20panel <- readRDS(file = "01 Data/ds20panel.rds")

# Data of the cross-sectional survey conducted in June/July 2019, panelists excluded
ds19cross_nopanel <- readRDS(file = "01 Data/ds19cross_nopanel.rds")

# Data of the cross-sectional survey conducted in June/July 2019, panelists included
ds19cross_complete <- readRDS(file = "01 Data/ds19cross_complete.rds")

# Data of the cross-sectional survey conducted in November 2020
ds20cross <- readRDS(file = "01 Data/ds20cross.rds")


# Merge all data and (long format)
dslong <- bind_rows(ds20panel, ds19panel, ds20cross, ds19cross_nopanel, ds19cross_complete) %>% arrange(t, id)


# id and t as factors
dslong %<>% transform(id = as.factor(id), t = as.factor(t))




#### Prepare variables ------------------------------------------------------------------------------------------

# -- 01) Goertzian SciPop Scores (use minimum subscale mean) ----------------------------------------------------
dslong %<>% mutate(scipopppl = rowMeans(select(., scipop1_ppl, scipop2_ppl), na.rm = T),
                   scipopeli = rowMeans(select(., scipop3_eli, scipop4_eli), na.rm = T),
                   scipopdec = rowMeans(select(., scipop5_dec, scipop6_dec), na.rm = T),
                   scipoptru = rowMeans(select(., scipop7_tru, scipop8_tru), na.rm = T)) %>%
  mutate(scipopgoertz = pmin(.$scipopppl, .$scipopeli, .$scipopdec, .$scipoptru))

dslong$scipopppl[is.nan(dslong$scipopppl)] <- NA
dslong$scipopeli[is.nan(dslong$scipopeli)] <- NA
dslong$scipopdec[is.nan(dslong$scipopdec)] <- NA
dslong$scipoptru[is.nan(dslong$scipoptru)] <- NA
dslong$scipopgoertz[is.nan(dslong$scipopgoertz)] <- NA



# -- 02) Gender -------------------------------------------------------------------------------------------------
dslong$gender %<>% recode(`1` = 0, `2` = 1) %>% set_labels(labels = c("male" = 0, "female" = 1))



# -- 03) Education ----------------------------------------------------------------------------------------------
dslong %<>% mutate(edu_uni = case_when(edu %in% 4:5~1, edu %in% 1:3 ~ 0, edu == 6~NA_real_))
dslong %<>% mutate(edu_sec = case_when(edu %in% 2:3~1, edu == 1 ~ 0, edu %in% 4:5~0, edu == 6~NA_real_))
dslong %<>% mutate(edu_com = case_when(edu %in% 1~1, edu %in% 2:5 ~ 0, edu == 6~NA_real_))



# -- 04) Language region ----------------------------------------------------------------------------------------
dslong %<>% mutate(lingreg_D = case_when(lingreg == 1~1, lingreg %in% 2:3 ~ 0, lingreg == 4~NA_real_))
dslong %<>% mutate(lingreg_F = case_when(lingreg == 2~1, lingreg == 1~0, lingreg == 3~0, lingreg == 4~NA_real_))
dslong %<>% mutate(lingreg_I = case_when(lingreg == 3~1, lingreg %in% 1:2 ~ 0, lingreg == 4~NA_real_))



# -- 05) Urbanity (i.e. inhabitant count of residence municipality) ---------------------------------------------

# Read census data
bfscensus <- read_xlsx("01 Data/bfs-censusdata.xlsx", sheet = 1, range = "A6:AK2221", col_names = T, 
                       na = c("X", "*"), trim_ws = T, .name_repair = "unique") %>% 
  slice(4:n()) %>%
  select("Gemeindecode", "Einwohner") %>%
  rename(bfsno = Gemeindecode, urbanity = Einwohner)


# Read correspondence table
bfscorrespontbl <- read_xlsx("01 Data/bfs-correspondencetable.xlsx", sheet = 2, col_names = T, trim_ws = T,
                             .name_repair = "unique") %>%
  select("PLZ4", "%_IN_GDE", "GDENR", "GDENAMK", "KTKZ") %>%
  rename(plz = PLZ4, bfsplzperc = `%_IN_GDE`, bfsno = GDENR, bfsname = GDENAMK, canton = KTKZ)


# Merge census data with correspondence table
bfscensus <- left_join(bfscensus, bfscorrespontbl, by = c("bfsno" = "bfsno"))


# See if we could match all postal codes in survey data with postal codes from correspondence table
anti_join(dslong, bfscensus, by = c("plz" = "plz")) %>% select(plz) %>% drop_na()

# --> 2 cases (with 6 different postal codes) can not be matched
#     Google search shows that:
#     4000 = Basel (whose municipality number is 2701)
#     8000 = Zurich (whose municipality number is 261)
#     So we add the unmatched cases manually:
bfsmissing <- rbind(cbind(bfsno = 2701,
                          urbanity = bfscensus[as.vector(bfscensus[1] == 2701), "urbanity"],
                          plz = 4000,
                          bfsplzperc = bfscensus[as.vector(bfscensus[1] == 2701), "bfsplzperc"],
                          bfsname = bfscensus[as.vector(bfscensus[1] == 2701), "bfsname"],
                          canton = bfscensus[as.vector(bfscensus[1] == 2701), "canton"]),
                    cbind(bfsno = 261,
                          urbanity = bfscensus[as.vector(bfscensus[1] == 261), "urbanity"],
                          plz = 8000,
                          bfsplzperc = bfscensus[as.vector(bfscensus[1] == 261), "bfsplzperc"],
                          bfsname = bfscensus[as.vector(bfscensus[1] == 261), "bfsname"],
                          canton = bfscensus[as.vector(bfscensus[1] == 261), "canton"]))

bfscensus <- bfsmissing %>% rbind(bfscensus) %>% arrange(bfsno)



# Create list of all 3186 postal codes
plzlist <- bfscensus %>% group_by(plz) %>% summarise(bfsno = first(bfsno)) %>% select(plz)


# Create an empty vector for the plzs and the inhabitant counts we want to preserve
bfscensus_preserve <- c()


# Now apply the above-mentioned procedure using a for loop
for (i in plzlist$plz){
  j <- subset(bfscensus, plz == i) %>% filter(bfsplzperc == max(bfsplzperc))
  if (nrow(j) > 1){
    j$bfsno <- NA_integer_
    j$urbanity <- NA_integer_
    j$bfsname <- NA_integer_
    j %<>% select(plz, bfsno, urbanity, bfsname, canton) %>%
      group_by(plz) %>%
      summarise(bfsno = first(bfsno), 
                urbanity = first(urbanity), 
                bfsname = first(bfsname), 
                canton = first(canton))
    bfscensus_preserve <- rbind(bfscensus_preserve, j)
  } else {
    j %<>% select(plz, bfsno, urbanity, bfsname, canton)
    bfscensus_preserve <- rbind(bfscensus_preserve, j)
  }
}


# Check how many postal codes are not clearly matching one municipality number (should be 80)
sum(is.na(bfscensus_preserve$bfsno))


# Merge the census data with the survey data
dslong %<>% left_join(bfscensus_preserve, by = c("plz" = "plz"))


# Inhabitant counts follow a lognormal distribution (see of Decker et al., 2007, p. 4, Figure 4):
hist(dslong$urbanity, main = "Histogram of inhabitant counts of residence municipality", 
     breaks = 200, xlim = c(0, 50000), ylim = c(0, 70))


# As this may be problematic for later analyses, we transform urbanity into its log:
dslong %<>% mutate(urbanity_log = log(urbanity))


# Now the distribution comes closer to a normal distribution:
hist(dslong$urbanity_log, main = "Histogram of inhabitant counts of residence municipality",
     breaks = 20, xlim = c(5, 15), ylim = c(0, 70))



# -- 06) Proximity to science -----------------------------------------------------------------------------------
dslong$sciprox1 %<>% recode(`1`= 1, `2`= 0, .default = NA_real_) %>% set_labels(labels = c("yes" = 1, "no" = 0))
dslong$sciprox2 %<>% recode(`1`= 1, `2`= 0, .default = NA_real_) %>% set_labels(labels = c("yes" = 1, "no" = 0))
dslong$sciprox3 %<>% recode(`1`= 1, `2`= 0, .default = NA_real_) %>% set_labels(labels = c("yes" = 1, "no" = 0))
dslong$sciprox4 %<>% recode(`1`= 1, `2`= 0, .default = NA_real_) %>% set_labels(labels = c("yes" = 1, "no" = 0))

dslong %<>% mutate(sciprox_score = case_when(sciprox1 == 1 ~ 4,
                                             sciprox2 + sciprox3 + sciprox4 == 3 ~ 3,
                                             sciprox2 + sciprox3 + sciprox4 == 2 ~ 2,
                                             sciprox2 + sciprox3 + sciprox4 == 1 ~ 1,
                                             sciprox1 == 0 | sciprox2 + sciprox3 + sciprox4 == 0 ~ 0))



# -- 07) Affected by COVID (at risk because of condition, tested for SARS-CoV-2) --------------------------------
dslong$covidrisk %<>% recode(`1`= 1, `2`= 0, .default = NA_real_) %>% set_labels(labels = c("yes" = 1, "no" = 0))
dslong$covidtest %<>% recode(`1`= 1, `2`= 0, .default = NA_real_) %>% set_labels(labels = c("yes" = 1, "no" = 0))

dslong %<>% mutate(covid_score = covidrisk + covidtest)



# Tidy up
rm(bfscensus, bfscensus_preserve, bfsmissing, bfscorrespontbl, i, j, plzlist)




#### Check data quality -----------------------------------------------------------------------------------------

# Inspect if people have "switched gender" have aged more than 2 years and exclude them (13 did)
faultycases <- dslong %>% group_by(id) %>%
  summarise(genderdiff = max(gender) - min(gender),
            agediff = max(age) - min(age)) %>%
  filter(genderdiff > 0 | agediff > 2)

faultycases

dslong <- subset(dslong, !(id %in% faultycases$id))



