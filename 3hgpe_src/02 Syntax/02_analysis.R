#### PREPARATIONS ###############################################################################################

#### Load libraries --------------------------------------------------------------------------------------------- 
library(rlang)
library(sjPlot)
library(forcats)
library(survey)
library(jtools)
library(broom)
library(moments)
library(lme4)
library(insight)
library(ez)
library(lavaan)
library(knitr)
library(kableExtra)
library(ggpubr)
library(ggfortify)
library(ggalluvial)



#### Define functions -------------------------------------------------------------------------------------------

# Means, SDs, and two sample t-tests comparing Swiss census data and the panel sample 
census.msd <- function(v) {
  v <- reformulate(v)
  cbind(
    as.data.frame(svyby(v, ~t, dslong_dsgn, svymean, na.rm = T))[-c(1,3)], # M (exclude SE)
    sqrt(as.data.frame(svyby(v, ~t, dslong_dsgn, svyvar, na.rm = T)[[2]]))) %>% # SD (with a workaround)
    set_colnames(c("M", "SD")) %>% 
    set_rownames(c("2019 (census)", "2019 (panel)"))
}



# Bar charts of valid response percentages
validfreqplot <- function(var, digits) {
  enquovar <- enquo(var)
  
  dslongpnl %>% 
    group_by(t) %>% count(!!enquovar) %>% drop_na() %>% 
    mutate(Percent = round(n / sum(n)*100, digits = digits)) %>%
    ggplot(aes(x = t, y = Percent, fill = fct_rev(factor(!!enquovar)))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste(Percent, "%"), y = Percent), 
              color = rep(c("black", "black", "black", "white", "white"), 2),
              position = position_stack(vjust = 0.5)) +
    coord_flip() + 
    labs(x = "t", y = "Percentage", fill = substitute(var)) +
    scale_x_discrete(limits = rev) +
    scale_fill_grey(limits = rev, start = 0.8, end = 0.05) +
    theme_minimal() +
    theme(axis.title.y = element_text(angle = 360, hjust = 0, vjust = 0.5),
          legend.position = "none",
          axis.text.x = element_blank(),
          axis.title.x = element_blank())
}


# Extract variances from multilevel models
# # random intercept variance, indicates between-subject variance
# nullmodelvars$var.random
# # residual variance, indicates the within-subject variance
# nullmodelvars$var.residual
# # proportion of SciPop Score variance between respondents in overall variance (ICC)
# nullmodelvars$var.random / (nullmodelvars$var.random + nullmodelvars$var.residual) 
# # proportion of SciPop Score variance within respondents in overall variance
# nullmodelvars$var.residual / (nullmodelvars$var.random + nullmodelvars$var.residual)
lmervars <- function(nullmodelvars) {
  rbind(`random intercept variance (= between-subject variance)` = 
          nullmodelvars$var.random,
        `residual variance (= within-subject variance)` = 
          nullmodelvars$var.residual,
        `proportion of score variance between respondents in overall variance (ICC)` = 
          nullmodelvars$var.random/(nullmodelvars$var.random + nullmodelvars$var.residual),
        `proportion of score variance within respondents in overall variance` = 
          nullmodelvars$var.residual/(nullmodelvars$var.random + nullmodelvars$var.residual))
}


# Helper functions for handling missing values in the ANOVAs of robustness test
# taken from https://stackoverflow.com/questions/13987615/ezanova-1-how-to-omit-na-data-2-still-getting-error-message-after-omitting-n
matchAll <- function(x, table, nomatch = NA_integer_, incomparables = NULL) {
  which(!is.na(match(table, x, nomatch, incomparables)))
}

complete.cases.within <- function(data, dv, wid) {
  incomplete <- data %>% select(dv) %>% complete.cases() %>% !.
  toRemove <- data %>% select(wid) %>% filter(incomplete) %>% .[, 1] %>% unique()
  positions <- matchAll(toRemove, data[,wid])
  return(if (length(positions) == 0) data else data[-positions, ])
}




#### PRELIMINARY ANALYSES #######################################################################################

#### Characteristics of panel sample and reference samples ------------------------------------------------------

# Characteristics of panel sample, remainder of 2019 cross-sectional sample, and 2020 cross-sectional sample
dslong %>% group_by(t) %>%
  summarise(`N` = n(),
            `Age (M)` = mean(age),
            `Age (SD)` = sd(age),
            `Female (%)` = 100*mean(gender),
            `Compulsory school (%)` = 100*mean(edu_com, na.rm = T),
            `Secondary education (%)` = 100*mean(edu_sec, na.rm = T),
            `University degree (%)` = 100*mean(edu_uni, na.rm = T),
            `German-speaking part (%)` = 100*mean(lingreg_D),
            `French-speaking part (%)` = 100*mean(lingreg_F),
            `Italian-speaking part (%)` = 100*mean(lingreg_I),
            `Scientists (%)` = 100 * mean(sciprox1, na.rm = T)) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  slice(2:4) %>%
  arrange(N) %>%
  kable(format = "html") %>% kable_styling(bootstrap_options = "hover") 



# Two sample t-tests comparing remainder of 2019 cross-sectional and the panel sample
t.test(dslong$age[dslong$t == "2019 (panel)"], 
       dslong$age[dslong$t == "2019 (cross-sectional, w/o panelists)"], 
       paired = F, "two.sided")
t.test(dslong$gender[dslong$t == "2019 (panel)"], 
       dslong$gender[dslong$t == "2019 (cross-sectional, w/o panelists)"], 
       paired = F, "two.sided")
t.test(dslong$edu_uni[dslong$t == "2019 (panel)"], 
       dslong$edu_uni[dslong$t == "2019 (cross-sectional, w/o panelists)"], 
       paired = F, "two.sided")
t.test(dslong$lingreg_D[dslong$t == "2019 (panel)"], 
       dslong$lingreg_D[dslong$t == "2019 (cross-sectional, w/o panelists)"], 
       paired = F, "two.sided")
t.test(dslong$sciprox1[dslong$t == "2019 (panel)"], 
       dslong$sciprox1[dslong$t == "2019 (cross-sectional, w/o panelists)"], 
       paired = F, "two.sided")


# Two sample t-tests comparing 2020 cross-sectional and the panel sample
t.test(dslong$age[dslong$t == "2019 (panel)"], 
       dslong$age[dslong$t == "2020 (cross-sectional)"], 
       paired = F, "two.sided")
t.test(dslong$gender[dslong$t == "2019 (panel)"], 
       dslong$gender[dslong$t == "2020 (cross-sectional)"], 
       paired = F, "two.sided")
t.test(dslong$edu_uni[dslong$t == "2019 (panel)"], 
       dslong$edu_uni[dslong$t == "2020 (cross-sectional)"], 
       paired = F, "two.sided")
t.test(dslong$lingreg_D[dslong$t == "2019 (panel)"], 
       dslong$lingreg_D[dslong$t == "2020 (cross-sectional)"], 
       paired = F, "two.sided")
t.test(dslong$sciprox1[dslong$t == "2019 (panel)"], 
       dslong$sciprox1[dslong$t == "2020 (cross-sectional)"], 
       paired = F, "two.sided")


# Means, SDs, and two sample t-tests comparing Swiss census data and the panel sample 
# (employing the survey weights computed by the polling company as they have been raked using Swiss census data)
dslong$weight[dslong$t != "2019 (cross-sectional, complete)"] <- 1
dslong_dsgn <- svydesign(id = ~0, data = dslong, weights = ~weight) %>% 
  subset(t %in% c("2019 (panel)", "2019 (cross-sectional, complete)")) # create survey design

census.msd("age")
census.msd("gender")
census.msd("edu_com")
census.msd("edu_sec")
census.msd("edu_uni")
census.msd("lingreg_D")
census.msd("lingreg_F")
census.msd("lingreg_I")

svyttest(age ~ t, dslong_dsgn)
svyttest(gender ~ t, dslong_dsgn)
svyttest(edu_uni ~ t, dslong_dsgn)
svyttest(lingreg_D ~ t, dslong_dsgn)




#### Inspect scale performance ----------------------------------------------------------------------------------

# Formulate measurement model with the following specifications:
#  (1) Specify 4 latent factors and 8 indicators, with indicators loading only on the factor which represents 
#      the conceptual dimension they are supposed to indicate (see Mede, Schafer, & Fuchslin, 2020).
#  (2) Model identification and scale setting relies on the LSC method (Little, Slegers, & Card, 2006), which
#      suggests constraining the average unstandardized loading of the indicators of each latent factor to
#      equal 1.0 and the corresponding indicator intercepts to sum to zero.
#  (3) Use MLM estimator (robust standard errors, Satorra-Bentler scaled test statistic)
cfascale <- '# Specify the four latent variables, manually free first loading, and
             # constrain the set of loadings for each latent factor to average 1.0
 
               ppl =~ NA*scipop1_ppl + lambda1*scipop1_ppl + lambda2*scipop2_ppl
               eli =~ NA*scipop3_eli + lambda3*scipop3_eli + lambda4*scipop4_eli
               dec =~ NA*scipop5_dec + lambda5*scipop5_dec + lambda6*scipop6_dec
               tru =~ NA*scipop7_tru + lambda7*scipop7_tru + lambda8*scipop8_tru
             
               lambda1 + lambda2 == 2
               lambda3 + lambda4 == 2
               lambda5 + lambda6 == 2 
               lambda7 + lambda8 == 2
 
             # Constrain the set of indicator intercepts to sum to zero for each latent factor
               scipop1_ppl ~ int1*1
               scipop2_ppl ~ int2*1
               scipop3_eli ~ int3*1
               scipop4_eli ~ int4*1
               scipop5_dec ~ int5*1
               scipop6_dec ~ int6*1
               scipop7_tru ~ int7*1
               scipop8_tru ~ int8*1
               
               int1 + int2 == 0
               int3 + int4 == 0 
               int5 + int6 == 0 
               int7 + int8 == 0
               
             # Free latent factor means
               ppl ~ NA*1
               eli ~ NA*1
               dec ~ NA*1
               tru ~ NA*1'


# Single-group and multi-goup CFAs (same script like in Mede, Schäfer, & Füchslin, 2020)
cfamodel_fit   <- cfa(cfascale, data = dslong[dslong$t %in% c("2019 (panel)", "2020 (panel)"), ],
                      meanstructure = T, estimator = "MLM")
cfamodel_fit_t <- cfa(cfascale, data =  dslong[dslong$t %in% c("2019 (panel)", "2020 (panel)"), ], 
                      meanstructure = T, estimator = "MLM", group = "t")

# Test model fit 
summary(cfamodel_fit, fit.measures = T, standardized = T)
summary(cfamodel_fit_t, fit.measures = T, standardized = T) # Scale performs very well





#### MAIN ANALYSES ##############################################################################################

#### H1: Examine SciPop differences between 2019 and 2020 -------------------------------------------------------

# .. Descriptive analyses --------------------------------------------------------------------------------------- 

# Only use panel data from now on
dslongpnl <- dslong[dslong$t %in% c("2019 (panel)", "2020 (panel)"), ]


# Bar charts of valid percentages of single items
placeholder <- paste(rep("\U2003", 10), sep= "", collapse="")

freqplot_scipop1_ppl <- validfreqplot(scipop1_ppl, 1) + xlab(paste0(placeholder, "\nWhat unites the\nordinary people is\nthat they trust their\ncommon sense in\neveryday life.\n", placeholder))
freqplot_scipop2_ppl <- validfreqplot(scipop2_ppl, 1) + xlab(paste0(placeholder, "\nOrdinary people\nare of good and\nhonest character.\n", placeholder))
freqplot_scipop3_eli <- validfreqplot(scipop3_eli, 1) + xlab(paste0(placeholder, "\nScientists are\nonly after their\nown advantage.\n", placeholder))
freqplot_scipop4_eli <- validfreqplot(scipop4_eli, 1) + xlab(paste0(placeholder, "\nScientists are\nin cahoots with\npolitics and business.\n", placeholder))
freqplot_scipop5_dec <- validfreqplot(scipop5_dec, 1) + xlab(paste0(placeholder, "\nThe people should\nhave influence\non the work of\nscientists.\n"))
freqplot_scipop6_dec <- validfreqplot(scipop6_dec, 1) + xlab(paste0(placeholder, "\nPeople like me\nshould be involved\nin decisions about\nthe topics scientists\nresearch.\n", placeholder))
freqplot_scipop7_tru <- validfreqplot(scipop7_tru, 1) + xlab(paste0(placeholder, "\nIn case of doubt,\none should rather\ntrust the life experience\nof ordinary people\nthan the estimations\nof scientists.\n", placeholder))
freqplot_scipop8_tru <- validfreqplot(scipop8_tru, 1) + xlab(paste0(placeholder, "\nWe should rely more\non common sense\nand less on scientific\nstudies.\n", placeholder))


# Create the legend
freqplot_legend <- dslongpnl %>% 
  group_by(t) %>% count(scipop1_ppl) %>% drop_na() %>% 
  mutate(Percent = n / sum(n)*100) %>%
  ggplot(aes(x = t, y = Percent, fill = fct_rev(factor(scipop1_ppl)))) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_fill_grey(limits = rev, start = 0.8, end = 0.05, 
                  labels = c("1 (fully disagree)", "2", "3", "4", "5 (fully agree)")) +
  guides(fill = guide_legend(title = "Response", reverse = F)) +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 360, hjust = 0, vjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.title.x = element_blank())

freqplot_legend %<>% get_legend() %>% as_ggplot()


# Arrange plots including legend
ggarrange(freqplot_scipop1_ppl, freqplot_scipop2_ppl, freqplot_scipop3_eli, freqplot_scipop4_eli, 
          freqplot_scipop5_dec, freqplot_scipop6_dec, freqplot_scipop7_tru, freqplot_scipop8_tru,
          freqplot_legend,
          nrow = 9)



# Means, SDs, skewness, kurtosis of scale and subscale
meantbl <- as.data.frame(matrix(0, nrow = 2)) # Create empty dataframe
groups <- quos(scipopgoertz, scipopppl, scipopeli, scipopdec, scipoptru)  # Create quosures

for (j in seq_along(groups)) {
  meantbl <- cbind(meantbl,
                   group_by(dslongpnl, t) %>%
                     summarise(mean = mean(!!groups[[j]], na.rm = T),
                               sd = sd(!!groups[[j]], na.rm = T),
                               skewness = skewness(!!groups[[j]], na.rm = T),
                               kurtosis = kurtosis(!!groups[[j]], na.rm = T)))
  
  names(meantbl)[names(meantbl) == "mean"] <- paste0(quo_text(groups[[j]]), " (M)")
  names(meantbl)[names(meantbl) == "sd"] <- paste0(quo_text(groups[[j]]), " (SD)")
  names(meantbl)[names(meantbl) == "skewness"] <- paste0(quo_text(groups[[j]]), " (Skewness)")
  names(meantbl)[names(meantbl) == "kurtosis"] <- paste0(quo_text(groups[[j]]), " (Kurtosis)")
  
  meantbl <- select(meantbl, contains("scipop")) %>% 
    set_rownames(c("2019", "2020"))
}


# Long form table
round(meantbl, 2) %>%
  t() %>% 
  as.data.frame() %>% 
  kable(format = "html") %>% 
  kable_styling(bootstrap_options = "hover")


# Wide form table
round(meantbl, 2) %>%   kable(format = "html") %>% 
  kable_styling(bootstrap_options = "hover")


# Plot means
dslongpnl %>%
  melt(id.vars = c("id", "t"),
       measure.vars = c("scipopgoertz", "scipopppl", "scipopeli", "scipopdec", "scipoptru"),
       variable.name = "scale", value.name = "value") %>% 
  ggplot(aes(x = scale, y = value, fill = t)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge", aes(width = 0.6)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", aes(width = 0.6)) +
  stat_summary(aes(label = round(..y.., 2)), fun = mean, geom = "text", show.legend = F, size = 4, vjust = -2,
               position = position_dodge(width = 0.6)) +
  labs(title = "Mean Subscale Scores in 2019 and 2020", x = "Score", y = "Mean") + 
  scale_fill_manual(name = "Year", labels = c("2019", "2020"), 
                    values = gray.colors(2, start = 0.4, end = 0.85)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4), breaks = seq(0, 4, 0.25)) +
  scale_x_discrete(labels = c("SciPop Score\n(Goertz)", 
                              "PPL Subscale\n", "ELI Subscale\n", "DEC Subscale\n", "TRU Subscale\n")) +
  theme_minimal() +
  theme(legend.position = "right")





# .. Paired sample t-tests --------------------------------------------------------------------------------------

# Convert data to wide format
dswidepnl <- merge(dslong[dslong$t == "2019 (panel)",], 
                   dslong[dslong$t == "2020 (panel)",], 
                   by = "id", all.y = T, suffixes = c("_19", "_20"))


# Paired-sample t tests
ttest_scipopgoertz <- t.test(dswidepnl$scipopgoertz_19, dswidepnl$scipopgoertz_20, paired = T, "two.sided")
ttest_scipopppl    <- t.test(dswidepnl$scipopppl_19, dswidepnl$scipopppl_20, paired = T, "two.sided")
ttest_scipopeli    <- t.test(dswidepnl$scipopeli_19, dswidepnl$scipopeli_20, paired = T,"two.sided")
ttest_scipopdec    <- t.test(dswidepnl$scipopdec_19, dswidepnl$scipopdec_20, paired = T,"two.sided")
ttest_scipoptru    <- t.test(dswidepnl$scipoptru_19, dswidepnl$scipoptru_20, paired = T,"two.sided")


# Print nice table
rbind(mutate(tidy(ttest_scipopgoertz), dv = "SciPop Score (Goertz)"), 
      mutate(tidy(ttest_scipopppl), dv = "PPL Subscale Score (Mean)"), 
      mutate(tidy(ttest_scipopeli), dv = "ELI Subscale Score (Mean)"), 
      mutate(tidy(ttest_scipopdec), dv = "DEC Subscale Score (Mean)"), 
      mutate(tidy(ttest_scipoptru), dv = "TRU Subscale Score (Mean)")) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  select(`Test variable` = dv, `Mean difference` = estimate, t = statistic, df = parameter, 
         `CI 2.5%` = conf.low, `CI 97.5%` = conf.high, p = p.value) %>%
  kable(format = "html") %>% 
  kable_styling(bootstrap_options = "hover")





# .. Multilevel models with random intercepts for subjects and within-subjects covariates -----------------------

# Backlog:
# https://stats.stackexchange.com/questions/444095/how-to-control-for-within-subject-covariate-in-a-single-arm-mirror-image-study
# https://stats.stackexchange.com/questions/165187/how-to-handle-age-and-timepoint-variables-in-a-mixed-model-for-longitudinal-data?rq=1
# https://easystats.github.io/insight/reference/get_variance.html

# Specify predictors of models
preds_lmers <- c("t", "age", "gender", "lingreg_D", "lingreg_I", "urbanity_log", "edu_uni", "edu_com", 
                 "sciprox_score", "polorientation", "religiosity", "interestscience", 
                 "trustscience", "trustscientists", "covid_score", "(1 | id)")


# Fit null model without predictors and extract variances
lmer_scipopgoertz_null_vars <- lmer(scipopgoertz ~ 1 + (1 | id), data = dslongpnl) %>% get_variance()
lmer_scipopppl_null_vars    <- lmer(scipopppl ~ 1 + (1 | id), data = dslongpnl) %>% get_variance()
lmer_scipopeli_null_vars    <- lmer(scipopeli ~ 1 + (1 | id), data = dslongpnl) %>% get_variance()
lmer_scipopdec_null_vars    <- lmer(scipopdec ~ 1 + (1 | id), data = dslongpnl) %>% get_variance()
lmer_scipoptru_null_vars    <- lmer(scipoptru ~ 1 + (1 | id), data = dslongpnl) %>% get_variance()


# Extract variances
lmervars(lmer_scipopgoertz_null_vars)
lmervars(lmer_scipopppl_null_vars)
lmervars(lmer_scipopeli_null_vars)
lmervars(lmer_scipopdec_null_vars)
lmervars(lmer_scipoptru_null_vars)


# Fit full models
lmer_scipopgoertz <- lmer(reformulate(response = "scipopgoertz", preds_lmers), data = dslongpnl)
lmer_scipopppl    <- lmer(reformulate(response = "scipopppl", preds_lmers), data = dslongpnl)
lmer_scipopeli    <- lmer(reformulate(response = "scipopeli", preds_lmers), data = dslongpnl)
lmer_scipopdec    <- lmer(reformulate(response = "scipopdec", preds_lmers), data = dslongpnl)
lmer_scipoptru    <- lmer(reformulate(response = "scipoptru", preds_lmers), data = dslongpnl)


# Print nice tables (with Kenward-Rogers df approximation -> more accurate p values)
tab_model(lmer_scipopgoertz, lmer_scipopppl, lmer_scipopeli, lmer_scipopdec, lmer_scipoptru,
          p.val = "kr", show.df = T, show.ci = F, show.se = T, string.est = "b", string.se = "SE", 
          title = "Multilevel models with random intercepts for subjects and within-subjects covariates",
          dv.labels = c("SciPop Score (Goertz)",
                        "PPL Subscale Score (Mean)", "ELI Subscale Score (Mean)",
                        "DEC Subscale Score (Mean)", "TRU Subscale Score (Mean)"),
          pred.labels = c("(Intercept)", "Year (1 = 2020)", "Age", "Gender (1 = female)", 
                          "Linguistic region (1 = German-speaking)", "Linguistic region (1 = Italian-speaking)",
                          "Urbanity", "Education (1 = University degree)", "Education (1 = Compulsory school)", 
                          "Proximity to science", "Political orientation (7 = right)", "Religiosity", 
                          "Interest in science", "Trust in science", "Trust in scientists", 
                          "Affected by COVID-19 (risk group/SARS-CoV-2 test)"))




#### RQ1: Examine predictors of change in SciPop Scores and subscale scores -------------------------------------

# .. Visual inspection with Sankey plot -------------------------------------------------------------------------

# Visualize within-subject change in SciPop Score in Sankey plot
dslongpnl %>%
  group_by(t, scipopgoertz) %>%
  summarise(scipopgoertz_freq = n()) %>%
  merge(dslongpnl, by = c("scipopgoertz", "t")) %>%
  transform(scipopgoertz_lvls = factor(scipopgoertz, 
                                       levels = c("5", "4.5", "4", "3.5", "3", "2.5", "2", "1.5", "1", "NA"))) %>%
  replace_na(list(scipopgoertz_lvls = "NA")) %>%
  ggplot(aes(x = t, stratum = scipopgoertz_lvls, alluvium = id,
             y = scipopgoertz_freq,
             fill = scipopgoertz_lvls, label = scipopgoertz_lvls)) +
  scale_x_discrete(expand = c(.1, .1)) +
  scale_fill_viridis_d() +
  geom_flow(width = .1) +
  geom_stratum(alpha = .5, width = .1) +
  geom_text(stat = "stratum", size = 4) +
  ggtitle("Sankey plot of within-subject change in SciPop Score") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# .. Multiple linear regressions predicting SciPop difference with baseline values of covariates ----------------

# Compute differences
dswidepnl$scipopgoertz_diff <- dswidepnl$scipopgoertz_20 - dswidepnl$scipopgoertz_19
dswidepnl$scipopppl_diff <- dswidepnl$scipopppl_20 - dswidepnl$scipopppl_19
dswidepnl$scipopeli_diff <- dswidepnl$scipopeli_20 - dswidepnl$scipopeli_19
dswidepnl$scipopdec_diff <- dswidepnl$scipopdec_20 - dswidepnl$scipopdec_19
dswidepnl$scipoptru_diff <- dswidepnl$scipoptru_20 - dswidepnl$scipoptru_19


# Specify predictors of models
preds_diffmods <- c("age_19", "gender_19", "lingreg_D_19", "lingreg_I_19", "urbanity_log_19", "edu_uni_19", 
                    "edu_com_19", "sciprox_score_19", "polorientation_19", "religiosity_19", 
                    "interestscience_19", "trustscience_19", "trustscientists_19", "covid_score_20")


# Fit models
lm_scipopgoertz_diff <- lm(reformulate(response = "scipopgoertz_diff", c("scipopgoertz_19", preds_diffmods)), data = dswidepnl)
lm_scipopppl_diff    <- lm(reformulate(response = "scipopppl_diff", c("scipopppl_19", preds_diffmods)), data = dswidepnl)
lm_scipopeli_diff    <- lm(reformulate(response = "scipopeli_diff", c("scipopeli_19", preds_diffmods)), data = dswidepnl)
lm_scipopdec_diff    <- lm(reformulate(response = "scipopdec_diff", c("scipopdec_19", preds_diffmods)), data = dswidepnl)
lm_scipoptru_diff    <- lm(reformulate(response = "scipoptru_diff", c("scipoptru_19", preds_diffmods)), data = dswidepnl)


# Print nice tables
tab_model(lm_scipopgoertz_diff, lm_scipopppl_diff, lm_scipopeli_diff, lm_scipopdec_diff, lm_scipoptru_diff,
          show.ci = F, show.se = T,
          string.est = "b", string.se = "SE",
          title = "Multiple linear regressions predicting SciPop difference with baseline values of covariates",
          dv.labels = c("2019-2020 change in SciPop Score (Goertz)",
                        "2019-2020 change in PPL Subscale Score (Mean)", 
                        "2019-2020 change in ELI Subscale Score (Mean)",
                        "2019-2020 change in DEC Subscale Score (Mean)", 
                        "2019-2020 change in TRU Subscale Score (Mean)"),
          order.terms = c(1:2, 17:20, 3:16),
          pred.labels = c("(Intercept)", 
                          "SciPop Score 2019 (Goertz)",
                          "Age", "Gender (1 = female)", 
                          "Linguistic region (1 = German-speaking)", "Linguistic region (1 = Italian-speaking)",
                          "Urbanity", "Education (1 = University degree)", "Education (1 = Compulsory school)", 
                          "Proximity to science", "Political orientation (7 = right)", "Religiosity", 
                          "Interest in science", "Trust in science", "Trust in scientists",
                          "Affected by COVID-19 (risk group/SARS-CoV-2 test)",
                          "PPL Subscale Score 2019",
                          "ELI Subscale Score 2019",
                          "DEC Subscale Score 2019",
                          "TRU Subscale Score 2019"))




#### ROBUSTNESS TESTS 1: Test H1 with repeated-measures ANOVA with time-varying covariates ----------------------

# Backlog:
# https://stats.stackexchange.com/questions/439787/how-do-i-analyze-the-treatment-effect-while-controlling-for-covariates-in-a-pret
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2613314/ (good comparison of ANOVA and MLM, explains differences)

# Fit ANOVA models after selecting complete cases and excluding unused variables
aov_scipopgoertz <- complete.cases.within(dslongpnl, c("scipopgoertz", preds_lmers[-c(16)]), "id") %>% 
  select(c("scipopgoertz", "id", preds_lmers[-c(16)])) %>%
  ezANOVA(dv = .(scipopgoertz), wid = .(id), within = .(t),
          within_covariates = .(age, gender, lingreg_D, lingreg_I, urbanity_log, edu_uni, edu_com, sciprox_score,
                                polorientation, religiosity, interestscience, trustscience, trustscientists,
                                covid_score), 
          type = 3, return_aov = T)

aov_scipopppl <- complete.cases.within(dslongpnl, c("scipopppl", preds_lmers[-c(16)]), "id") %>% 
  select(c("scipopppl", "id", preds_lmers[-c(16)])) %>%
  ezANOVA(dv = .(scipopppl), wid = .(id), within = .(t),
          within_covariates = .(age, gender, lingreg_D, lingreg_I, urbanity_log, edu_uni, edu_com, sciprox_score,
                                polorientation, religiosity, interestscience, trustscience, trustscientists,
                                covid_score), 
          type = 3, return_aov = T)

aov_scipopeli <- complete.cases.within(dslongpnl, c("scipopeli", preds_lmers[-c(16)]), "id") %>% 
  select(c("scipopeli", "id", preds_lmers[-c(16)])) %>%
  ezANOVA(dv = .(scipopeli), wid = .(id), within = .(t),
          within_covariates = .(age, gender, lingreg_D, lingreg_I, urbanity_log, edu_uni, edu_com, sciprox_score,
                                polorientation, religiosity, interestscience, trustscience, trustscientists,
                                covid_score), 
          type = 3, return_aov = T)

aov_scipopdec <- complete.cases.within(dslongpnl, c("scipopdec", preds_lmers[-c(16)]), "id") %>% 
  select(c("scipopdec", "id", preds_lmers[-c(16)])) %>%
  ezANOVA(dv = .(scipopdec), wid = .(id), within = .(t),
          within_covariates = .(age, gender, lingreg_D, lingreg_I, urbanity_log, edu_uni, edu_com, sciprox_score,
                                polorientation, religiosity, interestscience, trustscience, trustscientists,
                                covid_score), 
          type = 3, return_aov = T)

aov_scipoptru <- complete.cases.within(dslongpnl, c("scipoptru", preds_lmers[-c(16)]), "id") %>% 
  select(c("scipoptru", "id", preds_lmers[-c(16)])) %>%
  ezANOVA(dv = .(scipoptru), wid = .(id), within = .(t),
          within_covariates = .(age, gender, lingreg_D, lingreg_I, urbanity_log, edu_uni, edu_com, sciprox_score,
                                polorientation, religiosity, interestscience, trustscience, trustscientists,
                                covid_score), 
          type = 3, return_aov = T)

# Print nice table
rbind(aov_scipopgoertz$ANOVA,
      aov_scipopppl$ANOVA, 
      aov_scipopeli$ANOVA, 
      aov_scipopdec$ANOVA, 
      aov_scipoptru$ANOVA) %>% 
  mutate(Scale = c("SciPop Score (Goertz)", "PPL Subscale Score (Mean)", "ELI Subscale Score (Mean)",
                   "DEC Subscale Score (Mean)", "TRU Subscale Score (Mean)")) %>%
  select(Scale, `df (numerator)` = DFn, `df (denominator)` = DFd, `F`, p, `generalized eta-squared` = ges) %>% 
  mutate_if(is.numeric, round, 3) %>%
  kable(format = "html", row.names = F) %>% 
  kable_styling(bootstrap_options = "hover")



 
 
 
#### ROBUSTNESS TESTS 2: Repeat analyses with alternative SciPop Scores -----------------------------------------

# -- Prepare alternative scores ---------------------------------------------------------------------------------

# Compute Bollen Score Mean (use unweighted mean across all items)
dslongpnl %<>% mutate(scipopbollenm = rowMeans(select(dslongpnl, scipop1_ppl:scipop8_tru), na.rm = T))

dslongpnl$scipopbollenm[is.nan(dslongpnl$scipopbollenm)] <- NA


# Compute Bollen Score CFA (extract factor scores from CFA with one latent factor; van Hauwaert et al, 2019)
cfa1F <- 'scipopbollencfa =~ scipop1_ppl + scipop2_ppl + scipop3_eli + scipop4_eli + 
                             scipop5_dec + scipop6_dec + scipop7_tru + scipop8_tru'

cfa1F_fit <- cfa(model = cfa1F, data = dslongpnl, 
                 ordered = c("scipop1_ppl", "scipop2_ppl", "scipop3_eli", "scipop4_eli",
                             "scipop5_dec", "scipop6_dec", "scipop7_tru", "scipop8_tru"))

dslongpnl$seqid <- seq.int(nrow(dslongpnl))

dslongpnl %<>% left_join(cbind.data.frame(seqid = cfa1F_fit@Data@case.idx[[1]], lavPredict(cfa1F_fit)), 
                      by = "seqid", all.x = T)


# Compute Sartori Score 75% percentile (classify respondent as populist if they reach threshold on all subscales)
# use 75% percentile as threshold as Wuttke et al. do (2020, p. 363)
dslongpnl %<>% mutate(scipopsartori75 = case_when(scipopppl >= quantile(scipopppl, 0.75, na.rm = T) &
                                                    scipopeli >= quantile(scipopeli, 0.75, na.rm = T) & 
                                                    scipopdec >= quantile(scipopdec, 0.75, na.rm = T) & 
                                                    scipoptru >= quantile(scipoptru, 0.75, na.rm = T)~1, T~0))

dslongpnl$scipopsartori75[is.nan(dslongpnl$scipopsartori75)] <- NA


# Compute Sartori Score categorical (classify respondent as populist if they agree with at least six statements)
# similar to what Vehrkamp & Merkel (2020) have done
dslongpnl %<>% mutate(scipopsartoricat_scipop1_ppl = case_when(scipop1_ppl>3~1, scipop1_ppl<=3~0, T~NA_real_),
                      scipopsartoricat_scipop2_ppl = case_when(scipop2_ppl>3~1, scipop2_ppl<=3~0, T~NA_real_),
                      scipopsartoricat_scipop3_eli = case_when(scipop3_eli>3~1, scipop3_eli<=3~0, T~NA_real_),
                      scipopsartoricat_scipop4_eli = case_when(scipop4_eli>3~1, scipop4_eli<=3~0, T~NA_real_),
                      scipopsartoricat_scipop5_dec = case_when(scipop5_dec>3~1, scipop5_dec<=3~0, T~NA_real_),
                      scipopsartoricat_scipop6_dec = case_when(scipop6_dec>3~1, scipop6_dec<=3~0, T~NA_real_),
                      scipopsartoricat_scipop7_tru = case_when(scipop7_tru>3~1, scipop7_tru<=3~0, T~NA_real_),
                      scipopsartoricat_scipop8_tru = case_when(scipop8_tru>3~1, scipop8_tru<=3~0, T~NA_real_)) %>%
  mutate(scipopsartoricat = case_when(
    rowSums(select(., scipopsartoricat_scipop1_ppl:scipopsartoricat_scipop8_tru)) >= 6 ~ 1,
    rowSums(select(., scipopsartoricat_scipop1_ppl:scipopsartoricat_scipop8_tru)) < 6 ~ 0,
    T ~ NA_real_)) %>%
  select(-c(scipopsartoricat_scipop1_ppl:scipopsartoricat_scipop8_tru))


# Check if all scores have been calculated correctly:
dslongpnl %>% filter(t %in% c("2019 (panel)", "2020 (panel)")) %>% 
  select(scipop1_ppl:scipop8_tru, scipopppl:scipoptru,
         scipopgoertz, scipopbollenm, scipopbollencfa, scipopsartori75, scipopsartoricat) # %>% view()


# "Update" dswide for RQ1 models
dswidepnl <- merge(dslongpnl[dslongpnl$t == "2019 (panel)",], 
                   dslongpnl[dslongpnl$t == "2020 (panel)",], 
                   by = "id", all.y = T, suffixes = c("_19", "_20"))


# Compute differences of alternative SciPop Scores
dswidepnl$scipopbollenm_diff    <- dswidepnl$scipopbollenm_20 - dswidepnl$scipopbollenm_19
dswidepnl$scipopbollencfa_diff  <- dswidepnl$scipopbollencfa_20 - dswidepnl$scipopbollencfa_19
dswidepnl$scipopsartori75_diff  <- dswidepnl$scipopsartori75_20 - dswidepnl$scipopsartori75_19
dswidepnl$scipopsartoricat_diff <- dswidepnl$scipopsartoricat_20 - dswidepnl$scipopsartoricat_19




# -- H1 analyses ------------------------------------------------------------------------------------------------

# .. Descriptive analyses --------------------------------------------------------------------------------------- 

# Plot means, SDs, skewness, kurtosis of alternative SciPop Scores
meantbl <- as.data.frame(matrix(0, nrow = 2))
groups <- quos(scipopgoertz, scipopbollenm, scipopbollencfa, scipopsartori75, scipopsartoricat)

for (j in seq_along(groups)) {
  meantbl <- cbind(meantbl,
                   group_by(dslongpnl, t) %>%
                     summarise(mean = mean(!!groups[[j]], na.rm = T),
                               sd = sd(!!groups[[j]], na.rm = T),
                               skewness = skewness(!!groups[[j]], na.rm = T),
                               kurtosis = kurtosis(!!groups[[j]], na.rm = T)))
  
  names(meantbl)[names(meantbl) == "mean"] <- paste0(quo_text(groups[[j]]), " (M)")
  names(meantbl)[names(meantbl) == "sd"] <- paste0(quo_text(groups[[j]]), " (SD)")
  names(meantbl)[names(meantbl) == "skewness"] <- paste0(quo_text(groups[[j]]), " (Skewness)")
  names(meantbl)[names(meantbl) == "kurtosis"] <- paste0(quo_text(groups[[j]]), " (Kurtosis)")
  
  meantbl <- select(meantbl, contains("scipop")) %>% 
    set_rownames(c("2019", "2020"))
}


# Long form table
round(meantbl, 2) %>%
  t() %>% 
  as.data.frame() %>% 
  kable(format = "html") %>% 
  kable_styling(bootstrap_options = "hover")


# Wide form table
round(meantbl, 2) %>%   kable(format = "html") %>% 
  kable_styling(bootstrap_options = "hover")


# Plot means
dslongpnl %>%
  melt(id.vars = c("id", "t"),
       measure.vars = c("scipopgoertz", "scipopbollenm", "scipopbollencfa"),
       variable.name = "scale", value.name = "value") %>% 
  ggplot(aes(x = scale, y = value, fill = t)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge", aes(width = 0.6)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", aes(width = 0.6)) +
  stat_summary(aes(label = round(..y.., 2)), fun = mean, geom = "text", show.legend = F, size = 4, vjust = -2,
               position = position_dodge(width = 0.6)) +
  labs(title = "Mean Continuous SciPop Scores in 2019 and 2020", x = "Score", y = "Mean") + 
  scale_fill_manual(name = "Year", labels = c("2019", "2020"), 
                    values = gray.colors(2, start = 0.4, end = 0.85)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4), breaks = seq(0, 4, 0.25)) +
  scale_x_discrete(labels = c("SciPop Score\n(Goertz)", "SciPop Score\n(Bollen: Mean)", 
                              "SciPop Score\n(Bollen: CFA)")) +
  theme_minimal() +
  theme(legend.position = "right")



dslongpnl %>%
  melt(id.vars = c("id", "t"),
       measure.vars = c("scipopsartori75", "scipopsartoricat"),
       variable.name = "scale", value.name = "value") %>% 
  ggplot(aes(x = scale, y = value, fill = t)) +
  stat_summary(geom = "bar", fun = mean, position = "dodge", aes(width = 0.6)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", aes(width = 0.6)) +
  stat_summary(aes(label = round(..y.., 3)), fun = mean, geom = "text", show.legend = F, size = 4, vjust = -4,
               position = position_dodge(width = 0.6)) +
  labs(title = "Mean Discrete SciPop Scores in 2019 and 2020", x = "Score", y = "Mean") + 
  scale_fill_manual(name = "Year", labels = c("2019", "2020"), 
                    values = gray.colors(2, start = 0.4, end = 0.85)) + 
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 0.2, 0.025)) +
  coord_cartesian(ylim=c(0,0.2)) +
  scale_x_discrete(labels = c("SciPop Score\n(Sartori: 75% percentile)", 
                              "SciPop Score\n(Sartori: Agree 6+ items)")) +
  theme_minimal() +
  theme(legend.position = "right")



# .. Paired sample t-tests --------------------------------------------------------------------------------------
ttest_scipopbollenm    <- t.test(dswidepnl$scipopbollenm_19, dswidepnl$scipopbollenm_20, paired = T, "two.sided")
ttest_scipopbollencfa  <- t.test(dswidepnl$scipopbollencfa_19, dswidepnl$scipopbollencfa_20, paired = T, "two.sided")
ttest_scipopsartori75  <- t.test(dswidepnl$scipopsartori75_19, dswidepnl$scipopsartori75_20, paired = T, "two.sided")
ttest_scipopsartoricat <- t.test(dswidepnl$scipopsartoricat_19, dswidepnl$scipopsartoricat_20, paired = T, "two.sided")


# Print nice table
rbind(mutate(tidy(ttest_scipopgoertz), dv = "SciPop Score (Goertz)"), 
      mutate(tidy(ttest_scipopbollenm), dv = "SciPop Score (Bollen: Mean)"),
      mutate(tidy(ttest_scipopbollencfa), dv = "SciPop Score (Bollen: CFA)"), 
      mutate(tidy(ttest_scipopsartori75), dv = "SciPop Score (Sartori: 75% percentile)"), 
      mutate(tidy(ttest_scipopsartoricat), dv = "SciPop Score (Sartori: Agree 6+ items)")) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  select(`Test variable` = dv, `Mean difference` = estimate, t = statistic, df = parameter, 
         `CI 2.5%` = conf.low, `CI 97.5%` = conf.high, p = p.value) %>%
  kable(format = "html") %>% 
  kable_styling(bootstrap_options = "hover")



# .. Multilevel models with random intercepts for subjects and within-subjects covariates -----------------------

# Fit H1 regression models (boundary issues for the logistic models because too few data and too many predictors)
lmer_scipopbollenm     <- lmer(reformulate(response = "scipopbollenm", preds_lmers), dslongpnl)
lmer_scipopbollencfa   <- lmer(reformulate(response = "scipopbollencfa", preds_lmers), dslongpnl)
glmer_scipopsartori75  <- glmer(reformulate(response = "scipopsartori75", preds_lmers), dslongpnl, binomial)
glmer_scipopsartoricat <- glmer(reformulate(response = "scipopsartoricat", preds_lmers), dslongpnl, binomial)


# Print nice tables
tab_model(lmer_scipopgoertz, lmer_scipopbollenm, lmer_scipopbollencfa,
          p.val = "kr", show.df = T, show.ci = F, show.se = T, string.est = "b", string.se = "SE", 
          title = "Multilevel models with random intercepts for subjects and within-subjects covariates",
          dv.labels = c("SciPop Score (Goertz)", "SciPop Score (Bollen: Mean)", "SciPop Score (Bollen: CFA)"),
          pred.labels = c("(Intercept)", "Year (1 = 2020)", "Age", "Gender (1 = female)", 
                          "Linguistic region (1 = German-speaking)", "Linguistic region (1 = Italian-speaking)",
                          "Urbanity", "Education (1 = University degree)", "Education (1 = Compulsory school)", 
                          "Proxmity to science", "Political orientation (7 = right)", "Religiosity", 
                          "Interest in science", "Trust in science", "Trust in scientists",
                          "Affected by COVID-19 (risk group/SARS-CoV-2 test)"))

tab_model(glmer_scipopsartori75, glmer_scipopsartoricat,
          transform = NULL, show.ci = F, show.se = T, string.se = "SE", 
          title = "Multilevel models with random intercepts for subjects and within-subjects covariates",
          dv.labels = c("SciPop Score (Sartori: 75% percentile)", "SciPop Score (Sartori: Agree 6+ items)"),
          pred.labels = c("(Intercept)", "Year (1 = 2020)", "Age", "Gender (1 = female)", 
                          "Linguistic region (1 = German-speaking)", "Linguistic region (1 = Italian-speaking)",
                          "Urbanity", "Education (1 = University degree)", "Education (1 = Compulsory school)", 
                          "Proxmity to science", "Political orientation (7 = right)", "Religiosity", 
                          "Interest in science", "Trust in science", "Trust in scientists",
                          "Affected by COVID-19 (risk group/SARS-CoV-2 test)"))





# .. Test H1 with repeated-measures ANOVA with time-varying covariates ------------------------------------------

# Backlog: https://stats.idre.ucla.edu/spss/faq/how-can-i-do-repeated-measures-anova-with-covariates-in-spss/

# Fit ANOVA models after selecting complete cases and excluding unused variables
aov_scipopbollenm <- complete.cases.within(dslongpnl, c("scipopbollenm", preds_lmers[-c(16)]), "id") %>% 
  select(c("scipopbollenm", "id", preds_lmers[-c(16)])) %>%
  ezANOVA(dv = .(scipopbollenm), wid = .(id), within = .(t),
          within_covariates = .(age, gender, lingreg_D, lingreg_I, urbanity_log, edu_uni, edu_com, sciprox_score,
                                polorientation, religiosity, interestscience, trustscience, trustscientists,
                                covid_score), 
          type = 3, return_aov = T)

aov_scipopbollencfa <- complete.cases.within(dslongpnl, c("scipopbollencfa", preds_lmers[-c(16)]), "id") %>% 
  select(c("scipopbollencfa", "id", preds_lmers[-c(16)])) %>%
  ezANOVA(dv = .(scipopbollencfa), wid = .(id), within = .(t),
          within_covariates = .(age, gender, lingreg_D, lingreg_I, urbanity_log, edu_uni, edu_com, sciprox_score,
                                polorientation, religiosity, interestscience, trustscience, trustscientists,
                                covid_score), 
          type = 3, return_aov = T)

aov_scipopsartori75 <- complete.cases.within(dslongpnl, c("scipopsartori75", preds_lmers[-c(16)]), "id") %>% 
  select(c("scipopsartori75", "id", preds_lmers[-c(16)])) %>%
  ezANOVA(dv = .(scipopsartori75), wid = .(id), within = .(t),
          within_covariates = .(age, gender, lingreg_D, lingreg_I, urbanity_log, edu_uni, edu_com, sciprox_score,
                                polorientation, religiosity, interestscience, trustscience, trustscientists,
                                covid_score), 
          type = 3, return_aov = T)

aov_scipopsartoricat <- complete.cases.within(dslongpnl, c("scipopsartoricat", preds_lmers[-c(16)]), "id") %>% 
  select(c("scipopsartoricat", "id", preds_lmers[-c(16)])) %>%
  ezANOVA(dv = .(scipopsartoricat), wid = .(id), within = .(t),
          within_covariates = .(age, gender, lingreg_D, lingreg_I, urbanity_log, edu_uni, edu_com, sciprox_score,
                                polorientation, religiosity, interestscience, trustscience, trustscientists,
                                covid_score), 
          type = 3, return_aov = T)


# Print nice table
rbind(aov_scipopgoertz$ANOVA,
      aov_scipopbollenm$ANOVA, 
      aov_scipopbollencfa$ANOVA, 
      aov_scipopsartori75$ANOVA, 
      aov_scipopsartoricat$ANOVA) %>% 
  mutate(Scale = c("SciPop Score (Goertz)", "SciPop Score (Bollen: Mean)", "SciPop Score (Bollen: CFA)",
                   "SciPop Score (Sartori: 75% percentile)", "SciPop Score (Sartori: Agree 6+ items)")) %>%
  select(Scale, `df (numerator)` = DFn, `df (denominator)` = DFd, `F`, p, `generalized eta-squared` = ges) %>% 
  mutate_if(is.numeric, round, 4) %>%
  kable(format = "html", row.names = F) %>% 
  kable_styling(bootstrap_options = "hover")






#--- RQ1 analyses ----------------------------------------------------------------------------------------------

# .. Multiple linear regressions predicting SciPop difference with baseline values of covariates ----------------

# Fit RQ1 models with alternative SciPop Scores
lm_scipopbollenm_diff    <- lm(reformulate(response = "scipopbollenm_diff", c("scipopbollenm_19", preds_diffmods)), dswidepnl)
lm_scipopbollencfa_diff  <- lm(reformulate(response = "scipopbollencfa_diff", c("scipopbollencfa_19", preds_diffmods)), dswidepnl)
lm_scipopsartori75_diff  <- lm(reformulate(response = "scipopsartori75_diff", c("scipopsartori75_19", preds_diffmods)), dswidepnl)
lm_scipopsartoricat_diff <- lm(reformulate(response = "scipopsartoricat_diff", c("scipopsartoricat_19", preds_diffmods)), dswidepnl)


# Print nice tables
tab_model(lm_scipopgoertz_diff, 
          lm_scipopbollenm_diff, lm_scipopbollencfa_diff, lm_scipopsartori75_diff, lm_scipopsartoricat_diff,
          show.ci = F, show.se = T,
          string.est = "b", string.se = "SE",
          title = "Multiple linear regressions predicting SciPop difference with baseline values of covariates",
          dv.labels = c("2019-2020 change in SciPop Score (Goertz)", 
                        "2019-2020 change in SciPop Score (Bollen: Mean)", 
                        "2019-2020 change in SciPop Score (Bollen: CFA)",
                        "2019-2020 change in SciPop Score (Sartori: 75% percentile)", 
                        "2019-2020 change in SciPop Score (Sartori: Agree 6+ items)"),
          order.terms = c(1:2, 17:20, 3:16),
          pred.labels = c("(Intercept)", 
                          "SciPop Score 2019 (Goertz)",
                          "Age", "Gender (1 = female)", 
                          "Linguistic region (1 = German-speaking)", "Linguistic region (1 = Italian-speaking)",
                          "Urbanity", "Education (1 = University degree)", "Education (1 = Compulsory school)", 
                          "Proximity to science", "Political orientation (7 = right)", "Religiosity", 
                          "Interest in science", "Trust in science", "Trust in scientists",
                          "Affected by COVID-19 (risk group/SARS-CoV-2 test)",
                          "SciPop Score (Bollen: Mean)",
                          "SciPop Score 2019 (Bollen: CFA)",
                          "SciPop Score 2019 (Sartori: 75% percentile)",
                          "SciPop Score 2019 (Sartori: Agree 6+ items)"))



#### Assumption checks ------------------------------------------------------------------------------------------

# VIFs (no multicollinearity)
car::vif(lmer_scipopgoertz)
car::vif(lmer_scipopppl)
car::vif(lmer_scipopeli)
car::vif(lmer_scipopdec)
car::vif(lmer_scipoptru)
car::vif(lmer_scipopbollenm)
car::vif(lmer_scipopbollencfa)
car::vif(glmer_scipopsartori75)
car::vif(glmer_scipopsartoricat)

car::vif(lm_scipopgoertz_diff)
car::vif(lm_scipopppl_diff)
car::vif(lm_scipopeli_diff)
car::vif(lm_scipopdec_diff)
car::vif(lm_scipoptru_diff)
car::vif(lm_scipopbollenm_diff)
car::vif(lm_scipopbollencfa_diff)
car::vif(lm_scipopsartori75_diff)
car::vif(lm_scipopsartoricat_diff)



# Distributions (look good)
autoplot(lm_scipopgoertz_diff)
autoplot(lm_scipopppl_diff)
autoplot(lm_scipopeli_diff)
autoplot(lm_scipopdec_diff)
autoplot(lm_scipoptru_diff)
autoplot(lm_scipopbollenm_diff)
autoplot(lm_scipopbollencfa_diff)
autoplot(lm_scipopsartori75_diff)
autoplot(lm_scipopsartoricat_diff)

