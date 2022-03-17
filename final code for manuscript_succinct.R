##### Packages needed for data exploration or analysis (try to keep current)#####
library(agricolae) #v1.3-5
library(ggplot2)#v3.3.5
library(emmeans) #v1.6.1
library(multcomp) #v1.4-17
library(multcompView)#v0.8-1
library(lme4) #v1.1-27.1
library(lmerTest) #v3.1-3
library(ggsci) #v2.9
library(ggpubr) #v0.4.0
library(readxl) 
library(dplyr) #v1.0.7
library(cowplot)#v1.1.1
library(Rcpp)#v1.0.7

citation("agricolae")
citation("ggplot2")
citation("lsmeans")
citation("lme4")
citation("lmerTest")
citation("ggsci")
citation("ggpubr")
citation("readxl")

getwd()

#### Reading in the data for both years and all locations ####
fld_trl_2018 <- read_excel("2018 MI data.xlsx")
fld_trl_2019 <- read_excel("2019 MI data.xlsx")
Indiana_2019 <- read_excel("IN data for R.xlsx")
Minnesota_2019 <- read_excel("MN 2019.xlsx")

level.order <- c("NAKED", "BASE", "INTEGO.SUITE")

#### summary values for all varieties & treatments, both MI years ####

#MI 2018
fld_trl_MI2018_summary <- fld_trl_2018 %>%
  group_by(VAR, FST) %>%
  summarise(mean_stand_vc = mean(AVG.VC),
            mean_plantwt_vc = mean(weight.per.plant.VC),
            mean_plantwt_v2 = mean(weight.per.plantV2),
            mean_yield = mean(Yield.kgha))

write.csv(fld_trl_MI2018_summary, "2018_MI_field_trial_summary.csv")

# VC MI 2018 individual plant weights within variety by seed treatment
lm_MI18.total.wt.perplant.VC <- lmer(weight.per.plant.VC ~ FST * VAR  + (1|REP), data = fld_trl_2018)
anova(lm_MI18.total.wt.perplant.VC, test.statistic="F", type = 1)
lsmeans_MI18.total.wt.perplant.VC <- lsmeans::lsmeans(lm_MI18.total.wt.perplant.VC, "FST", by = "VAR")
Results_MI18.lsmeans.totalwt.perplant.VC <- cld(lsmeans_MI18.total.wt.perplant.VC, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)

# V2 MI 2018 individual plant weights within variety by seed treatment
lm_MI18.total.wt.perplant.V2 <- lmer(weight.per.plantV2 ~ FST * VAR  + (1|REP), data = fld_trl_2018)
anova(lm_MI18.total.wt.perplant.V2, test.statistic="F", type = 1)
lsmeans_MI18.total.wt.perplant.V2 <- lsmeans::lsmeans(lm_MI18.total.wt.perplant.V2, "FST", by = "VAR")
Results_MI18.lsmeans.totalwt.perplant.V2 <- cld(lsmeans_MI18.total.wt.perplant.V2, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)

# MI 2019
fld_trl_MI2019_summary <- fld_trl_2019 %>%
  group_by(VAR, FST) %>%
  summarise(mean_stand_vc = mean(AVG.VC),
            mean_plantwt_vc = mean(totalwt.perplant.vc),
            mean_plantwt_v2 = mean(totalwt.perplant.grams.v2),
            mean_yield = mean(Yield.kgha))

write.csv(fld_trl_MI2019_summary, "2019_MI_field_trial_summary.csv")

# VC MI 2019 individual plant weights within variety by seed treatment
lm_MI19.total.wt.perplant.VC <- lmer(totalwt.perplant.vc ~ FST * VAR  + (1|Rep), data = fld_trl_2019)
anova(lm_MI19.total.wt.perplant.VC, test.statistic="F", type = 1)
lsmeans_MI19.total.wt.perplant.VC <- lsmeans::lsmeans(lm_MI19.total.wt.perplant.VC, "FST", by = "VAR")
Results_MI19.lsmeans.totalwt.perplant.VC <- cld(lsmeans_MI19.total.wt.perplant.VC, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)

# V2 MI 2019 individual plant weights within variety by seed treatment
lm_MI19.total.wt.perplant.V2 <- lmer(totalwt.perplant.grams.v2 ~ FST * VAR  + (1|Rep), data = fld_trl_2019)
anova(lm_MI19.total.wt.perplant.V2, test.statistic="F", type = 1) # not significant by seed treatment
lsmeans_total.wt.perplant.V2 <- lsmeans::lsmeans(lm_MI19.total.wt.perplant.V2, "FST", by = "VAR")
Results_lsmeans.totalwt.perplant.V2 <- cld(lsmeans_total.wt.perplant.V2, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)

#### summary values for all varieties & seed treatments, IN & MN 2019 ####
# IN 2019
fld_trl_IN2019_summary <- Indiana_2019 %>%
  group_by(VAR, FST) %>%
  summarise(mean_stand_vc = mean(AVG.VC),
            mean_plantwt_vc = mean(totalwt.perplant.vc),
            mean_plantwt_v2 = mean(totalwt.perplant.v2),
            mean_yield = mean(Yield.kgha))

write.csv(fld_trl_MI2018_summary, "2018_MI_field_trial_summary.csv")

# VC IN 2019 individual plant weights within variety by seed treatment
lm_IN19.total.wt.perplant.VC <- lmer(totalwt.perplant.vc ~ FST * VAR  + (1|Rep), data = Indiana_2019)
anova(lm_IN19.total.wt.perplant.VC, test.statistic="F", type = 1)
lsmeans_IN19.total.wt.perplant.VC <- lsmeans::lsmeans(lm_IN19.total.wt.perplant.VC, "FST", by = "VAR")
Results_IN19.lsmeans.totalwt.perplant.VC <- cld(lsmeans_IN19.total.wt.perplant.VC, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)

# V2 IN 2019 individual plant weights within variety by seed treatment
lm_IN19.total.wt.perplant.V2 <- lmer(totalwt.perplant.v2 ~ FST * VAR  + (1|Rep), data = Indiana_2019)
anova(lm_IN19.total.wt.perplant.V2, test.statistic="F", type = 1)
lsmeans_IN19.total.wt.perplant.V2 <- lsmeans::lsmeans(lm_IN19.total.wt.perplant.V2, "FST", by = "VAR")
Results_IN19.lsmeans.totalwt.perplant.V2 <- cld(lsmeans_IN19.total.wt.perplant.V2, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)


# MN 2019
fld_trl_MN2019_summary <- Minnesota_2019 %>%
  group_by(var, fst) %>%
  summarise(mean_stand_vc = mean(perc.vc.stand),
            mean_plantwt_vc = mean(totalwt.perplant.vc),
            mean_plantwt_v2 = mean(totalwt.perplant.v2),
            mean_yield = mean(Yield.kgha))

write.csv(fld_trl_MI2019_summary, "2019_MN_field_trial_summary.csv")

# VC Mn 2019 individual plant weights within variety by seed treatment
lm_MN19.total.wt.perplant.VC <- lmer(totalwt.perplant.vc ~ fst * var  + (1|rep), data = Minnesota_2019)
anova(lm_MN19.total.wt.perplant.VC, test.statistic="F", type = 1)
lsmeans_MN19.total.wt.perplant.VC <- lsmeans::lsmeans(lm_MN19.total.wt.perplant.VC, "fst", by = "var")
Results_MN19.lsmeans.totalwt.perplant.VC <- cld(lsmeans_MN19.total.wt.perplant.VC, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)

# V2 Mn 2019 individual plant weights within variety by seed treatment
lm_MI19.total.wt.perplant.V2 <- lmer(totalwt.perplant.v2 ~ fst * var  + (1|rep), data = Minnesota_2019)
anova(lm_MI19.total.wt.perplant.V2, test.statistic="F", type = 1)
lsmeans_total.wt.perplant.V2 <- lsmeans::lsmeans(lm_MI19.total.wt.perplant.V2, "fst", by = "var")
Results_lsmeans.totalwt.perplant.V2 <- cld(lsmeans_total.wt.perplant.V2, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)

#### investigation the role of partial resistance ####

#### 2018 MI dry individual plant weights by partial resistance ####
AG_plantwtv2_t.test <- fld_trl_2018 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "AG27x8" | VAR == "AG28x7") %>%
  t.test(weight.per.plantV2 ~ Tolerance, data = .) ### no significant (p = 0.2251) differences in weight, MS has higher (+0.139 grams) mean plant weights

C_plantwtv2_t.test <- fld_trl_2018 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "C1340RX" | VAR == "C2888RX") %>%
  t.test(weight.per.plantV2 ~ Tolerance, data = .) ### no significant (p = 0.2594) differences in weight, MR has higher (+0.1606 grams) mean plant weights

H_plantwtv2_t.test <- fld_trl_2018 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "H2512NX" | VAR == "H2862NX") %>%
  t.test(weight.per.plantV2 ~ Tolerance, data = .) ### no significant (p = 0.6096) differences in weight, MS has higher (+0.0724 grams) plant weights

NK_plantwtv2_t.test <- fld_trl_2018 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "NK2788X" | VAR == "NK3195X") %>%
  t.test(weight.per.plantV2 ~ Tolerance, data = .) ### BARELY (p = 0.05614) no significant differences in weight, MS has higher (+0.2934) plant weights

# DF seeds only had a single variety in our trial in 2018, cannot compare it to others within the company this year

#### 2019 MI plant wt by tolerance ####
AG_2019MI_plantwtv2_t.test <- fld_trl_2019 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "AG27x8" | VAR == "AG28x7") %>%
  t.test(totalwt.perplant.grams.v2 ~ Tolerance, data = .) ### no significant (p = 0.08543) differences in weight, MR has higher (+0.2696 grams) mean plant weights

C_2019MI_plantwtv2_t.test <- fld_trl_2019 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "C3140RX" | VAR == "C2888RX") %>%
  t.test(totalwt.perplant.grams.v2 ~ Tolerance, data = .) ### no significant (p = 0.1475) differences in weight, MS has higher (+0.3186 grams) mean plant weights

H_2019MI_plantwtv2_t.test <- fld_trl_2019 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "H2512NX" | VAR == "H2862NX") %>%
  t.test(totalwt.perplant.grams.v2 ~ Tolerance, data = .) ### no significant (p = 0.8527) differences in weight, MR has higher (+0.0612 grams) mean plant weights

DF_2019MI_plantwtv2_t.test <- fld_trl_2019 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "DF5193" | VAR == "DF5252") %>%
  t.test(totalwt.perplant.grams.v2 ~ Tolerance, data = .) ### no significant (p = 0.93) differences in weight, MR has higher (+0.0226) plant weights
#DF2790X did not have a field trial rating, contact DFseeds to see if they have nay info!

#### 2019 IN dry individual plant weights by tolerance ####

AG_2019IN_plantwtv2_t.test <- Indiana_2019 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "AG27X8" | VAR == "AG28X7") %>%
  t.test(totalwt.perplant.v2 ~ Tolerance, data = .) ### no significant (p = 0.2264) differences in weight, MS has higher (+0.0875 grams) mean plant weights

H_2019IN_plantwtv2_t.test <- Indiana_2019 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "H2512NX" | VAR == "H2862NX") %>%
  t.test(totalwt.perplant.v2 ~ Tolerance, data = .) ### no significant (p = 0.3923) differences in weight, MS has higher (+0.096 grams) mean plant weights

#### 2019 MN individual plant weights by tolerance ####

AG_2019MN_plantwtv2_t.test <- Minnesota_2019 %>%
  subset(., fst == "NON-TREATED") %>%
  subset(., var == "AG27x8" | var == "AG28x7") %>%
  t.test(totalwt.perplant.v2 ~ Tolerance, data = .) ### no significant (p = 0.8252) differences in weight, MS has higher (+0.164 grams) mean plant weights

H_2019MN_plantwtv2_t.test <- Minnesota_2019 %>%
  subset(., fst == "NON-TREATED") %>%
  subset(., var == "H2512NX" | var == "H2862NX") %>%
  t.test(totalwt.perplant.v2 ~ Tolerance, data = .) ### no significant (p = 0.3809) differences in weight, MS has higher (+0.644 grams) mean plant weights

####2018 MI yield by field tolerance score significance ####

AG_yield.kgha_t.test <- fld_trl_2018 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "AG27x8" | VAR == "AG28x7") %>%
  t.test(Yield.kgha ~ Tolerance, data = .) ### no significant (p = 0.1503) differences in yield, MR has higher (+543.807 kg/ha) mean yield.kgha

C_yield.kgha_t.test <- fld_trl_2018 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "C1340RX" | VAR == "C2888RX") %>%
  t.test(Yield.kgha ~ Tolerance, data = .) ### significant (p = 0.00556) differences in Yield.kgha, MR has higher (+551.337 kg/ha) mean yield.kgha

H_yield.kgha_t.test <- fld_trl_2018 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "H2512NX" | VAR == "H2862NX") %>%
  t.test(Yield.kgha ~ Tolerance, data = .) ### no significant (p = 0.973) differences in Yield.kgha, MR has higher (+9.259 kg/ha) plant weights

NK_yield.kgha_t.test <- fld_trl_2018 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "NK2788X" | VAR == "NK3195X") %>%
  t.test(Yield.kgha ~ Tolerance, data = .) ### no significant (p = 0.2218) differences in Yield.kgha, MS has higher (+430.701 kg/ha) plant weights

# only 1 DFseed variety this year (2018), did not compare it to others

DF5268R2Y_2018_mean_yield_kgha <- fld_trl_2018 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "DF5268R2Y") %>%
  summary(Yield.kgha)

print(DF5268R2Y_2018_mean_yield_kgha, digits = 5)

## 2018 MI comparison of all varieties yield, nontreated seed only
MI_2018_yield.kgha_non_treated <- fld_trl_2018 %>%
  subset(., FST == "NON-TREATED")
lm_yield_kgha.nontreated_MI18 <- lmer(Yield.kgha ~ VAR  + (1|REP), data = MI_2018_yield.kgha_non_treated)
anova_yield_kgha <- anova(lm_yield_kgha.nontreated_MI18, test.statistic="F", type = 1)
lsmeans_yield_kgha_nontreated_MI18 <- lsmeans::lsmeans(lm_yield_kgha.nontreated_MI18, "VAR")
Results_lsmeans.yield_kgha_nontreated_MI18 <- cld(lsmeans_yield_kgha_nontreated_MI18, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)

#### 2019 MI yield by field tolerance score significance ####

AG_2019MI_yield.kgha_t.test <- fld_trl_2019 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "AG27x8" | VAR == "AG28x7") %>%
  t.test(Yield.kgha ~ Tolerance, data = .) ### no significant (p = 0.1331) differences in yield, MR has higher (+578.303 kg/ha) mean yield.kgha

C_2019MI_yield.kgha_t.test <- fld_trl_2019 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "C3140RX" | VAR == "C2888RX") %>%
  t.test(Yield.kgha ~ Tolerance, data = .) ### no significant (p = 0.3272) differences in Yield.kgha, MS has higher (+645.322) mean yield.kgha

H_2019MI_yield.kgha_t.test <- fld_trl_2019 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "H2512NX" | VAR == "H2862NX") %>%
  t.test(Yield.kgha ~ Tolerance, data = .) ### no significant (p = 0.3762) differences in Yield.kgha, MS has higher (+453.342) mean yield.kgha

DF_2019MI_yield.kgha_t.test <- fld_trl_2019 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "DF5193" | VAR == "DF5252") %>%
  t.test(Yield.kgha ~ Tolerance, data = .) ### no significant (p = 0.08308) differences in Yield.kgha, MR has higher (+577.956) mean yield.kgha
# no tolerance data on DF2790X, contact DFseeds to see if they have more information
DF7290X_mean_yield_kgha <- fld_trl_2019 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "DF7290NX") %>%
  summary(Yield.kgha)

print(DF7290X_mean_yield_kgha, digits = 5)

## MI 2019 comparison of all varieties yield, nontreated seed only
MI_2019_yield.kgha_non_treated <- fld_trl_2019 %>%
  subset(., FST == "NON-TREATED")
lm_yield_kgha.nontreated_MI19 <- lmer(Yield.kgha ~ VAR  + (1|Rep), data = MI_2019_yield.kgha_non_treated)
anova_yield_kgha <- anova(lm_yield_kgha.nontreated_MI19, test.statistic="F", type = 1)
lsmeans_yield_kgha_nontreated_MI19 <- lsmeans::lsmeans(lm_yield_kgha.nontreated_MI19, "VAR")
Results_lsmeans.yield_kgha_nontreated_MI19 <- cld(lsmeans_yield_kgha_nontreated_MI19, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)


#### 2019 IN Yield.kgha by tolerance ####

AG_2019IN_yield.kgha_t.test <- Indiana_2019 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "AG27X8" | VAR == "AG28X7") %>%
  t.test(Yield.kgha ~ Tolerance, data = .) ### no significant (p = 0.5485) differences in yield, MS has higher (+166.081) mean yield.kgha

H_2019IN_yield.kgha_t.test <- Indiana_2019 %>%
  subset(., FST == "NON-TREATED") %>%
  subset(., VAR == "H2512NX" | VAR == "H2862NX") %>%
  t.test(Yield.kgha ~ Tolerance, data = .) ### no significant (p = 0.2656) differences in yield, MR has higher (+361.101) mean yield.kgha

## comparison of all varieties yield, nontreated seed only
IN_2019_yield.kgha_non_treated <- Indiana_2019 %>%
  subset(., FST == "NON-TREATED")
lm_yield_kgha.nontreated_IN19 <- lmer(Yield.kgha ~ VAR  + (1|Rep), data = IN_2019_yield.kgha_non_treated)
anova_yield_kgha <- anova(lm_yield_kgha.nontreated_IN19, test.statistic="F", type = 1)
lsmeans_yield_kgha_nontreated_IN19 <- lsmeans::lsmeans(lm_yield_kgha.nontreated_IN19, "VAR")
Results_lsmeans.yield_kgha_nontreated_IN19 <- cld(lsmeans_yield_kgha_nontreated_IN19, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)


#### 2019 MN Yield.kgha by tolerance ####

AG_2019MN_yield.kgha_t.test <- Minnesota_2019 %>%
  subset(., fst == "NON-TREATED") %>%
  subset(., var == "AG27x8" | var == "AG28x7") %>%
  t.test(Yield.kgha ~ Tolerance, data = .) ### no significant (p = 0.8551) differences in yield, MR has higher (+44.632) mean Yield.kgha

H_2019MN_yield.kgha_t.test <- Minnesota_2019 %>%
  subset(., fst == "NON-TREATED") %>%
  subset(., var == "H2512NX" | var == "H2862NX") %>%
  t.test(Yield.kgha ~ Tolerance, data = .) ### no significant (p = 0.6268) differences in yield, MS has higher (+54.154) mean Yield.kgha

## comparison of all varieties yield, nontreated seed only
MN_2019_yield.kgha_non_treated <- Minnesota_2019 %>%
  subset(., fst == "NON-TREATED")
lm_yield_kgha.nontreated_MN19 <- lmer(Yield.kgha ~ var  + (1|rep), data = MN_2019_yield.kgha_non_treated)
anova_yield_kgha <- anova(lm_yield_kgha.nontreated_MN19, test.statistic="F", type = 1)
lsmeans_yield_kgha_nontreated_MN19 <- lsmeans::lsmeans(lm_yield_kgha.nontreated_MN19, "var")
Results_lsmeans.yield_kgha_nontreated_MN19 <- cld(lsmeans_yield_kgha_nontreated_MN19, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)


#### Figure 1, 2018 MI only - stand and yield ####

stand_figure_2018MI <- ggplot(fld_trl_2018,aes(x = factor(FST, level = level.order), y = perc.avg.vc, fill = FST)) +
  scale_fill_npg() +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5) +
  stat_summary(fun=mean,position=position_dodge(width=0.95), geom="bar") +
  stat_summary(fun.data = mean_se,position=position_dodge(width=0.95), geom = "errorbar") +
  theme_gray() +
  facet_wrap(~VAR, ncol = 9)+
  guides(fill = "none") +
  ## scale_fill_manual(values = col[1:2]) +
  theme(axis.text.x = element_text(size = 15, face = "bold", angle=45, hjust=1, family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 10, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 10, face="bold", family = "serif"),
        legend.position = "right",
        strip.text.x = element_text(size = 15, face = "bold", family = "serif"),
        title = element_text(size = 15, family = "serif")) +
  ggtitle("2018 Michigan Percentage Emerged Plant Stand") +
  xlab("Treatment") + ylab("Percent Plant Stand")

lm_stand.MIVC <- lmer(perc.avg.vc ~ FST * VAR  + (1|REP), data = fld_trl_2018)
anova(lm_stand.MIVC, test.statistic="F", type = 1)
lsmeans_stand.MIVC <- lsmeans::lsmeans(lm_stand.MIVC, "FST", by = "VAR")
Results_lsmeans.stand.MI18VC <- cld(lsmeans_stand.MIVC, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)
Results_lsmeans.stand.MI18VC

yield_figure_2018MI <- ggplot(fld_trl_2018,aes(x = factor(FST, level = level.order), y = Yield.kgha, fill = FST)) +
  scale_fill_npg() +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5) +
  stat_summary(fun=mean,position=position_dodge(width=0.95), geom="bar") +
  stat_summary(fun.data = mean_se,position=position_dodge(width=0.95), geom = "errorbar") +
  theme_gray() +
  facet_wrap(~VAR, ncol = 9)+
  guides(fill = "none") +
  ## scale_fill_manual(values = col[1:2]) +
  theme(axis.text.x = element_text(size = 15, face = "bold", angle=45, hjust=1, family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 10, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 10, face="bold", family = "serif"),
        legend.position = "right",
        strip.text.x = element_text(size = 15, face = "bold", family = "serif"),
        title = element_text(size = 15, family = "serif")) +
  ggtitle("2018 Michigan Final yields (kg/Ha)") +
  xlab("Treatment") + ylab("Yield (kg/Ha)")

lm_yield_kgha.MIVC <- lmer(Yield.kgha ~ FST * VAR  + (1|REP), data = fld_trl_2018)
anova(lm_yield_kgha.MIVC, test.statistic="F", type = 1)
lsmeans_yield_kgha.MIVC <- lsmeans::lsmeans(lm_yield_kgha.MIVC, "FST", by = "VAR")
Results_lsmeans.stand.MI18VC <- cld(lsmeans_yield_kgha.MIVC, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)
Results_lsmeans.stand.MI18VC

plot_grid(stand_figure_2018MI, yield_figure_2018MI, nrow = 2)

#### Figure 2, 2019 MI only - stand and yield ####

stand_figure_2019MI <- ggplot(fld_trl_2019,aes(x = factor(FST, level = level.order), y = perc.avg.vc, fill = FST)) +
  scale_fill_npg() +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5) +
  stat_summary(fun=mean,position=position_dodge(width=0.95), geom="bar") +
  stat_summary(fun.data = mean_se,position=position_dodge(width=0.95), geom = "errorbar") +
  theme_gray() +
  facet_wrap(~VAR, ncol = 9)+
  guides(fill = "none") +
  ## scale_fill_manual(values = col[1:2]) +
  theme(axis.text.x = element_text(size = 15, face = "bold", angle=45, hjust=1, family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 10, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 10, face="bold", family = "serif"),
        legend.position = "right",
        strip.text.x = element_text(size = 15, face = "bold", family = "serif"),
        title = element_text(size = 15, family = "serif")) +
  ggtitle("2019 Michigan Percentage Emerged Plant Stand") +
  xlab("Treatment") + ylab("Percent Plant Stand")

lm_stand.MI19VC <- lmer(perc.avg.vc ~ FST * VAR  + (1|Rep), data = fld_trl_2019)
anova(lm_stand.MI19VC, test.statistic="F", type = 1)
lsmeans_stand.MI19VC <- lsmeans::lsmeans(lm_stand.MI19VC, "FST", by = "VAR")
Results_lsmeans.stand.MI19VC <- cld(lsmeans_stand.MI19VC, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)
Results_lsmeans.stand.MI19VC

yield_figure_2019MI <- ggplot(fld_trl_2019,aes(x = factor(FST, level = level.order), y = Yield.kgha, fill = FST)) +
  scale_fill_npg() +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5) +
  stat_summary(fun=mean,position=position_dodge(width=0.95), geom="bar") +
  stat_summary(fun.data = mean_se,position=position_dodge(width=0.95), geom = "errorbar") +
  theme_gray() +
  facet_wrap(~VAR, ncol = 9)+
  guides(fill = "none") +
  ## scale_fill_manual(values = col[1:2]) +
  theme(axis.text.x = element_text(size = 15, face = "bold", angle=45, hjust=1, family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 10, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 10, face="bold", family = "serif"),
        legend.position = "right",
        strip.text.x = element_text(size = 15, face = "bold", family = "serif"),
        title = element_text(size = 15, family = "serif")) +
  ggtitle("2019 Michigan Final Yields (kg/Ha)") +
  xlab("Treatment") + ylab("Yield (kg/Ha)")

lm_yield_kgha.MI19 <- lmer(Yield.kgha ~ FST * VAR  + (1|Rep), data = fld_trl_2019)
anova(lm_yield_kgha.MI19, test.statistic="F", type = 1)
lsmeans_yield_kgha.MI19 <- lsmeans::lsmeans(lm_yield_kgha.MI19, "FST", by = "VAR")
Results_lsmeans.stand.MI19 <- cld(lsmeans_yield_kgha.MI19, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)
Results_lsmeans.stand.MI19

plot_grid(stand_figure_2019MI, yield_figure_2019MI, nrow = 2)


#### Figure 3. IN '19 percent stand and yield ####

stand_figure_2019IN <- ggplot(Indiana_2019,aes(x = factor(FST, level = level.order), y = perc.avg.vc, fill = FST)) +
  scale_fill_npg() +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5) +
  stat_summary(fun=mean,position=position_dodge(width=0.95), geom="bar") +
  stat_summary(fun.data = mean_se,position=position_dodge(width=0.95), geom = "errorbar") +
  theme_gray() +
  facet_wrap(~VAR, ncol = 9)+
  guides(fill = "none") +
  ## scale_fill_manual(values = col[1:2]) +
  theme(axis.text.x = element_text(size = 15, face = "bold", angle=45, hjust=1, family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 10, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 10, face="bold", family = "serif"),
        legend.position = "right",
        strip.text.x = element_text(size = 15, face = "bold", family = "serif"),
        title = element_text(size = 15, family = "serif")) +
  ggtitle("2019 Indiana Percentage Emerged Plant Stand") +
  xlab("Treatment") + ylab("Percent Plant Stand")

lm_stand.INVC <- lmer(perc.avg.vc ~ FST * VAR  + (1|Rep), data = Indiana_2019)
anova(lm_stand.INVC, test.statistic="F", type = 1)
lsmeans_stand.INVC <- lsmeans::lsmeans(lm_stand.INVC, "FST", by = "VAR")
Results_lsmeans.stand.INVC <- cld(lsmeans_stand.INVC, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)
Results_lsmeans.stand.INVC

yield_figure_2019IN <- ggplot(Indiana_2019,aes(x = factor(FST, level = level.order), y = Yield.kgha, fill = FST)) +
  scale_fill_npg() +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5) +
  stat_summary(fun=mean,position=position_dodge(width=0.95), geom="bar") +
  stat_summary(fun.data = mean_se,position=position_dodge(width=0.95), geom = "errorbar") +
  theme_gray() +
  facet_wrap(~VAR, ncol = 9)+
  guides(fill = "none") +
  ## scale_fill_manual(values = col[1:2]) +
  theme(axis.text.x = element_text(size = 15, face = "bold", angle=45, hjust=1, family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 10, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 10, face="bold", family = "serif"),
        legend.position = "right",
        strip.text.x = element_text(size = 15, face = "bold", family = "serif"),
        title = element_text(size = 15, family = "serif")) +
  ggtitle("2019 Indiana Final Yields (kg/Ha)") +
  xlab("Treatment") + ylab("Yield (kg/Ha)")

lm_yield.IN <- lmer(Yield.kgha ~ FST * VAR  + (1|Rep), data = Indiana_2019)
anova(lm_yield.IN, test.statistic="F", type = 1)
lsmeans_IN_yieldkgha <- lsmeans::lsmeans(lm_yield.IN, "FST", by = "VAR")
Results_lsmeans.yield.kgha.IN <- cld(lsmeans_IN_yieldkgha, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)
Results_lsmeans.yield.kgha.IN

plot_grid(stand_figure_2019IN, yield_figure_2019IN, nrow = 2)

#### Figure 4. 2019 MN stand and yield (kg/ha) ####

stand_figure_2019MN <- ggplot(Minnesota_2019,aes(x = factor(fst, level = level.order), y = perc.vc.stand, fill = fst)) +
  scale_fill_npg() +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5) +
  stat_summary(fun=mean,position=position_dodge(width=0.95), geom="bar") +
  stat_summary(fun.data = mean_se,position=position_dodge(width=0.95), geom = "errorbar") +
  theme_gray() +
  facet_wrap(~var, ncol = 9)+
  guides(fill = "none") +
  ## scale_fill_manual(values = col[1:2]) +
  theme(axis.text.x = element_text(size = 15, face = "bold", angle=45, hjust=1, family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 10, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 10, face="bold", family = "serif"),
        legend.position = "right",
        strip.text.x = element_text(size = 15, face = "bold", family = "serif"),
        title = element_text(size = 15, family = "serif")) +
  ggtitle("2019 Minnesota Percentage Emerged Plant Stand") +
  xlab("Treatment") + ylab("Percent Plant Stand")

lm_stand.MNVC <- lmer(perc.vc.stand ~ fst * var  + (1|rep), data = Minnesota_2019)
anova(lm_stand.MNVC, test.statistic="F", type = 1)
lsmeans_stand.MNVC <- lsmeans::lsmeans(lm_stand.MNVC, "fst", by = "var")
Results_lsmeans.stand.MNVC <- cld(lsmeans_stand.MNVC, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)
Results_lsmeans.stand.MNVC

yield_figure_2019MN <- ggplot(Minnesota_2019,aes(x = factor(fst, level = level.order), y = Yield.kgha, fill = fst)) +
  scale_fill_npg() +
  #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5) +
  stat_summary(fun=mean,position=position_dodge(width=0.95), geom="bar") +
  stat_summary(fun.data = mean_se,position=position_dodge(width=0.95), geom = "errorbar") +
  theme_gray() +
  facet_wrap(~var, ncol = 9)+
  guides(fill = "none") +
  ## scale_fill_manual(values = col[1:2]) +
  theme(axis.text.x = element_text(size = 15, face = "bold", angle=45, hjust=1, family = "serif"),
        axis.text.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.x = element_text(size = 20, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 20, face = "bold", family = "serif"),
        axis.line.x = element_line(colour = 'gray', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'gray', size=0.5, linetype='solid'),
        legend.text = element_text(size = 10, face = "bold", family = "serif"),
        legend.key = element_blank(),
        legend.title = element_text(size = 10, face="bold", family = "serif"),
        legend.position = "right",
        strip.text.x = element_text(size = 15, face = "bold", family = "serif"),
        title = element_text(size = 15, family = "serif")) +
  ggtitle("2019 Minnesota Final Yields (kg/Ha)") +
  xlab("Treatment") + ylab("Yield (kg/Ha)")

lm_yield.MN <- lmer(Yield.kgha ~ fst * var  + (1|rep), data = Minnesota_2019)
lmerTest::anova(lm_yield.MN, test.statistic="F", type = 1)
lsmeans_MN_yieldkgha <- lsmeans::lsmeans(lm_yield.MN, "fst", by = "var")
Results_lsmeans.yield.kgha.MN <- cld(lsmeans_MN_yieldkgha, alpha = 0.05, adjust = "tuk", Letters = letters, reversed = TRUE)


plot_grid(stand_figure_2019MN, yield_figure_2019MN, nrow = 2)