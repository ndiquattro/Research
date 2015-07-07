# Paper Graphs
library(dplyr)
library(extrafont)
library(ggplot2)
library(grid)
library(lme4)
library(tidyr)
library(zoo)

# Import fonts
loadfonts(device="win")

# Load trial report and fix errors
setwd("C:/Users/Jessica/Documents/Research/data")
df <- read.delim('trial_report_32015.txt', na.strings = c(" ", ".", "NA", ""))
df$rtype[df$rtype == "correj"] <- "cr"
df$rtype[df$rtype == "falsealarm"] <- "fa"
df$rtype[df$rtype == "hit"] <- "hi"
df$rtype[df$rtype == "miss"] <- "mi"

# Load IA report and fix errors
setwd("C:/Users/Jessica/Documents/Research/data")
df2 <- read.delim('IA_report_4282015.txt', na.strings = c(" ", ".", "NA", ""))

m <- merge(df, df2)

# Remove subjects below 40 TUTs (Remove: 1_zw and 45_sjk)
filter <- m %>%
  filter(!RECORDING_SESSION_LABEL == "1_zw",
         !RECORDING_SESSION_LABEL == "45_sjk")

# Trial data
td <- filter %>%
  filter(!is.na(tnum))

# Add TUT scores from last and remove any trials without a TUT score to fill with
df3 <- td %>%
  group_by(sub) %>%
  mutate(tutra2 = na.locf(tutra,
                          fromLast  = TRUE,
                          na.rm     = FALSE)) %>%
  filter(!is.na(tutra2))

# Physiological Data
pd1 <- df3 %>%
  group_by(sub) %>%
  mutate(
    # Rates
    hirate = sum(rtype == 'hi') / (sum(rtype == 'hi') + sum(rtype == 'mi')),
    crrate = sum(rtype == 'cr') / (sum(rtype == 'cr') + sum(rtype == 'fa')),
    mirate = sum(rtype == 'mi') / (sum(rtype == 'hi') + sum(rtype == 'mi')),
    farate = sum(rtype == 'fa') / (sum(rtype == 'cr') + sum(rtype == 'fa')))

# Fix false alarm rate
pd1$farate[pd1$farate == "0"] <- 0.000001

pd2 <- pd1 %>%
  group_by(sub) %>%
  mutate(dp   = qnorm(hirate) - qnorm(farate), # Dprime 
         mtut = mean(tutra2))                  # Mean TUT


pd3 <- pd2 %>%
  group_by(sub, rtype) %>%
  mutate(mrt = mean(rt))                       #  Mean Reaction Time for Correct Rejection, False Alarm, Hit, and Miss

  # Graph: number of participants with rtype for tut level
  table <- pd1 %>%
    count(rtype, tutra2) %>%
    spread(rtype, n)

  # Graph: number of subjects with mean tut level
  pd4 <- pd2 %>%
    subset(select = c(sub, mtut)) %>%
    unique()

  ggplot(pd4, aes(mtut))+
    geom_histogram(breaks = c(0, 1, 2, 3, 4, 5),
                   colour = "dimgrey",
                   fill   = "black")+
    labs(list(x = "Mean TUT Score",
              y = "Number of Participants"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))

  # Graph: dprime versus mean TUT
  ct1 <- pd2 %>%
    subset(select = c(sub, mtut, dp)) %>%
    unique()
  cor.test(ct1$mtut, ct1$dp)
  
  ggplot(ct1, aes(mtut, dp))+
    geom_point(colour = "dimgrey",
               size   = 2)+
    geom_smooth(colour  = "black",
                method  = "lm",
                se      = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "D'"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))

  # Graph: mean RT versus mean TUT score by correct rejection, false alarm, hit, and miss
  ct2 <- pd3 %>%
    subset(select = c(sub, mtut, mrt, rtype)) %>%
    unique()

  ct2cr <- ct2 %>%
    filter(rtype == "cr")
  cor.test(ct2cr$mtut, ct2cr$mrt)

  ct2fa <- ct2 %>%
    filter(rtype == "fa")
  cor.test(ct2fa$mtut, ct2fa$mrt)

  ct2h <- ct2 %>%
    filter(rtype == "hi")
  cor.test(ct2h$mtut, ct2h$mrt)

  ct2m <- ct2 %>%
    filter(rtype == "mi")
  cor.test(ct2m$mtut, ct2m$mrt)
  
    # Changing names of rtype
    levels(ct2$rtype)[levels(ct2$rtype)=="cr"] <- "Correct Rejection"
    levels(ct2$rtype)[levels(ct2$rtype)=="fa"] <- "False Alarm"
    levels(ct2$rtype)[levels(ct2$rtype)=="hi"] <- "Hit"
    levels(ct2$rtype)[levels(ct2$rtype)=="mi"] <- "Miss"
  
  ggplot(ct2, aes(mtut, mrt))+
    geom_point(colour = "dimgrey",
               size   = 2)+
    facet_wrap(~rtype)+
    geom_smooth(colour  = "black",
                method  = "lm",
                se      = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "Mean Reaction Time"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))

# Eye Data
ed <- pd2 %>%
  group_by(sub) %>%
  mutate(scrt   = SACCADE_COUNT/rt,               # Saccade Count/RT
         mscrt  = mean(scrt),                     # Mean Saccade Count/RT
         afdrt  = AVERAGE_FIXATION_DURATION/rt,   # Average Fixation Duration/RT
         mafdrt = mean(afdrt, na.rm = TRUE))      # Mean Average Fixation Duration/RT

edr <- pd2 %>%
  group_by(sub, rtype) %>%
  mutate(scrtr    = SACCADE_COUNT/rt,               # Saccade Count/RT for Correct Rejection, False Alarm, Hit, and Miss
         mscrtr   = mean(scrtr),                     # Mean Saccade Count/RT for Correct Rejection, False Alarm, Hit, and Miss
         afdrtr   = AVERAGE_FIXATION_DURATION/rt,   # Average Fixation Duration/RT for Correct Rejection, False Alarm, Hit, and Miss
         mfrtr    = mean(afdrtr, na.rm = TRUE))      # Mean Average Fixation Duration/RT for Correct Rejection, False Alarm, Hit, and Miss

ed2 <- ed %>%
  group_by(sub) %>%
  mutate(viacrt = VISITED_INTEREST_AREA_COUNT/rt, # Visited Interest Area Count/RT
         miacrt = mean(viacrt))                   # Mean Visited Interest Area Count/RT

ed3 <- ed2 %>%
  group_by(sub, tnum) %>%
  mutate(r = sum(IA_RUN_COUNT)-TRIAL_TOTAL_VISITED_IA_COUNT)  # IA Run Count-Trial Total Visited IA Count

ed4 <- ed3 %>%
  group_by(sub) %>%
  mutate(mr = mean(r, na.rm = TRUE))  # Mean IA Run Count-Trial Total Visited IA Count

ed3r <- edr %>%
  ungroup() %>%
  group_by(sub, tnum) %>%
  mutate(r = sum(IA_RUN_COUNT)-TRIAL_TOTAL_VISITED_IA_COUNT)  # IA Run Count-Trial Total Visited IA Count

ed4r <- ed3r %>%
  group_by(sub, rtype) %>%
  mutate(mrr = mean(r, na.rm = TRUE))  # Mean IA Run Count-Trial Total Visited IA Count

  # Graph: mean saccade count/rt versus mtut
  ct3a <- ed %>%
  subset(select = c(sub, mtut, mscrt)) %>%
  unique()

  ggplot(ct3a, aes(mtut, mscrt))+
    geom_point(colour = 'dimgrey',
               size   = 2)+
    geom_smooth(colour = "black",
                method  = "lm",
                se      = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "Mean Saccade Count\nper Unit Time"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))

  # Graph: mean saccade count/rt versus mtut by correct rejection, false alarm, hit, and miss
  ct3b <- edr %>%
    subset(select = c(sub, mtut, mscrtr, rtype)) %>%
    unique()
  
  ct3cr <- ct3b %>%
    filter(rtype == "cr")
  cor.test(ct3cr$mtut, ct3cr$mscrt)
  
  ct3fa <- ct3b %>%
    filter(rtype == "fa")
  cor.test(ct3fa$mtut, ct3fa$mscrt)
  
  ct3h <- ct3b %>%
    filter(rtype == "hi")
  cor.test(ct3h$mtut, ct3h$mscrt)
  
  ct3m <- ct3b %>%
    filter(rtype == "mi")
  cor.test(ct3m$mtut, ct3m$mscrt)
  
    # Changing names of rtype
    levels(ct3b$rtype)[levels(ct3b$rtype)=="cr"] <- "Correct Rejection"
    levels(ct3b$rtype)[levels(ct3b$rtype)=="fa"] <- "False Alarm"
    levels(ct3b$rtype)[levels(ct3b$rtype)=="hi"] <- "Hit"
    levels(ct3b$rtype)[levels(ct3b$rtype)=="mi"] <- "Miss"
  
  ggplot(ct3b, aes(mtut, mscrtr))+
    geom_point(colour = 'dimgrey',
               size   = 2)+
    facet_wrap(~rtype)+
    geom_smooth(colour = "black",
                method  = "lm",
                se      = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "Mean Saccade Count\nper Unit Time"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))

  # Graph: mean average fixation duration/rt
  ct4a <- ed %>%
    subset(select = c(sub, mtut, mafdrt)) %>%
    unique()
  
  ggplot(ct4a, aes(mtut, mafdrt))+
    geom_point(colour = 'dimgrey',
               size   = 2)+
    geom_smooth(colour = "black",
                method  = "lm",
                se      = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "Mean Average Fixation\nDuration per Unit Time"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))

  # Graph: mean average fixation duration/rt versus mtut by correct rejection, false alarm, hit, and miss
  ct4b <- edr %>%
    subset(select = c(sub, mtut, mfrtr, rtype)) %>%
    unique()
  
  ct4cr <- ct4b %>%
    filter(rtype == "cr")
  cor.test(ct4cr$mtut, ct4cr$mfrtr)
  
  ct4fa <- ct4b %>%
    filter(rtype == "fa")
  cor.test(ct4fa$mtut, ct4fa$mfrtr)
  
  ct4h <- ct4b %>%
    filter(rtype == "hi")
  cor.test(ct4h$mtut, ct4h$mfrtr)
  
  ct4m <- ct4b %>%
    filter(rtype == "mi")
  cor.test(ct4m$mtut, ct4m$mfrtr)

    # Changing names of rtype
    levels(ct4b$rtype)[levels(ct4b$rtype)=="cr"] <- "Correct Rejection"
    levels(ct4b$rtype)[levels(ct4b$rtype)=="fa"] <- "False Alarm"
    levels(ct4b$rtype)[levels(ct4b$rtype)=="hi"] <- "Hit"
    levels(ct4b$rtype)[levels(ct4b$rtype)=="mi"] <- "Miss"
  
  ggplot(ct4b, aes(mtut, mfrtr))+
    geom_point(colour = "dimgrey",
               size   = 2)+
    facet_wrap(~rtype)+
    geom_smooth(colour = "black",
                method = "lm", 
                se = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "Mean Average Fixation\nDuration per Unit Time"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))

  # Graph: mean visited interest area count/rt v mean TUT
  ct5 <- ed2 %>%
    subset(select = c(sub, mtut, miacrt)) %>%
    unique()
  cor.test(ct5$mtut, ct5$miacrt)
  
  ggplot(ct5, aes(mtut, miacrt))+
    geom_point(colour = "dimgrey",
               size = 2)+
    geom_smooth(colour = "black",
                method = "lm",
                se = FALSE)+
    labs(list(x = "Mean TUT Score", 
              y = "Mean Visited Interest Area\nCount per Unit Time"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))

# Graph: mean interest area count v mean average fixation duration per unit time
ct6 <- ed2 %>%
  subset(select = c(sub, mafdrt, miarc)) %>%
  unique()
cor.test(ct6$mafdrt, ct6$miarc)

ggplot(ct6, aes(mafdrt, miarc))+
  geom_point(colour = "dimgrey",
             size = 2)+
  geom_smooth(colour = "black",
              method = "lm",
              se = FALSE)+
  labs(list(x = "Mean Average Fixation Duration Per Unit Time", 
            y = "Mean Interest Area Run Count"))+
  theme(axis.title.x      = element_text(vjust = -0.2),
        axis.title.y      = element_text(vjust = 1.2),
        legend.text       = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 22),
        panel.background  = element_rect(fill = "white"),
        panel.grid.major  = element_line(colour = "white"),
        panel.grid.minor  = element_line(colour = "white"),
        text              = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 22))

  # Graph: mean IA run count-trial total visited IA count versus mtut by correct rejection, false alarm, hit, and miss
  ct5a <- ed4 %>%
    subset(select = c(sub, mtut, mr)) %>%
    unique()

  ggplot(ct5a, aes(mtut, mr))+
    geom_point(colour = "dimgrey",
               size = 2)+
    geom_smooth(colour = "black",
                method = "lm",
                se = FALSE)+
    labs(list(x = "Mean TUT Score", 
              y = "Mean Regressions"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))

  # Graph: mean IA run count-trial total visited IA count versus mtut by correct rejection, false alarm, hit, and miss
  ct5b <- ed4r %>%
    subset(select = c(sub, mtut, mrr, rtype)) %>%
    unique()

  ct5cr <- ct5b %>%
    filter(rtype == "cr")
  cor.test(ct5cr$mtut, ct5cr$mrr)
  
  ct5fa <- ct5b %>%
    filter(rtype == "fa")
  cor.test(ct5fa$mtut, ct5fa$mrr)
  
  ct5h <- ct5b %>%
    filter(rtype == "hi")
  cor.test(ct5h$mtut, ct5h$mrr)
  
  ct5m <- ct5b %>%
    filter(rtype == "mi")
  cor.test(ct5m$mtut, ct5m$mrr)
  
    # Changing names of rtype
    levels(ct5b$rtype)[levels(ct5b$rtype)=="cr"] <- "Correct Rejection"
    levels(ct5b$rtype)[levels(ct5b$rtype)=="fa"] <- "False Alarm"
    levels(ct5b$rtype)[levels(ct5b$rtype)=="hi"] <- "Hit"
    levels(ct5b$rtype)[levels(ct5b$rtype)=="mi"] <- "Miss"
  
  ggplot(ct5b, aes(mtut, mrr))+
    geom_point(colour = "dimgrey",
               size = 2)+
    facet_wrap(~rtype)+
    geom_smooth(colour = "black",
                method = "lm",
                se = FALSE)+
    labs(list(x = "Mean TUT Score", 
              y = "Mean Regressions"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))


# Linear Model
lm <- ed4 %>%
  ungroup() %>%
  filter(r< 25, scrt > 0) %>%
  select(sub, mscrt, mafdrt, mtut, mr) %>%
  unique() %>%
  mutate(tc = (mtut - mean(mtut))/sd(mtut),
         sc = (mscrt - mean(mscrt))/sd(mscrt),
         fc = (mafdrt - mean(mafdrt, na.rm = TRUE))/sd(mafdrt, na.rm = TRUE),
         rc = (mr - mean(mr))/sd(mr))

cor(select(lm, mr, mscrt, mafdrt, mtut))
#cor.test(~ mscrt + mafdrt, new)

sr <- lm(tc ~ rc + sc, lm)
summary(sr)

lmr <- ed4r %>%
  ungroup() %>%
  filter(r< 25, scrtr > 0) %>%
  select(sub, mscrtr, mfrtr, mtut, mrr, rtype) %>%
  unique() %>%
  mutate(tc = (mtut - mean(mtut))/sd(mtut),
         sc = (mscrtr - mean(mscrtr))/sd(mscrtr),
         fc = (mfrtr - mean(mfrtr, na.rm = TRUE))/sd(mfrtr, na.rm = TRUE),
         rc = (mrr - mean(mrr))/sd(mrr))

srr <- lm(tc ~ rc*rtype + sc*rtype, lmr)
summary(srr)

ggplot(lm, aes(tc, sc))+
  geom_point(colour = 'dimgrey',
             size   = 2)+
  geom_smooth(colour = "black",
              method  = "lm",
              se      = FALSE)+
  labs(list(x = "Mean TUT Score",
            y = "Mean Saccade Count\nper Unit Time"))+
  theme(axis.title.x      = element_text(vjust = -0.2),
        axis.title.y      = element_text(vjust = 1.2),
        legend.text       = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 22),
        panel.background  = element_rect(fill = "white"),
        panel.grid.major  = element_line(colour = "white"),
        panel.grid.minor  = element_line(colour = "white"),
        text              = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 22))

ggplot(lmr, aes(tc, sc))+
  geom_point(colour = 'dimgrey',
             size   = 2)+
  facet_wrap(~rtype)+
  geom_smooth(colour = "black",
              method  = "lm",
              se      = FALSE)+
  labs(list(x = "Mean TUT Score",
            y = "Mean Saccade Count\nper Unit Time"))+
  theme(axis.title.x      = element_text(vjust = -0.2),
        axis.title.y      = element_text(vjust = 1.2),
        legend.text       = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 22),
        panel.background  = element_rect(fill = "white"),
        panel.grid.major  = element_line(colour = "white"),
        panel.grid.minor  = element_line(colour = "white"),
        text              = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 22))
  
# Linear Mixed-Effects Model
lmem <- ed4 %>%
  ungroup() %>%
  filter(r < 25, scrt > 0) %>%
  select(sub, tnum, r, scrt, afdrt, tutra2) %>%
  unique() %>%
  group_by(sub) %>%
  mutate(rc = (r - mean(r))/sd(r),
         sc = (scrt - mean(scrt))/sd(scrt),
         fc = (afdrt - mean(afdrt, na.rm = TRUE))/sd(afdrt, na.rm = TRUE),
         tc = (tutra2 - mean(tutra2))/sd(tutra2))

lmem %>%
  filter(!afdrt == "NA") %>%
  ungroup() %>%
  select(tutra2, scrt, afdrt, r) %>%
  cor()

all <- lmer(tc ~ sc + fc + rc + (sc + fc + rc|sub), lmem)
summary(all)
  
lmemr <- ed4r %>%
  ungroup() %>%
  filter(r < 25, scrt > 0) %>%
  select(sub, tnum, r, scrt, afdrt, tutra2, rtype) %>%
  unique() %>%
  group_by(sub) %>%
  mutate(rc = (r - mean(r))/sd(r),
         sc = (scrt - mean(scrt))/sd(scrt),
         fc = (afdrt - mean(afdrt, na.rm = TRUE))/sd(afdrt, na.rm = TRUE),
         tc = (tutra2 - mean(tutra2))/sd(tutra2))

allr <- lmer(tc ~ sc*rtype + fc*rtype + rc*rtype + (sc*rtype + fc*rtype + rc*rtype|sub), lmemr)
summary(allr)

anova(rc, scrc)

cc <- ed4 %>%
  select(sub, tcat, hirate, crrate, farate, mirate) %>%
  unique()

ggplot(cc, aes(tcat, crrate))+
  geom_boxplot(colour = "dimgrey")+
  labs(list(x = "Category",
            y = "Correct Rejection Rate"))+
  theme(axis.title.x      = element_text(vjust = -0.2),
        axis.title.y      = element_text(vjust = 1.2),
        legend.text       = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 22),
        panel.background  = element_rect(fill = "white"),
        panel.grid.major  = element_line(colour = "white"),
        panel.grid.minor  = element_line(colour = "white"),
        text              = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 22))

ggplot(cc, aes(tcat, hirate))+
  geom_boxplot(colour = "dimgrey")+
  labs(list(x = "Category",
            y = "Hit Rate"))+
  theme(axis.title.x      = element_text(vjust = -0.2),
        axis.title.y      = element_text(vjust = 1.2),
        legend.text       = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 22),
        panel.background  = element_rect(fill = "white"),
        panel.grid.major  = element_line(colour = "white"),
        panel.grid.minor  = element_line(colour = "white"),
        text              = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 22))
