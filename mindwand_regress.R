# Mindwandering Regression
library(dplyr)
library(ggplot2)
library(zoo)

# Load Data ---------------------------------------------------------------

# Trial Report
trep <- read.delim("trial_report_32015.txt", na.strings = c('.', 'NA', ''),
                   stringsAsFactors = FALSE) %>%
          select(sub, tnum, tuttime, tar, hunger, resp, rt, tcat, rtype, tired,
                 bnum, time, tutra, mfixdur = AVERAGE_FIXATION_DURATION,
                 mblinkdur = AVERAGE_BLINK_DURATION,
                 msamp = AVERAGE_SACCADE_AMPLITUDE, blinkc = BLINK_COUNT,
                 fixc = FIXATION_COUNT, maxfixdur = FIXATION_DURATION_MAX,
                 index = INDEX, pupszmax = PUPIL_SIZE_MAX,
                 mpupsz = PUPIL_SIZE_MEAN, saccount = SACCADE_COUNT,
                 vIAc = VISITED_INTEREST_AREA_COUNT)

# Interest Area report
iarep <- read.delim("IA_report_4282015.txt", na.strings = c('.', 'NA', ''),
                    stringsAsFactors = FALSE) %>%
          select(sub, tnum, tindex = TRIAL_INDEX, tutra,
                 iampupsz = IA_AVERAGE_FIX_PUPIL_SIZE,
                 iadwell = IA_DWELL_TIME_per,
                 firstrundwell = IA_FIRST_RUN_DWELL_TIME,
                 iafix = IA_FIXATION_per, runc = IA_RUN_COUNT,
                 totiavisit = TRIAL_TOTAL_VISITED_IA_COUNT)

# Combine data
raw <- left_join(trep, iarep)

# Clean data and fill in tuts, add measures
badsubs <- c('1_zw', '45_sjk')
cdat <- raw %>%
          filter(!sub %in% badsubs, !is.na(tnum)) %>%  # Take out fixcheck/pupil
          group_by(sub) %>%
          mutate(
            tutra = na.locf(tutra, fromLast = TRUE, na.rm = FALSE),
            rtype = ifelse(rtype == "correj", 'cr', rtype),
            rtype = ifelse(rtype == "falsealarm", 'fa', rtype),
            rtype = ifelse(rtype == "hit", 'hi', rtype),
            rtype = ifelse(rtype == "miss", 'mi', rtype)
          ) %>%
          filter(!is.na(tutra)) %>%  # Remove trials without tutra rating
          group_by(sub, tnum) %>%
          mutate(
            regs = sum(runc) - totiavisit
          )%>%
          filter(regs < 25, rt > 0)

# Data Checking -----------------------------------------------------------

submns <- cdat %>%
            group_by(sub, tnum) %>%
            slice(1) %>%
            group_by(sub) %>%
            summarise(
              msc = mean(saccount)
            )

ggplot(submns, aes(msc))+
  geom_histogram()

# Within Subject Analysis -------------------------------------------------
library(lme4)
library(lmerTest)

# Make data
wdat <- cdat %>%
          group_by(sub, tnum) %>%
          filter(sub != '31_lc') %>%
          slice(1) %>%  # Select first row of each trial
          mutate(
            scrt = saccount / (rt / 1000),
            fcrt = fixc / (rt / 1000)
          ) %>%
          group_by(sub) %>%
          mutate(
            fix_cen = (fcrt - mean(fcrt)) / sd(fcrt),
            sac_cen = (scrt - mean(scrt)) / sd(fcrt),
            tutra_cen = (tutra - mean(tutra)) / sd(tutra)
          )

# Run models
mod1 <- lmer(tutra_cen ~ sac_cen + regs + (sac_cen + regs|sub), wdat)
summary(mod1)

# Plots
ggplot(wdat, aes(tutra_cen, msamp))+
  geom_point()+
  geom_smooth(method = lm)+
  facet_wrap(~sub)

# Between Subjects Analysis -----------------------------------------------

# Make Data
bdat <- cdat %>%
          group_by(sub, tnum) %>%
          slice(1) %>%
          mutate(
            scrt = saccount / rt,
            fcrt = fixc / rt
          ) %>%
          group_by(sub) %>%  # Choose one of these, if looking at rtype or not
          #group_by(sub, rtype) %>%
          summarise(
            mtut = mean(tutra),
            mfix = mean(fcrt) * 1000,
            msac = mean(scrt) * 1000,
            mreg = mean(regs),
            mrt  = mean(rt) / 1000,
            hun  = mean(hunger),
            tire = mean(tired)
          ) %>%
          ungroup() %>%
          mutate(
            msac_cen = (msac - mean(msac)),
            mfix_cen = (mfix - mean(mfix)),
            mrt_cen = (mrt - mean(mrt))
          )

# RT Model
rtmod <- lm(mtut ~ mrt_cen * rtype, bdat)

ggplot(bdat, aes(mtut, mrt, color = rtype))+
  geom_point()+
  geom_smooth(method = lm)+
  facet_wrap(~rtype)

# Eye metrics
bmod <- lm(mtut ~ msac_cen + mreg + tire, bdat)
  summary(bmod)

# Plots
ggplot(bdat, aes(mreg, mtut, color = rtype))+
  geom_point()+
  geom_smooth(aes(color = rtype), method = lm, se = T)

ggplot(bdat, aes(msac, mfix))+
  geom_point()+
  geom_smooth(method = lm)

