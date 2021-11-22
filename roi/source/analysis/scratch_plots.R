# Packages ----------------------------------------------------------------

library(dplyr)
library(fst)
library(ggplot2)
library(readr)
library(tidyr)

# Load Data ---------------------------------------------------------------

wioa <- read_fst(here::here("scratch/derived/wioa-wp-analysis-panel.fst")) %>%
  as_tibble() %>%
  select(sirad_id, treatment, contains("wages")) %>%
  rename(`-4`=paste0("wages", "_prev4"),
         `-3`=paste0("wages", "_prev3"),
         `-2`=paste0("wages", "_prev2"),
         `-1`=paste0("wages", "_prev1"),
         `0`="wages",
         `1`=paste0("wages", "_next1"),
         `2`=paste0("wages", "_next2"),
         `3`=paste0("wages", "_next3"),
         `4`=paste0("wages", "_next4")) %>%
  pivot_longer(cols=`-4`:`4`,
               names_to="relative_yyq",
               values_to="outcome") %>%
  mutate(outcome = replace_na(outcome, 0)) %>%
  group_by(treatment, relative_yyq) %>%
  summarise(outcome = mean(outcome)) %>%
  ungroup() %>%
  mutate(relative_yyq = as.numeric(relative_yyq))

matching_rjri <- read_csv(here::here("scratch/analysis/matching/plot-match_rjri.csv"))

matching_wioa <- read_csv(here::here("scratch/analysis/matching/plot-match_wioa.csv"))

# Plots -------------------------------------------------------------------

matching %>%
  filter(program %in% c("CNA Training",
                        "Industry-Driven Incumbent Worker Training",
                        "New Hire Training in Maritime Trades",
                        "Northern RI Career Academy",
                        "RJRI")) %>%
  rename("Treatment" = "treatment",
         "Program" = "program") %>%
  ggplot(aes(x = relative_yyq,
             y = mean_outcome,
             linetype = Treatment,
             color = Program)) +
  annotate("text",
           x = Inf,
           y = Inf,
           hjust = 1,
           vjust = 1,
           label = "Matched on prior wages within $500 bandwidth") +
  geom_line() +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  scale_x_continuous(breaks = seq(-4, 4, by = 1)) +
  labs(x = "Relative YYQ",
       y = "Mean Quarterly Wages") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold",
                                  hjust = 0.5))

wioa %>%
  rename("Treatment" = "treatment") %>%
  ggplot(aes(x = relative_yyq,
             y = outcome,
             linetype = Treatment)) +
  geom_line() +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  scale_x_continuous(breaks = seq(-4, 4, by = 1)) +
  labs(x = "Relative YYQ",
       y = "Mean Quarterly Wages") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold",
                                  hjust = 0.5)) +
  scale_y_continuous(limits = c(1000, 9000))

matching_wioa %>%
  rename("Treatment" = "treatment",
         "Program" = "program") %>%
  ggplot(aes(x = relative_yyq,
             y = mean_outcome,
             linetype = Treatment)) +
  annotate("text",
           x = Inf,
           y = Inf,
           hjust = 1,
           vjust = 1,
           label = "Matched on prior wages within $1500 bandwidth") +
  geom_line() +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  scale_x_continuous(breaks = seq(-4, 4, by = 1)) +
  labs(x = "Relative YYQ",
       y = "Mean Quarterly Wages") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold",
                                  hjust = 0.5)) +
  scale_y_continuous(limits = c(1000, 9000))