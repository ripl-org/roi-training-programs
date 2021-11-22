# Load required packages -----------------
require(here)
require(tidyverse)
require(haven)
require(sjPlot)
require(ggthemes)
require(compareGroups)

tidy.or.se <- function(model) {
    broom::tidy(model) %>%
        mutate(
            or = exp(estimate),
            var.diag = diag(vcov(model)),
            or.se = sqrt(or^2 * var.diag),
            or.lower = or - (qnorm(1 - .05 / 2) * or.se),
            or.upper = or + (qnorm(1 - .05 / 2) * or.se)
        )
}

## Set ggplot2 plotting theme (optional)
#theme_set(hrbrthemes::theme_ipsum())
# theme_set(ggplot2::theme_minimal())
scale_color_discrete <- scale_color_colorblind
scale_fill_discrete <- scale_fill_colorblind

## Set ggplot2 set continuous color scheme (optional)
options(ggplot2.continuous.color = "viridis")
options(ggplot2.continuous.fill = "viridis")

conjoint_df <- read_dta(here("data", "conjoint_tables_with_demog.dta")) %>%
    mutate(
        cj = as_factor(cj),
        prof = as_factor(prof),
        rank = as_factor(rank)
    )

conjoint_df_outside <- conjoint_df

conjoint_df <- filter(conjoint_df, prof != "c0")

cjtab <- descrTable(cj ~ cost_int + duration_int + traveltime_int +
    completionrate_int + earn_posttrain_prop_int +
    earn_notrain_prop_int + earn_increase_int + unemployment_int,
data = conjoint_df,
max.ylev = 10, show.p.overall = FALSE
)

cjtab

export2word(cjtab, here("output", "attributes_by_conjoint.doc"))

descrTable(prof ~ topchoice, conjoint_df)


ggplot(conjoint_df, aes(x = as.factor(cost_int), fill = cj)) +
    geom_bar(position = "dodge", aes(y = ..count.., group = cj)) +
    scale_fill_colorblind(guide_legend(nrow = 1)) +
    scale_y_continuous(limit = c(0, 3000), breaks = seq(0, 3000, by = 1500)) +
    facet_wrap(vars(cj), nrow = 3) +
    labs(title = "Cost", x = "Cost (USD)") +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = -0.1),
        axis.text.y = element_text(size = 10)
    )

ggsave(here("output", "conjoint_cost_distribution.png"), device = "png")


ggplot(conjoint_df, aes(x = as.factor(duration_int), fill = cj)) +
    geom_bar(position = "dodge", aes(y = ..count.., group = cj)) +
    scale_fill_colorblind(guide_legend(nrow = 1)) +
    scale_y_continuous(limit = c(0, 6000), breaks = seq(0, 6000, by = 3000)) +
    facet_wrap(vars(cj), nrow = 3) +
    labs(title = "Duration of Training Program", x = "Duration (weeks)") +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = -0.1)
    )

ggsave(here("output", "conjoint_duration_distribution.png"), device = "png")


ggplot(conjoint_df, aes(x = as.factor(traveltime_int), fill = cj)) +
    geom_bar(position = "dodge", aes(y = ..count.., group = cj)) +
    scale_fill_colorblind(guide_legend(nrow = 1)) +
    scale_y_continuous(limit = c(0, 6000), breaks = seq(0, 6000, by = 3000)) +
    facet_wrap(vars(cj), nrow = 3) +
    labs(title = "Travel Time", x = "One-way Travel Time (minutes)") +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = -0.05)
    )

ggsave(here("output", "conjoint_traveltime_distribution.png"), device = "png")


ggplot(conjoint_df, aes(x = as.factor(completionrate_int), fill = cj)) +
    geom_bar(position = "dodge", aes(y = ..count.., group = cj)) +
    scale_fill_colorblind(guide_legend(nrow = 1)) +
    scale_y_continuous(breaks = seq(0, 18000, by = 6000)) +
    facet_wrap(vars(cj), nrow = 3) +
    labs(title = "Completion Rate", x = "Completion Rate (%)") +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = -0.1)
    )


ggsave(here("output", "conjoint_completionrate_distribution.png"), device = "png")


conjoint_df %>%
    filter(!is.na(earn_posttrain_prop_int)) %>%
    ggplot(aes(x = earn_posttrain_prop_int, fill = cj)) +
    geom_histogram() +
    # scale_x_log10() +
    scale_fill_colorblind(guide_legend(nrow = 1)) +
    facet_grid(cols = vars(cj), rows = vars(earn_ref)) +
    labs(
        title = "Post-Training Earnings",
        subtitle = "by Conjoint Table and Reference Period"
    ) +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = -0.1)
    )

ggsave(here("output", "conjoint_earn_posttrain_distribution.png"), device = "png")


ggplot(conjoint_df, aes(x = as.factor(earn_increase_int), fill = cj)) +
    geom_bar(position = "dodge", aes(y = ..count.., group = cj)) +
    scale_fill_colorblind(guide_legend(nrow = 1)) +
    scale_y_continuous(breaks = seq(0, 18000, by = 6000)) +
    facet_wrap(vars(cj), nrow = 3) +
    labs(title = "Increase in Earnings", x = "Typical Earnings Increase (%)") +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = -0.1)
    )

ggsave(here("output", "conjoint_earn_increase_distribution.png"), device = "png")