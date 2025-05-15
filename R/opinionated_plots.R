library(tidyverse)
library(hrbrthemes)
library(janitor)

x <- read_csv("data/fall_membership_statistics.csv") |>
  clean_names()

#clean up school year
x2 <- x |>
  mutate(fall_sch_year = as.numeric(str_sub(school_year, 1, 4)))

#get 10 most populous in 2019
baseline <- x2 |>
  filter(fall_sch_year == 2019) |>
  slice_max(total_count, n = 10, with_ties = FALSE) |>
  select(division_name, n_2019 = total_count)

top10_df <- x2 |>
  filter(division_name %in% unique(baseline$division_name)) |>
  left_join(baseline) |>
  mutate(change = (total_count - n_2019) / n_2019)

#basic plot
base_plot <- top10_df |>
  ggplot(aes(x = fall_sch_year, y = change, color = division_name)) +
  geom_line() +
  scale_color_discrete(name = "Division") +
  scale_y_continuous(label = scales::percent_format()) +
  labs(
    x = "School Year (Fall)",
    y = "% Change in Fall Enrollment Since 2019",
    title = "Change in Enrollment of Virginia's 10 Largest School Divisions",
    subtitle = "Since SY2019"
  ) +
  theme_ipsum_rc() +
  theme(
    panel.grid.minor = element_blank()
  )

#with opinionated title
op_title_plot <- top10_df |>
  ggplot(aes(x = fall_sch_year, y = change, color = division_name)) +
  geom_line() +
  scale_color_discrete(name = "Division") +
  scale_y_continuous(label = scales::percent_format()) +
  labs(
    x = "School Year (Fall)",
    y = "% Change in Fall Enrollment Since 2019",
    title = "Chesterfield and Stafford Grow while Other Large Divisions' Enrollment\nDeclines",
    subtitle = "Since SY2019, Chesterfield and Stafford are the only of Virginia's 10 largest school divisions that have increased\nstudent enrollment"
  ) +
  theme_ipsum_rc() +
  theme(
    panel.grid.minor = element_blank()
  )

#color refinements
fewer_colors_plot <- top10_df |>
  mutate(div_name = if_else(str_detect(division_name, "Chester|Stafford"), division_name, "Other")) |>
  ggplot(aes(x = fall_sch_year, y = change, group = division_name, color = div_name)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(
    name = "Division",
    values = c("#006666", "#b7b7b7", "#ffcc66")
  ) +
  scale_y_continuous(label = scales::percent_format()) +
  labs(
    x = "School Year (Fall)",
    y = "% Change in Fall Enrollment Since 2019",
    title = "Chesterfield and Stafford Grow while Other Large Divisions' Enrollment\nDeclines",
    subtitle = "Since SY2019, Chesterfield and Stafford are the only of Virginia's 10 largest school divisions that have increased\nstudent enrollment"
  ) +
  theme_ipsum_rc() +
  theme(
    panel.grid.minor = element_blank()
  )

#save outputs
ps <- list(base_plot, op_title_plot, fewer_colors_plot)
nms <- c("base_plot", "op_title_plot", "fewer_colors_plot")

outpaths <- paste0("plots/opinionated_plots/", nms, ".png")

walk2(
    outpaths,
    ps,
    ggsave
)
