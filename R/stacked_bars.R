library(tidyverse)
library(blueycolors)

theme_set(theme_minimal(base_size = 12) + theme(plot.background = element_rect(fill = "white")))

socks1 <- bluey_palette("socks")[1]

set.seed(0409)

schs <- c("School A", "School B", "School C")
race <- c("White", "Black", "Hispanic", "Asian")

x <- expand.grid(schs, race)
names(x) <- c("school", "race")
x$enroll <- round(runif(nrow(x), min = 0, max = 60), digits = 0)

# standard stacked bar
p1 <- x |>
    ggplot(aes(x = school, y = enroll, fill = race)) +
    geom_col() +
    labs(
        y = "Enrollment"
    )

# in my mind, there are 2 ways to improve this
# first, make 2 plots

# this lets you compare the overall enrollment across schools
p2a <- x |>
    group_by(school) |>
    summarize(total = sum(enroll)) |>
    ggplot(aes(x = school, y = total)) +
    geom_col(fill = socks1) +
    labs(
        y = "Enrollment"
    )

# this lets you compare the enrollment by race, both across schools and within schools
p2b <- x |>
    ggplot(aes(x = school, y = enroll, fill = race)) +
    geom_col(position = "dodge") +
    labs(
        y = "Enrollment"
    )

# i'd probably lean toward portraying this in 2 plots, but if it's absolutely necessary to do it in a single plot, here would be my approach
x_sum <- x |>
    group_by(school) |>
    summarize(total = sum(enroll))

# you can create a 'shadow' that shows the overall amounts while still displaying the enrollment by race using colors
p3 <- x_sum |>
    ggplot(aes(x = school, y = total)) +
    geom_col(fill = "grey80", alpha = .5) +
    geom_col(data = x, aes(x = school, y = enroll, fill = race), position = "dodge") +
    labs(
        y = "Enrollment"
    )

ps <- list(p1, p2a, p2b, p3)
names(ps) <- c("p1", "p2a", "p2b", "p3")

walk2(
    ps,
    names(ps),
    ~ ggsave(paste0("plots/stacked_bars/", .y, ".png"), .x)
)
