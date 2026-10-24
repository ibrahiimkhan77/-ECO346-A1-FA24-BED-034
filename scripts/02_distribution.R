source("scripts/00_prepare.R")
dir.create("figures", showWarnings = FALSE)

students <- students %>% mutate(log_spend = log10(ed_total))

# Statistics on all students (ed_total > 0)
n_students <- nrow(students)
w_median <- weighted_quantile(students$ed_total, students$hhweight, 0.5)
u_median <- median(students$ed_total)
p10 <- weighted_quantile(students$ed_total, students$hhweight, 0.1)
p90 <- weighted_quantile(students$ed_total, students$hhweight, 0.9)
ratio <- p90 / p10

# Histogram sample: students between Rs 100 and Rs 1,000,000
in_range <- students %>% filter(log_spend >= 2, log_spend <= 6)
n_excluded <- n_students - nrow(in_range)

breaks <- 2 + (0:40) / 10
hist_df <- in_range %>%
  mutate(bin = cut(log_spend, breaks = breaks, right = FALSE, include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(w = sum(hhweight), .groups = "drop") %>%
  mutate(
    left = breaks[as.integer(bin)],
    right = left + 0.1,
    density = w / sum(w) / 0.1
  )

rs <- function(x) paste0("Rs ", format(round(x), big.mark = ","))

p <- ggplot(hist_df, aes(xmin = left, xmax = right, ymin = 0, ymax = density)) +
  geom_rect(fill = "#4a7ba6", colour = "white", linewidth = 0.2) +
  geom_vline(xintercept = log10(c(w_median, p90)), linetype = "dashed") +
  annotate("text", x = log10(w_median), y = max(hist_df$density) * 1.08,
           label = paste0("Median\n", rs(w_median)), hjust = 1.1) +
  annotate("text", x = log10(p90), y = max(hist_df$density) * 1.08,
           label = paste0("90th percentile\n", rs(p90)), hjust = -0.1) +
  scale_x_continuous(
    limits = c(2, 6),
    breaks = 2:6,
    labels = c("Rs 100", "Rs 1,000", "Rs 10,000", "Rs 100,000", "Rs 1,000,000")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = paste0("Rural Sindh: half of students cost households ", rs(w_median),
                   " or less a year, but the top tenth spend over ", rs(p90)),
    x = "Annual education spending per student (log scale)",
    y = "Density per log10 unit",
    caption = stringr::str_wrap(
      paste0("Source: PSLM 2019-20 person file. Sample: students (ed_total > 0) in Sindh rural, n = ",
             n_students, ". Weighted by hhweight. ",
             n_excluded, if (n_excluded == 1) " student" else " students",
             " outside Rs 100 to Rs 1,000,000 excluded."),
      width = 110)
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 11),
        plot.title.position = "plot", plot.caption.position = "plot",
        panel.grid.minor = element_blank())

ggsave("figures/distribution.svg", p, width = 9, height = 5.5)

cat("Students:", n_students, "\n")
cat("Weighted median:", w_median, " Unweighted median:", u_median, "\n")
cat("p10:", p10, " p90:", p90, " ratio:", round(ratio, 2), "\n")
cat("Excluded:", n_excluded, "\n")