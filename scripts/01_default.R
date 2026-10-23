   source("scripts/00_prepare.R")
   dir.create("figures", showWarnings = FALSE)

   p <- ggplot(lit_sample, aes(age, literate)) + geom_point()
   ggsave("figures/default.svg", p)