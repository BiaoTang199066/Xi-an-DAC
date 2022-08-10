################################################################################
################################### Figure 3A ###################################
################################################################################
## Library R packages.
library(survival)
library(survminer)

## Cox regression analysis in Nanjing dataset.
res.cox <-
  coxph(Surv(os, state1) ~ age + sex + type, data = nanjingnumber2)

# Stepwise regression method.
tstep   <- step(res.cox, direction = "both", data = nanjingnumber2)
tstep

res.cox <-
  coxph(Surv(os, state1) ~ age + sex + type, data = nanjingnumber2)
fit     <- survfit(res.cox)
fit
#        n events median 0.95LCL 0.95UCL
# [1,] 109    109     88      60     102

# Results with only significant variables.
res.cox <-
  coxph(Surv(os, state1) ~ age + type,       data = nanjingnumber2)
fit     <- survfit(res.cox)
fit
#        n events median 0.95LCL 0.95UCL
# [1,] 109    109     96      70     102

# Calculate the value of the drop to 5% and its 95% confidence interval.
median505 <-
  cbind(fit[["time"]], fit[["surv"]], fit[["lower"]], fit[["upper"]])
View(median505)

# Save data for curves.
write.csv(cbind(fit[["time"]], fit[["surv"]]), file = "~/Supporting Documents/nanjing_fit.csv")


# Plot Fig3A.
Fig3A <- list()
Fig3A[[1]] <- ggsurvplot(
  fit,
  data = nanjingnumber2,
  xlim = c(0, 200),
  xlab = "",
  ylab = "",
  main = "",
  break.time.by = 50,
  ggtheme = theme(
    axis.line.x = element_line(color  = "black", size = 0.5),
    axis.line.y = element_line(color  = "black", size = 0.5),
    axis.text   = element_text(colour = "black", size = rel(0.8)),
    axis.ticks  = element_line(colour = "black", size = 0.5),
    panel.grid  = element_line(
      color  = "lightgray",
      size = 0.25,
      linetype = 1
    ),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  ),
  conf.int.fill = "#7DA9FF",
  # pval = TRUE, pval.size=2,pval.lwd=1,
  conf.int = TRUE,
  risk.table = TRUE,
  # Add risk table
  tables.y.text = FALSE,
  risk.table.title = "",
  risk.table.y.text = FALSE,
  fontsize = 3,
  linetype = "strata",
  # Change line type by groups
  surv.median.line = "hv",
  # Specify median survival
  tables.ggtheme = theme_classic(),
  # Change ggplot2 theme
  palette = c("#0242A8")
)

# Arrange and save into pdf file
res <- arrange_ggsurvplots(
  Fig3A,
  print = TRUE,
  title = "",
  ncol = 1,
  nrow = 1,
  risk.table.height = 0.28
)

ggsave(
  "~/Supporting Documents/Figure/Figure3A.pdf",
  res,
  width = 100 ,
  height = 100 ,
  units = "mm"
)

