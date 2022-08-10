################################################################################
################################### Figure 3B ###################################
################################################################################
## Library R packages.
library(survival)
library(survminer)

# Cox regression analysis in N-TX dataset.
res.cox <-
  coxph(Surv(os, state1) ~ age + sex + type, data = xiannumber2_t)

# Stepwise regression method.
tstep   <- step(res.cox, direction = "both", data = xiannumber2_t)
tstep

res.cox <- coxph(Surv(os, state1) ~ age + sex, data = xiannumber2_t)
fit     <- survfit(res.cox)
fit
#         n events median 0.95LCL 0.95UCL
# [1,] 1261   1261    108     102     112

# Results with only significant variables.
res.cox <- coxph(Surv(os, state1) ~ age,       data = xiannumber2_t)
fit     <- survfit(res.cox)
fit
#         n events median 0.95LCL 0.95UCL
# [1,] 1261   1261    105     100     111


# Calculate the value of the drop to 5% and its 95% confidence interval.
median505 <-
  cbind(fit[["time"]], fit[["surv"]], fit[["lower"]], fit[["upper"]])
View(median505)

# Save data for curves.
write.csv(cbind(fit[["time"]], fit[["surv"]]), file = "~/Supporting Documents/xian_fit.csv")



# Plot Fig3B.
Fig3B <- list()
Fig3B[[1]] <-
  ggsurvplot(
    fit,
    data = xiannumber2_t,
    xlim = c(0, 200),
    xlab = "",
    ylab = "",
    main = "",
    break.time.by = 50,
    ggtheme = theme(
      axis.line.x = element_line(color = "black", size = 0.5),
      axis.line.y = element_line(color = "black", size = 0.5),
      axis.text = element_text(colour = "black", size = rel(0.8)),
      axis.ticks = element_line(colour = "black", size = 0.5),
      panel.grid = element_line(
        color = "lightgray",
        size = 0.25,
        linetype = 1
      ),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white")
    ),
    conf.int.fill = "#F79DBD",
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
    palette = c("#97234F")
  )

# Arrange and save into pdf file
res <- arrange_ggsurvplots(
  Fig3B,
  print = TRUE,
  title = "",
  ncol = 1,
  nrow = 1,
  risk.table.height = 0.28
)

ggsave(
  "~/Supporting Documents/Figure/Figure3B.pdf",
  res,
  width = 200 / 2 ,
  height = 100 ,
  units = "mm"
)
