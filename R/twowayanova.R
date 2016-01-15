

### Note that the order determines significance.

# data(colas)
# names(colas)
#
# lmod <- aov(unclass(q1a) ~ d1 + d2, data = colas)
# lhypotheses <- glht(lmod,  linfct = mcp(d2 = "Tukey")) # Tukey refers to the contrasts, not the correction.
# summary(lhypotheses)
# summary(lhypotheses, test = adjusted("bonferroni"))
# summary(lhypotheses, test = adjusted("fdr"))
#
#
#
# summary(glht(model,  linfct = mcp(year = "Tukey")), test = adjusted("fdr"))
# summary(glht(model,  linfct = mcp(year = "Tukey")), test = adjusted("fdr"))
#
# option(width = 200)
# library(multcomp)
# summary(glht(model1,  linfct = mcp(CarnegieClassification = "Tukey")))
#
# option(width = 200)
# library(multcomp)
# summary(glht(model1,  linfct = mcp(CarnegieClassification = "Tukey")), test = adjusted("fdr"))
#
# options(width = 200)
# library(multcomp)
# summary(glht(model1,  linfct = mcp(SurveyYear = "Tukey")))
#
# options(width = 200)
# library(multcomp)
# summary(glht(model1,  linfct = mcp(SurveyYear = "Tukey")), test = adjusted("fdr"))
