
# Comments for Documentation
#1. Note that order of variables in the model determines significance.
#2. SPSS does not produce two-way post hoc comparisons.  That is, the outputs in SPSS test differences between the observed mean scores, and not
# between the actual effects. But, their standard errors look right, so perhaps can just substitute and do as an option.
# Robust stanard errors are possible: glht(vcov = sandwich)
#Note that the order determines significance.

# data(colas)
# names(colas)
#
# library(multcomp)
# lmod <- aov(unclass(q1a) ~ d1 + d2, data = colas)
# lhypotheses <- glht(lmod,  linfct = mcp(d2 = "Tukey")) # Tukey refers to the contrasts, not the correction.
# summary(lhypotheses)
# summary(lhypotheses, test = adjusted("bonferroni"))
# summary(lhypotheses, test = univariate()) #Uncorrected (i.e., Fished LSD)



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
