# U.S.-Iran-Sacntions

rm(list=ls())

library(texreg)
library(lme4)
library(readxl)

EU <- read_excel("EU1.xlsx")
View(EU)


# Multi-level Model Without Population --------------------
# Total Trade with Iran
modelB.lmertt= lmer(LN_TRD ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (1 + SANU + SANM|COUNTRY), data = EU)


# Export to Iran
modelB.lmerex= lmer(LN_EX ~ LN_M_GDP + LN_DIST+ SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (1 + SANU + SANM|COUNTRY),data = EU)


# Import from Iran
modelB.lmerim= lmer(LN_IM ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (1 + SANU + SANM|COUNTRY),data = EU)


screenreg(list(modelB.lmertt,modelB.lmerex,modelB.lmerim),
          custom.model.names = c("Model TRD", "Model EX", "Model IM"))

htmlreg(list(modelB.lmertt, modelB.lmerex, modelB.lmerim),
        custom.model.names = c("Model TRD", "Model EX", "Model IM"),
        file = "Multi-level Model EU (non_POP).doc")





# Multi-level Model With Population -----------------------
# Total Trade
modelA.lmertt= lmer(LN_TRD ~ LN_M_GDP + LN_M_POP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (1 + SANU + SANM|COUNTRY),data = EU)


# Export to Iran
modelA.lmerex= lmer(LN_EX ~ LN_M_GDP + LN_M_POP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (1 + SANU + SANM|COUNTRY),data = EU)


# Import from Iran
modelA.lmerim= lmer(LN_IM ~ LN_M_GDP + LN_M_POP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (1 + SANU + SANM|COUNTRY),data = EU)


screenreg(list(modelA.lmertt,modelA.lmerex,modelA.lmerim),
          custom.model.names = c("Model TRD", "Model EX", "Model IM"))

htmlreg(list(modelA.lmertt, modelA.lmerex, modelA.lmerim),
        custom.model.names = c("Model TRD", "Model EX", "Model IM"),
        file = "Multi-level Model EU (POP).doc")




# Detect Multicollinearity-----------------------------------
library(car)

VIFTT_A = vif(modelA.lmertt)
VIFEX_A = vif(modelA.lmerex)
VIFIM_A = vif(modelA.lmerim)

VIFTT_B = vif(modelB.lmertt)
VIFEX_B = vif(modelB.lmerex)
VIFIM_B = vif(modelB.lmerim)


VIF_RESULT_A = data.frame(VIFTT_A, VIFEX_A, VIFIM_A)
VIF_RESULT_B = data.frame(VIFTT_B, VIFEX_B, VIFIM_B)

View(VIF_RESULT_A)
View(VIF_RESULT_B)

colMeans(VIF_RESULT_A)
colMeans(VIF_RESULT_B)


write.table(VIF_RESULT_A, file="EU_VIF_TEST_A.csv", row.names = TRUE, sep=".")
write.table(VIF_RESULT_B, file="EU_VIF_TEST_B.csv", row.names = FALSE, sep=".")




# Test the random effects - Variance: intercept ----------------
# Total Trade with Iran (Test Model)
modelB.lmertt.test = lmer(LN_TRD ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                          + (0 + SANM + SANU|COUNTRY), data = EU)


# Export to Iran (Test Model)
modelB.lmerex.test = lmer(LN_EX ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                          + (0 + SANM + SANU|COUNTRY), data = EU)


# Import from Iran (Test Model)
modelB.lmerim.test = lmer(LN_IM ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                          + (0 + SANM + SANU|COUNTRY), data = EU)

anova(modelB.lmertt, modelB.lmertt.test, reduce.term = TRUE)
anova(modelB.lmerex, modelB.lmerex.test, reudce.term = TRUE)
anova(modelB.lmerim, modelB.lmerim.test, reduce.term = TRUE)




# Test the random effects - Variacne: COUNTRY:SANU, COUNTRY:SANM ---------
summary(modelB.lmertt)
summary(modelB.lmerex)
summary(modelB.lmerim)


library(lmerTest)
ranova(modelB.lmertt, reduce.terms = TRUE)
ranova(modelB.lmerex, reduce.terms = TRUE)
ranova(modelB.lmerim, reduce.terms = TRUE)
