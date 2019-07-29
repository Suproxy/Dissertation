rm(list=ls())

library(texreg)
library(lme4)
library(readxl)

NIC <- read_excel("NIC1.xlsx")
View(NIC)
summary(NIC)


# Multi-level Model Without Population -----------------------------------
# Total Trade with Iran
modelB.lmertt= lmer(LN_TRD ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (1 + SANM + SANU|COUNTRY), data = NIC)


# Export to Iran
modelB.lmerex= lmer(LN_EX ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (1 + SANM + SANU|COUNTRY), data = NIC)


# Import from Iran
modelB.lmerim= lmer(LN_IM ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (1 + SANM + SANU|COUNTRY), data = NIC)


screenreg(list(modelB.lmertt,modelB.lmerex,modelB.lmerim),
          custom.model.names = c("Model TRD", "Model EX", "Model IM"))

htmlreg(list(modelB.lmertt, modelB.lmerex, modelB.lmerim),
        custom.model.names = c("Model TRD", "Model EX", "Model IM"),
        file = "Multi-level Model EU (non_POP).doc")


summary(modelB.lmertt)
summary(modelB.lmerex)
summary(modelB.lmerim)

# Multi-level Model With Population -----------------------------------
# Total Trade
modelA.lmertt= lmer(LN_TRD ~ LN_M_GDP + LN_M_POP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (1 + SANM + SANU|COUNTRY), data = NIC)


# Export to Iran
modelA.lmerex= lmer(LN_EX ~ LN_M_GDP + LN_M_POP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (1 + SANM + SANU|COUNTRY), data = NIC)


# Import from Iran
modelA.lmerim= lmer(LN_IM ~ LN_M_GDP  + LN_M_POP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (1 + SANM + SANU|COUNTRY), data = NIC)


screenreg(list(modelA.lmertt,modelA.lmerex,modelA.lmerim),
          custom.model.names = c("Model TRD", "Model EX", "Model IM"))

htmlreg(list(modelA.lmertt, modelA.lmerex, modelA.lmerim),
        custom.model.names = c("Model TRD", "Model EX", "Model IM"),
        file = "Multi-level Model NIC (POP).doc")




# Detect Multicollinearity------------------------------
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

write.table(VIF_RESULT_A, file = "NIC_VIF_TEST_A.csv", row.names = TRUE, sep = ",")
write.table(VIF_RESULT_B, file = "NIC_VIF_TEST_B.csv", row.names = TRUE, sep = ",")

colMeans(VIF_RESULT_A)
colMeans(VIF_RESULT_B)



# Total Trade with Iran (Test Model)
modelB.lmertt.test = lmer(LN_TRD ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (0 + SANM + SANU|COUNTRY), data = NIC)


# Export to Iran (Test Model)
modelB.lmerex.test = lmer(LN_EX ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (0 + SANM + SANU|COUNTRY), data = NIC)


# Import from Iran (Test Model)
modelB.lmerim.test = lmer(LN_IM ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                    + (0 + SANM + SANU|COUNTRY), data = NIC)

anova(modelB.lmertt, modelB.lmertt.test)
anova(modelB.lmerex, modelB.lmerex.test)
anova(modelB.lmerim, modelB.lmerim.test)

# Test the random effects - Variance: intercept --------------------------------
# Total Trade with Iran (Test Model)
modelB.lmertt.test = lmer(LN_TRD ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                          + (0 + SANM + SANU|COUNTRY), data = NIC)


# Export to Iran (Test Model)
modelB.lmerex.test = lmer(LN_EX ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                          + (0 + SANM + SANU|COUNTRY), data = NIC)


# Import from Iran (Test Model)
modelB.lmerim.test = lmer(LN_IM ~ LN_M_GDP + LN_DIST + SANU + SANM + SANU:UTIME + SANM:MTIME
                          + (0 + SANM + SANU|COUNTRY), data = NIC)

anova(modelB.lmertt, modelB.lmertt.test, reduce.term = TRUE)
anova(modelB.lmerex, modelB.lmerex.test, reudce.term = TRUE)
anova(modelB.lmerim, modelB.lmerim.test, reduce.term = TRUE)


# Test the random effects - Variance: COUNTRY:SANU, COUNTRY:SANM ----------------
summary(modelB.lmertt)
summary(modelB.lmerex)
summary(modelB.lmerim)


library(lmerTest)
ranova(modelB.lmertt, reduce.terms = TRUE)
ranova(modelB.lmerex, reduce.terms = TRUE)
ranova(modelB.lmerim, reduce.terms = TRUE)










# BOX Plot Regression EX (SANM)
NIC$SANM = factor(NIC$SANM, labels = c("Multilateral Sanctions Regimes", "No Multilateral Sanctions Regimes"))

boxplot(LN_EX ~ SANM, data = NIC,
        las = 1,
        frame.plot = FALSE,
        xlab = "",
        ylab = "The Natural Logarithm of NIC's Export to Iran",
        ylim = c(ylim=5, ylmax=25),
        col = "lightgrey")
