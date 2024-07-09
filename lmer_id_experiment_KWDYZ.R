library(RePsychLing)
library(lme4)

singularity_tol <- lmerControl()$checkConv$check.conv.singular$tol

m0 <- lmer(rt ~ 1 + c1 + c2 + c3 + (1 + c1 + c2 + c3 | subj), KWDYZ, REML = FALSE)

# not converged so update a few times
pars <- getME(m0, "theta")
m0_update <- update(m0, start = pars, REML = FALSE)
pars_update <- getME(m0_update, "theta")
m0_update2 <- update(m0_update, start = pars_update, REML = FALSE)

m0_pca <- rePCA(m0_update2)
rank0 <- sum(m0_pca$subj$sdev > singularity_tol)

# Do ID on both ----------------------------------------------------------

chf0_subj <- getME(m0_update2, "Tlist")[[1]]
(xx_subj <- tcrossprod(chf0_subj))
qr0_subj <- qr(xx_subj, LAPACK = TRUE)
idx <- qr0_subj$pivot[1:rank0]
idx # 1, 2, 3

m_id <- lmer(rt ~ 1 + c1 + c2 + c3 + (1 + c1 + c2 | subj), KWDYZ, REML = FALSE)
