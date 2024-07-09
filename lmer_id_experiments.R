library(RePsychLing)
library(lme4)

singularity_tol <- 1e-4

m0 <- lmer(sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + 
             (1 + T + F + TF | session) + (1 + T + P + TP | item), 
           gb12, REML = FALSE)
m0_pca <- rePCA(m0)
session_rank0 <- sum(m0_pca$session$sdev > singularity_tol)
item_rank0 <- sum(m0_pca$item$sdev > singularity_tol)

# Do ID on both ----------------------------------------------------------

chf0_session <- getME(m0, "Tlist")[[1]]
(xx_session <- tcrossprod(chf0_session))
qr0_session <- qr(xx_session, LAPACK = TRUE)
session_idx <- qr0_session$pivot[1:session_rank0]
session_idx # 1, 3

chf0_item <- getME(m0, "Tlist")[[2]]
(xx_item <- tcrossprod(chf0_item))
qr0_item <- qr(xx_item, LAPACK = TRUE)
item_idx <- qr0_item$pivot[1:item_rank0]
item_idx # 1, 3

m_id_both <- lmer(
  sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + (1 + F | session) + (1 + P | item), 
  gb12, REML = FALSE)
m_id_both_pca <- rePCA(m_id_both)
session_rank_id_both <- sum(m_id_both_pca$session$sdev > singularity_tol)
item_rank_id_both <- sum(m_id_both_pca$item$sdev > singularity_tol)

chf_id_both_item <- getME(m_id_both, "Tlist")[[2]]
(xx_id_both_item <- tcrossprod(chf_id_both_item))
qr_id_both_item <- qr(xx_id_both_item, LAPACK = TRUE)
item_id_both_idx <- qr_id_both_item$pivot[1:item_rank_id_both]
item_id_both_idx # 1

#Final model
m_id_both2 <- lmer(
  sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + (1 + F | session) + (1 | item), 
  gb12, REML = FALSE)

# Do ID on session first ----------------------------------------------------

m_id_session <- lmer(
  sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + (1 + F | session) + (1 + T + P + TP | item), 
  gb12, REML = FALSE)
m_id_session_pca <- rePCA(m_id_session)
item_rank_id_session <- sum(m_id_session_pca$item$sdev > singularity_tol)

chf_id_session_item <- getME(m_id_session, "Tlist")[[2]]
(xx_id_session_item <- tcrossprod(chf_id_session_item))
qr_id_session_item <- qr(xx_id_session_item, LAPACK = TRUE)
item_id_session_idx <- qr_id_session_item$pivot[1:item_rank_id_session]
item_id_session_idx # 1, 3 -- same as before

# Do id on item first ----------------------------------------------------

m_id_item <- lmer(
  sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + (1 + T + F + TF | session) + (1 + P | item), 
  gb12, REML = FALSE)
m_id_item_pca <- rePCA(m_id_item)
session_rank_id_item <- sum(m_id_item_pca$session$sdev > singularity_tol)

chf_id_item_session <- getME(m_id_item, "Tlist")[[1]]
(xx_id_item_session <- tcrossprod(chf_id_item_session))
qr_id_item_session <- qr(xx_id_item_session, LAPACK = TRUE)
session_id_item_idx <- qr_id_item_session$pivot[1:session_rank_id_item]
session_id_item_idx # 1, 3 -- same as before

# Remove small variance components from zcpLMM first ----------------------

m_zcp <- lmer(sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + 
                (1 + T + F + TF || session) + (1 + T + P + TP || item), 
              gb12, REML = FALSE)
summary(m_zcp)

m_zcp2 <- lmer(sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + 
                 (1 + T + F || session) + (1 + T || item), 
               gb12, REML = FALSE)
summary(m_zcp2)

m_cp <- lmer(sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + 
               (1 + T + F | session) + (1 + T | item), 
             gb12, REML = FALSE)
m_cp_pca <- rePCA(m_cp) 
session_rank_cp <- sum(m_cp_pca$session$sdev > singularity_tol)
item_rank_cp <- sum(m_cp_pca$item$sdev > singularity_tol)

chf_cp_session <- getME(m_cp, "Tlist")[[1]]
(xx_cp_session <- tcrossprod(chf_cp_session))
qr_cp_session <- qr(xx_cp_session, LAPACK = TRUE)
session_cp_idx <- qr_cp_session$pivot[1:session_rank_cp]
session_cp_idx # 1, 2, 3?

chf_cp_item <- getME(m_cp, "Tlist")[[2]]
(xx_cp_item <- tcrossprod(chf_cp_item))
qr_cp_item <- qr(xx_cp_item, LAPACK = TRUE)
item_cp_idx <- qr_cp_item$pivot[1:item_rank_cp]
item_cp_idx # 1

m_cp2 <- lmer(sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + 
               (1 + T + F | session) + (1 | item), 
             gb12, REML = FALSE)
summary(rePCA(m_cp2))
