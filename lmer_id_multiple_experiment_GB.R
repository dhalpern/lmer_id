library(RePsychLing)
library(lme4)

# Try multiple submatrices ------------------------------------------------

get_idx <- function(chf, rank) {
  xx <- tcrossprod(chf)
  qr_xx <- qr(xx, LAPACK = TRUE)
  idx <- qr_xx$pivot[1:rank]
  idx
}

singularity_tol <- lmerControl()$checkConv$check.conv.singular$tol

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
  sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + (1 + F | session) + 
    (0 + T + TF | session) + (1 + P | item) + (0 + T + TP | item), 
  gb12, REML = FALSE)
m_id_both_pca <- rePCA(m_id_both)
session_rank_id_both <- sum(m_id_both_pca$session$sdev > singularity_tol)
item_rank_id_both <- sum(m_id_both_pca$item$sdev > singularity_tol)

(session_2_idx <- get_idx(getME(m_id_both, "Tlist")[[2]], 1))
(item_1_idx <- get_idx(getME(m_id_both, "Tlist")[[3]], 1))
(item_2_idx <- get_idx(getME(m_id_both, "Tlist")[[4]], 1))

m_id_both2 <- lmer(
  sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + (1 + F | session) + 
    (0 + T | session) + (1 + P || item) + (0 + T + TP || item), 
  gb12, REML = FALSE)
rePCA(m_id_both2)

m_id_both3 <- lmer(
  sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + (1 + F | session) + 
    (0 + T | session) + (1 + T || item), 
  gb12, REML = FALSE)
