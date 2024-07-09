library(RePsychLing)
library(lme4)
library(rsvd)

m0 <- lmer(sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + 
             (1 + T + F + TF | session) + (1 + T + P + TP | item), 
           gb12, REML = FALSE)
summary(rePCA(m0))  # both item and session have rank 2

# Do RID on both ----------------------------------------------------------

chf0_session <- getME(m0, "Tlist")[[1]]
(xx_session <- tcrossprod(chf0_session))
id_out_session <- rid(xx_session, k = 2)
id_out_session$idx # 1 and 3

chf0_item <- getME(m0, "Tlist")[[1]]
(xx_item <- tcrossprod(chf0_item))
id_out_item <- rid(xx_item, k = 2)
id_out_item$idx # 1 and 3

m_rid_both <- lmer(
  sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + (1 + F | session) + (1 + P | item), 
  gb12, REML = FALSE)
summary(rePCA(m_rid_both))

# Do RID on session first ----------------------------------------------------

m_rid_session <- lmer(
  sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + (1 + F | session) + (1 + T + P + TP | item), 
  gb12, REML = FALSE)
summary(rePCA(m_rid_session)) # item has rank 2

chf_rid_item <- getME(m_rid_session, "Tlist")[[2]]
(xx_rid_item <- tcrossprod(chf_rid_item))
id_out_rid_item <- rid(xx_rid_item, k = 2)
id_out_rid_item$idx # This results in the same as before

# Do RID on item first ----------------------------------------------------

m_rid_item <- lmer(
  sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + (1 + T + F + TF | session) + (1 + P | item), 
  gb12, REML = FALSE)
summary(rePCA(m_rid_item)) # item is still not full rank!


