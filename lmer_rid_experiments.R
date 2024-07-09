library(RePsychLing)
library(lme4)
library(rsvd)

m0 <- lmer(sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + 
             (1 + T + F + TF | session) + (1 + T + P + TP | item), 
           gb12, REML = FALSE)
summary(rePCA(m0))

# Do RID on both ----------------------------------------------------------

chf0_session <- getME(m0, "Tlist")[[1]]
(xx_session <- tcrossprod(chf0_session))
id_out_session <- rid(xx_session, k = dim(xx_session)[1] - 2, mode='column')
id_out_session$idx

chf0_item <- getME(m0, "Tlist")[[1]]
(xx_item <- tcrossprod(chf0_item))
id_out_item <- rid(xx_item, k = dim(xx_item)[1] - 2, mode='column')
id_out_item$idx

m_rid_both <- lmer(
  sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + (1 + F | session) + (1 + P | item), 
  gb12, REML = FALSE)
summary(rePCA(m_rid_both))

# Do RID on session first ----------------------------------------------------

chf0_session <- getME(m0, "Tlist")[[1]]
(xx_session <- tcrossprod(chf0_session))
id_out_session <- rid(xx_session, k = dim(xx_session)[1] - 2, mode='column')
id_out_session$idx

m_rid_session <- lmer(
  sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + (1 + F | session) + (1 + T + P + TP | item), 
  gb12, REML = FALSE)
summary(rePCA(m_rid_session))

chf0_item <- getME(m_rid_session, "Tlist")[[2]]
(xx_item <- tcrossprod(chf0_item))
id_out_item <- rid(xx_item, k = dim(xx_item)[1] - 2, mode='column')
id_out_item$idx

# Do RID on item first ----------------------------------------------------

chf0_item <- getME(m0, "Tlist")[[1]]
(xx_item <- tcrossprod(chf0_item))
id_out_item <- rid(xx_item, k = dim(xx_item)[1] - 2, mode='column')
id_out_item$idx

m_rid_item <- lmer(
  sottrunc2 ~ 1 + T + P + F + TP + TF + PF + TPF + (1 + T + F + TF | session) + (1 + P | item), 
  gb12, REML = FALSE)
summary(rePCA(m_rid_item))

chf0_session <- getME(m_rid_item, "Tlist")[[1]]
(xx_session <- tcrossprod(chf0_session))
id_out_session <- rid(xx_session, k = dim(xx_session)[1] - 2, mode='column')
id_out_session$idx

