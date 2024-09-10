library(RePsychLing)
library(lme4)

H <- function( X ) {
  if(is.complex(X)) {
    return( Conj(t(X)) )
  } else {
    return( t(X) )
  }
}
pinv <- function(A){
  s <- svd(A)
  nz <- s$d > s$d[1] * .Machine$double.eps
  if(any(nz)){
    return(s$v[, nz] %*% (H(s$u[, nz]) / s$d[nz]))
  } else {
    return(A)
  }
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

S <- qr.R(qr0_session)
k <- session_rank0
n <- length(qr0_session$pivot)
V = pinv(S[1:k, 1:k]) %*% S[1:k, (k + 1):n]
V <- pinv(S[1:k, 1:k]) %*% S[1:k, (k + 1):n]
Z <- cbind(diag(k), V)
Z <- matrix(Z[, ordered.pivot], nrow = k, ncol = n)
C <- matrix(xx_session[, session_idx], nrow = n, ncol = k)
xx_re <- t(C %*% Z)
xx_session_resid <- xx_session[-session_idx, -session_idx] - xx_re[-session_idx, -session_idx]
qr0_session_resid <- qr(xx_session_resid, LAPACK = TRUE)
session_resid_idx <- qr0_session_resid$pivot #2, 1
