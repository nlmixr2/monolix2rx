indDef <- function(val) {
  .Call(`_monolix2rx_trans_indDef`, val)
}


indDef("V = {distribution=logNormal, typical=V_pop, sd=omega_V}")
