indDef <- function(val) {
  .indDefIni()
  .Call(`_monolix2rx_trans_indDef`, val)
}

indDef("F = {distribution=logitnormal, typical=F_pop,sd=omega_F, min=0, max=1}
ka = {distribution=lognormal,typical=ka_pop, no-variability}
V = {distribution=lognormal,typical=V_pop, sd=omega_V }
Cl = {distribution=lognormal,typical=Cl_pop, sd=omega_Cl}
")

indDef("Tlag = {distribution=logNormal, typical=Tlag_pop, varlevel=id*occ, sd=gamma_Tlag}
ka = {distribution=logNormal, typical=ka_pop, varlevel={id, id*occ}, sd={omega_ka, gamma_ka}}
Cl = {distribution=logNormal, typical=Cl_pop, varlevel={id, id*occ}, sd={omega_Cl, gamma_Cl}}
V = {distribution=logNormal, typical=V_pop, sd=omega_V}
correlation = {level=id, r(V, Cl)=corr1_V_Cl}
correlation = {level=id*occ, r(ka, Tlag)=corr2_ka_Tlag}")
