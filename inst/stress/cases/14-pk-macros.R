## More PK: macros (01-pk.R has the common ones)

.macroModel <- function(input, pk, output="Cc") {
  paste0("DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {", input, "}

PK:
", pk, "

OUTPUT:
output = ", output, "
")
}

kitCase(
  name="macro-mm-elimination",
  covers="elimination(cmt=1, Vm, Km) Michaelis-Menten in concentration, compartment(volume=V, concentration=Cc)",
  tags=c("pk", "macro", "nonlinear"),
  sim=function() {
    ini({
      ka_pop <- 1.2; V_pop <- 30; Vm_pop <- 10; Km_pop <- 2
      omega_ka ~ 0.09; omega_V ~ 0.04; omega_Vm ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      ka <- ka_pop * exp(omega_ka)
      V <- V_pop * exp(omega_V)
      Vm <- Vm_pop * exp(omega_Vm)
      Km <- Km_pop
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - Vm * (central / V) / (Km + central / V)
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=.oralData,
  model=.macroModel("ka, V, Vm, Km", "compartment(cmt=1, amount=Ac, volume=V, concentration=Cc)
oral(cmt=1, ka)
elimination(cmt=1, Vm, Km)"),
  mlxtran=.mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Vm=.mlxPar(10, 0.3),
                           Km=.mlxPar(2))))

## zero-order input straight into the central compartment
kitCase(
  name="macro-oral-tk0-tlag-p",
  covers="oral(cmt=1, Tk0, Tlag, p): zero-order absorption with a lag and a bioavailability",
  tags=c("pk", "macro", "absorption"),
  sim=function() {
    ini({
      Tk0_pop <- 2; Tlag_pop <- 0.5; p_pop <- 0.7; V_pop <- 30; Cl_pop <- 3
      omega_Tk0 ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      Tk0 <- Tk0_pop * exp(omega_Tk0)
      Tlag <- Tlag_pop
      p <- p_pop
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      d/dt(central) <- -Cl / V * central
      dur(central) <- Tk0
      alag(central) <- Tlag
      f(central) <- p
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1, rate=-2),
            mlxObs(.id, pkTimes(48), cmt=1))
  },
  model=.macroModel("Tk0, Tlag, p, V, Cl", "compartment(cmt=1, amount=Ac)
oral(cmt=1, Tk0, Tlag, p)
elimination(cmt=1, k=Cl/V)
Cc = Ac/V"),
  mlxtran=.mlxProject(list(Tk0=.mlxPar(2, 0.3), Tlag=.mlxPar(0.5), p=.mlxPar(0.7, dist="logitNormal"),
                           V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3))))

kitCase(
  name="macro-3cmt-micro",
  covers="iv() with peripheral(k12, k21) and peripheral(k13, k31): three compartments from micro constants",
  tags=c("pk", "macro"),
  sim=function() {
    ini({
      V_pop <- 10; k_pop <- 0.2; k12_pop <- 0.5; k21_pop <- 0.2; k13_pop <- 0.1; k31_pop <- 0.02
      omega_V ~ 0.04; omega_k ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      V <- V_pop * exp(omega_V)
      k <- k_pop * exp(omega_k)
      k12 <- k12_pop
      k21 <- k21_pop
      k13 <- k13_pop
      k31 <- k31_pop
      d/dt(central) <- -(k + k12 + k13) * central + k21 * p2 + k31 * p3
      d/dt(p2) <- k12 * central - k21 * p2
      d/dt(p3) <- k13 * central - k31 * p3
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
            mlxObs(.id, c(0.25, 0.5, 1, 2, 4, 8, 12, 24, 48, 72, 96), cmt=1))
  },
  model=.macroModel("V, k, k12, k21, k13, k31", "compartment(cmt=1, amount=Ac)
iv(cmt=1)
peripheral(k12, k21)
peripheral(k13, k31)
elimination(cmt=1, k)
Cc = Ac/V"),
  mlxtran=.mlxProject(list(V=.mlxPar(10, 0.2), k=.mlxPar(0.2, 0.3), k12=.mlxPar(0.5),
                           k21=.mlxPar(0.2), k13=.mlxPar(0.1), k31=.mlxPar(0.02))))

kitCase(
  name="macro-effect",
  covers="effect(cmt=1, ke0, concentration=Ce) macro, the effect-site concentration observed",
  tags=c("pk", "macro", "effect"),
  sim=function() {
    ini({
      ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3; ke0_pop <- 0.3
      omega_ka ~ 0.09; omega_Cl ~ 0.09; omega_ke0 ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      ka <- ka_pop * exp(omega_ka)
      V <- V_pop
      Cl <- Cl_pop * exp(omega_Cl)
      ke0 <- ke0_pop * exp(omega_ke0)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - Cl / V * central
      d/dt(Ce) <- ke0 * (central / V - Ce)
      Ce ~ add(a) + prop(b) + combined1()
    })
  },
  data=.oralData,
  model=.macroModel("ka, V, Cl, ke0", "compartment(cmt=1, amount=Ac, volume=V, concentration=Cc)
oral(cmt=1, ka)
elimination(cmt=1, k=Cl/V)
effect(cmt=1, ke0, concentration=Ce)", output="Ce"),
  mlxtran=sub("prediction=Cc", "prediction=Ce",
              .mlxProject(list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30), Cl=.mlxPar(3, 0.3),
                               ke0=.mlxPar(0.3, 0.3))), fixed=TRUE))

kitCase(
  name="macro-iv-tlag-p",
  covers="iv(cmt=1, Tlag, p): an IV bolus with a lag time and a bioavailability",
  tags=c("pk", "macro"),
  sim=function() {
    ini({
      Tlag_pop <- 0.75; p_pop <- 0.8; V_pop <- 10; Cl_pop <- 2
      omega_Tlag ~ 0.09; omega_V ~ 0.04; omega_Cl ~ 0.09
      a <- 0.05; b <- 0.1
    })
    model({
      Tlag <- Tlag_pop * exp(omega_Tlag)
      p <- p_pop
      V <- V_pop * exp(omega_V)
      Cl <- Cl_pop * exp(omega_Cl)
      d/dt(central) <- -Cl / V * central
      alag(central) <- Tlag
      f(central) <- p
      Cc <- central / V
      Cc ~ add(a) + prop(b) + combined1()
    })
  },
  data=function(nSub) {
    .id <- seq_len(nSub)
    mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
            mlxObs(.id, c(0.5, 1, 1.5, 2, 4, 8, 12, 24), cmt=1))
  },
  model=.macroModel("Tlag, p, V, Cl", "compartment(cmt=1, amount=Ac)
iv(cmt=1, Tlag, p)
elimination(cmt=1, k=Cl/V)
Cc = Ac/V"),
  mlxtran=.mlxProject(list(Tlag=.mlxPar(0.75, 0.3), p=.mlxPar(0.8, dist="logitNormal"),
                           V=.mlxPar(10, 0.2), Cl=.mlxPar(2, 0.3))))
