# [LONGITUDINAL] DEFINITION:

    Code
      print(tmp)
    Output
      Seizure = {type=event, eventType=intervalCensored, maxEventNumber=1, rightCensoringTime=120, intervalLength=10, hazard=haz}

---

    Code
      print(tmp)
    Output
      level = {type=categorical, categories= {0, 1, 2, 3},
      logit(P(level <=0)) = th1
      logit(P(level <=1)) = th1 + th2
      logit(P(level <=2)) = th1 + th2 + th3}

---

    Code
      print(tmp)
    Output
      y = {type=count,
      P(y=k) = exp(-lambda)*(lambda^k)/factorial(k)}

