for (t in 1:MaxTime) {
  for (i in 1:n) {

    if (ActualConnects > 0) {
      #### For those with contacts.
      contacts = sample(Partners[[i]], 10, replace = TRUE)
      PI[i] = 1 - prod(1 - beta * S[contacts, t])
      if (PI[i] < runif(1)) {
        S[i, t + 1] = 1
      } else {
        S[i, t + 1] = 0
      }
    } else {
      S[i, t + 1] = S[i, t]
    }

    if (S[i, t + 1] == 1) {

      Test = HealthInsure[i] * 0.15 + (1-HealthInsure)*deltavec(NeighborhoodVec[i])
      #### Chance you get tested. Interviduals without insurance depend on free clinics
      #### for access.
      if (runif(1) < Test) {
        S[i, t + 1] = 0  #####Test and Recover
      } else {
        S[i, t + 1] = S[i, t]  ##########No test, no recovery
      }

    }

  }
}
