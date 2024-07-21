#' Simulate PK based on the examplinib PopPK model
#'
#' @param event_table The event table as required by RxODE.
#' @return PK simulation as data frame
#' @keywords internal
pk_sim <- function(event_table) {
  if (!("EGFR" %in% colnames(event_table))) {
    event_table <- event_table %>%
      mutate(EGFR = 1)
  }

  if (!("CL_FACTOR" %in% colnames(event_table))) {
    event_table <- event_table %>%
      mutate(CL_FACTOR = 1)
  }

  keep_columns <- event_table %>%
    select(any_of(c("id", "time", "NTIME", "PERIOD"))) %>%
    distinct()

  mod <- rxode2::rxode2({
    c_centr <- centr / v_centr * (1 + centr.err)
    c_peri <- peri / v_peri
    c_metab <- metab / v_metab

    ke <- t.ke * exp(eta.ke) * (EGFR / 100)^0.9 # renal elimination constant
    ka <- t.ka * exp(eta.ka) + FOOD * t.ka1
    d1 <- t.d1 * exp(eta.d1)
    fm <- t.fm * exp(eta.fm) # fraction metabolized

    cl <- t.cl * exp(eta.cl) * CL_FACTOR # metabolic clearance

    kem <- t.kem * exp(eta.kem) # elimination constant for metabolite
    fpar <- 1 * exp(eta.fpar) + FOOD * t.fpar1
    if(FORM == 1) {
      fpar <- fpar * t.fpar2
    }
    q <- t.q * exp(eta.q)

    d / dt(depot) <- -ka * depot * fpar
    dur(depot) <- d1
    d / dt(centr) <- ka * depot * fpar - ke * c_centr - q * c_centr + q * c_peri - cl * c_centr
    d / dt(peri) <- q * c_centr - q * c_peri
    d / dt(renal) <- ke * c_centr * (1 - fm)

    d / dt(metab) <- cl * c_centr * fm - kem * c_metab
    d / dt(metab_excr) <- kem * c_metab
  })

  theta <- c(
    t.ka = 0.8,
    t.ka1 = 0.8,  # food effect on Ka
    t.d1 = 1.8,
    t.fpar1 = 2,  # food effect on F
    t.fpar2 = 0.6, # capsule formulation effect on F
    t.ke = 30,
    t.q = 5,
    t.cl = 20,
    #t.cl1 = -17,
    # t.kem = 10,
    t.kem = 2,
    t.fm = 0.8,
    v_centr = 100,
    v_peri = 100,
    v_metab = 10
  )

  omega <- rxode2::lotri(
    eta.ke + eta.ka + eta.d1 + eta.fpar + eta.q + eta.cl + eta.kem + eta.fm ~ c(
      0.3^2,
      0, 0.1^2,
      0, 0, 0.2^2,
      0, 0, 0, 0.3^2,
      0, 0, 0, 0, 0.3^2,
      0, 0, 0, 0, 0, 0.4^2,
      0, 0, 0, 0, 0, 0, 0.3^2,
      0, 0, 0, 0, 0, 0, 0, 0.03^2
    )
  )

  sigma <- rxode2::lotri(centr.err ~ .1^2)

  sim <- mod$solve(theta, event_table, omega = omega, sigma = sigma) %>%
    as.data.frame() %>%
    left_join(keep_columns, by = c("id", "time"))
  return(sim)
}
