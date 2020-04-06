# ---- fun ----

# Ages to lengths - Von Bertalanffy Growth Function.
fun_a_l = function(a, linf, k, t0){l = linf * (1 - exp(-k * (a - t0)))}

# Lengths to weights.
fun_l_w = function(a, l, b){w = a * l ^ b}

# Ages to natural mortalities.
fun_a_nmort = function(a, a_mat, a_old, m_juv, m_mat, m_old){s = 1 - ifelse(a < a_mat, 
                                                                            exp(-m_juv), 
                                                                            ifelse(a < a_old, 
                                                                                   exp(-m_mat), 
                                                                                   exp(-m_old)))}

# Lengths to selectivities.
fun_l_s = function(l, a, b, m){s = a / (1 + exp(b - m * l))}

# Ages to bycatch mortalities.
fun_a_bmort = function(a, b_b, a_mat, n0){b = ifelse(a < a_mat, 
                                                     b_b / floor(a_mat) / n0[floor(a_mat)],  
                                                     0)}

#(b_b / round(a_mat)) / n0[round(a_mat)], 0)} The code in use is a Band-Aid. Bycatch = 0.2 for a < a_mat.

# Numbers at age to recruitment - Shepherd Recruitment Function.
fun_rec = function(n, a_rec, b_rec, d_rec, f1_rec, f2_rec)
  
{n0 = ifelse((a_rec * n) / (1 + (n / b_rec) ^ d_rec) * exp(f1_rec * f2_rec) > 0, 
             (a_rec * n) / (1 + (n / b_rec) ^ d_rec) * exp(f1_rec * f2_rec),
             0)}

# Production and grams to price in multivariate inverse demand specification.
#  Deprecated nonlinear option.
# fun_p = function(q, g, a_ma, b_ma, c_ma){p = q * a_ma + g ^ b_ma + c_ma
#                                          return(ifelse(p > 0, p, 0))}
#  Linear option.
fun_p = function(q, g, a_ma, b_ma, c_ma)
  
  {
  
  p = q * a_ma + g * b_ma + c_ma
  return(ifelse(p > 0, p, 0))
  
  }

# Ages to natural mortalities in aquaculture.
fun_a_aqmort = function(a, b1, b2, mmin){m = b1 * exp(b2 * a * 12) + mmin}

# Weights to optimal stocking densities in numbers.
fun_ns = function(cage_size, dens, w){ns = (cage_size * dens) / w}

# The whole hog.
fun = function(par){
  
  # Name inputs.
  for(i in 1:nrow(par)){assign(rownames(par)[i], par[i,])}
  # Run intermediate set-up.
  #  Fishery.
  #   Numbers in 2017.
  n0 = dat_bio$n * nprop
  #   Catchability.
  # F = qENS > q = F / ENS; N is in numbers, F is in tonnes, and S is in proportions, so conversions are in order.
  q = 1000 * f_2017 / sum(n0 * fun_l_w(a_lw, fun_a_l(seq(a_0, a_i), linf_al, k_al, t0_al), b_lw) * fun_l_s(fun_a_l(seq(a_0, a_i), linf_al, k_al, t0_al), a_ls, b_ls, m_ls) * e_2017)
  #  Aquaculture.
  #   Cohort count at first saleable size is the ratio of density in kgm^-3 to size in kg.
  nsale = fun_ns(cage_size, dens, sale_size)
  #   Initial cohort count is density at first saleable size, plus cumulative mortality at first saleable age.
  #   Casually, nstart = nsale + mort(a(l(wsale))).
  #   Cumulative mortality to first saleable age:
  a_sale = (t0_al - 1 / k_al * (log(1 - ((sale_size / a_lw) ^ (1 / b_lw)) / linf_al))) # Inverse Von Bertalanffy.
  #   Initial stock to reach optimal density at first salable size:
  nstart = nsale * (100 / (100 - fun_a_aqmort(a_sale, b1_mort, b2_mort, mmin))) ^ a_sale
  
  # Build objects to fill.
  #  Fishery.
  n = matrix(nrow = t_i - t_0 + 1, ncol = a_i - a_0 + 1) # Build a matrix of numbers at age.
  m = matrix(nrow = t_i - t_0 + 1, ncol = a_i - a_0 + 1) # Build a matrix of natural mortalities at age.
  b = matrix(nrow = t_i - t_0 + 1, ncol = a_i - a_0 + 1) # Build a matrix of bycatch at age.
  y = matrix(nrow = t_i - t_0 + 1, ncol = a_i - a_0 + 1) # Build a matrix of catch at age.
  g = matrix(nrow = t_i - t_0 + 1, ncol = a_i - a_0 + 1) # Build a matrix of ghost catch at age.
  p_mat = matrix(nrow = t_i - t_0 + 1, ncol = a_i - a_0 + 1) # Build a matrix of prices at age.
  a_matrix = matrix(nrow = a_i - a_0 + 1, ncol = t_i - t_0 + 1) # Build a matrix of ages for reference in functions. Transposed.
  rec = as.numeric(vector(length = t_i - t_0 + 1))  # Build a vector of recruitment at age.
  e = as.numeric(vector(length = t_i - t_0 + 1))  # Build a vector of effort. This is the variable for optimization in the economic component.
  r_fi = as.numeric(vector(length = t_i - t_0 + 1))  # Build a vector of total revenues.
  c_fi = as.numeric(vector(length = t_i - t_0 + 1))  # Build a vector of total costs.
  #  Aquaculture.
  #   Current.
  a0_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages) # Ages of stock.
  w0_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages) # Weights.
  nm0_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)# Mortalities.
  ns0_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)# Survivors.
  nt0_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)# Trimming.
  n0_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages) # Leftovers.
  rt0_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)# Revenues, trimming.
  p0_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages) # Prices, maw.
  r0_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages) # Revenues, maw.
  c0_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages) # Costs.
  #   Led.
  a1_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)
  w1_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)
  nm1_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)
  ns1_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)
  nt1_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)
  n1_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)
  rt1_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)
  p1_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)
  r1_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)
  c1_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)
  #   Outputs.
  h_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)    # Decision.
  hinv_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages) # Decision, inverse.
  r_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)    # Revenues.
  c_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)    # Costs.
  y_aq = matrix(nrow = t_i - t_0 + 1, ncol = c_cages)    # Production, wet tonnes.

  # Add initial values.
  #  Fishery.
  a_matrix[, 1:(t_i - t_0 + 1)] = seq(a_0, a_i) # Matrix of ages.
  a_matrix = t(a_matrix) # Transposing matrix of ages.
  n[1,] = n0 # Age distribution for first year, e.g. 2017.
  m[1,] = n[1,] * fun_a_nmort(a_matrix[1,], a_mat_am, a_old_am, m_juv_am, m_mat_am, m_old_am) # Natural mortalities.
  b[1,] = (n[1,] - m[1,]) * fun_a_bmort(a_matrix[1,], b_b, a_mat_am, n0) # Bycatch mortalities by cohort for first year.
  e[1] = e_2017 # Effort in boats/season for 2017.
  y[1,] = (n[1,] - m[1,] - b[1,]) * q * e[1] * fun_l_s(fun_a_l(a_matrix[1,], linf_al, k_al, t0_al), a_ls, b_ls, m_ls) # Catch for first year by cohort.
  g[1,] = (n[1,] - m[1,] - b[1,] - y[1,]) * g_r * q * e[1] * fun_l_s(fun_a_l(a_matrix[1,], linf_al, k_al, t0_al), a_ls, b_ls, m_ls) # Catch for first year by cohort.
  p_mat[1,] = fun_p(sum(fun_l_w(a_lw, fun_a_l(a_matrix[1, ], linf_al, k_al, t0_al), b_lw) * y[1, ] * by1 * by2) / 1000, # Prices from tonnes of production and grams of maw at age. Placeholder names.
                    fun_l_w(a_lw, fun_a_l(a_matrix[1, ], linf_al, k_al, t0_al), b_lw) * by1 * by2 * 1000,
                    a_ma, b_ma, c_ma) * loss
  r_fi[1] = sum(p_mat[1,] * fun_l_w(a_lw, fun_a_l(a_matrix[1, ], linf_al, k_al, t0_al), b_lw) * y[1, ] * by1 * by2 * 1000) # Constant for conversion to grams of buche.
  c_fi[1] = e[1] * c_2017 + e[1] * switch_en * multi_en * c_enf # Costs for first year. Vessel costs, crew shares of profit, and enforcement intensification.
  rec[1] = fun_rec(sum(n[1, 4:(a_i - a_0 + 1)]), a_r, b_r, d_r, f1_r, f2_r) # Recruitment for first year. Start of column designation is hard-coded.
  eta = (e[1] * eta_limit) / abs(r_fi[1] - c_fi[1]) # Parameter to restrict changes in effort.

  #  Aquaculture.
  #   Current.
  # a0_aq[1,] = round(runif(c_cages, 0, ceiling(a_sale))) # Ages set from random uniform distribution.
  a0_aq[1,] = rep(ceiling(a_sale), c_cages) # Ages set to first harvest.
  w0_aq[1,] = fun_l_w(a_lw, fun_a_l(a0_aq[1,], linf_al, k_al, t0_al), b_lw)
  nm0_aq[1,] = nstart * (0.01 * fun_a_aqmort(a0_aq[1,], b1_mort, b2_mort, mmin))
  ns0_aq[1,] = nstart * (1 - 0.01 * fun_a_aqmort(a0_aq[1,], b1_mort, b2_mort, mmin)) # Note leading mortality.
  nt0_aq[1,] = ifelse(ns0_aq[1,] - fun_ns(cage_size, dens, w0_aq[1,]) > 0,
                      ns0_aq[1,] - fun_ns(cage_size, dens, w0_aq[1,]),
                      0)
  n0_aq[1,] = nstart - nm0_aq[1,] - nt0_aq[1,]
  #for(j in 1:c_cages){p0_aq[1, j] = p_mat[1, a0_aq[1, j]] * 1000} # Looping to enable position references in the price matrix.
  p0_aq[1,] = rep(10, c_cages) # Watch out for this Band-Aid.
  rt0_aq[1,] = nt0_aq[1,] * w0_aq[1,] * by1 * by2 * p0_aq[1,] * switch_aq + nt0_aq[1,] * w0_aq[1,] * wy * wp # Trimming revenues for maw and wet product. Fix placeholder names.
  r0_aq[1,] = w0_aq[1,] * n0_aq[1,] * by1 * by2 * n0_aq[1,] * p0_aq[1,] * switch_aq + n0_aq[1,] * w0_aq[1,] * wy * wp # Harvest revenues for maw and wet product. Fix placeholder names.
  c0_aq[1,] = n0_aq[1,] * fun_l_w(a_lw, fun_a_l(a0_aq[1,] - 0.5, linf_al, k_al, t0_al), b_lw) * feed_prop * feed_cost * 365 # Fix placeholder variable names.

  #   Led.
  a1_aq[1,] = a0_aq[1,] + 1
  w1_aq[1,] = fun_l_w(a_lw, fun_a_l(a1_aq[1,], linf_al, k_al, t0_al), b_lw)
  # Since this implementation of mortality/survival and trimming require iteration to work, the corresponding lead variables are spaghetti.
  nm1_aq[1,] = (nstart * (1 - 0.01 * fun_a_aqmort(a1_aq[1,] - 1, b1_mort, b2_mort, mmin)) -
                 ifelse(nstart * (1 - 0.01 * fun_a_aqmort(a1_aq[1,] - 1, b1_mort, b2_mort, mmin) - fun_ns(cage_size, dens, w1_aq[1,])) > 0,
                        nstart * (1 - 0.01 * fun_a_aqmort(a1_aq[1,] - 1, b1_mort, b2_mort, mmin) - fun_ns(cage_size, dens, w1_aq[1,])),
                        0)) * (0.01 * fun_a_aqmort(a1_aq[1,], b1_mort, b2_mort, mmin))
  ns1_aq[1,] = (nstart * (1 - 0.01 * fun_a_aqmort(a1_aq[1,] - 1, b1_mort, b2_mort, mmin)) -
                 ifelse(nstart * (1 - 0.01 * fun_a_aqmort(a1_aq[1,] - 1, b1_mort, b2_mort, mmin) - fun_ns(cage_size, dens, w1_aq[1,])) > 0,
                        nstart * (1 - 0.01 * fun_a_aqmort(a1_aq[1,] - 1, b1_mort, b2_mort, mmin) - fun_ns(cage_size, dens, w1_aq[1,])),
                        0)) * (1 - 0.01 * fun_a_aqmort(a1_aq[1,], b1_mort, b2_mort, mmin))
  nt1_aq[1,] = ifelse(ns1_aq[1,] - fun_ns(cage_size, dens, w1_aq[1,]), ns1_aq[1,] - fun_ns(cage_size, dens, w1_aq[1,]), 0)
  n1_aq[1,] = fun_ns(cage_size, dens, w1_aq[1,])
  p0_aq[1,] = rep(10, c_cages)
  #for(j in 1:c_cages){p0_aq[1, j] = p_mat[1, a0_aq[1, j]] * 1000} # Looping to enable position references in the price matrix.
  rt1_aq[1,] = nt1_aq[1,] * w1_aq[1,] * by1 * by2 * p1_aq[1,] * switch_aq + nt1_aq[1,] * w1_aq[1,] * wy * wp # Trimming revenues for maw and wet product. Fix placeholder names.
  r1_aq[1,] = w1_aq[1,] * n1_aq[1,] * by1 * by2 * n1_aq[1,] * p1_aq[1,] * switch_aq + n1_aq[1,] * w1_aq[1,] * wy * wp # Harvest revenues for maw and wet product. Fix placeholder names.
  c1_aq[1,] = n1_aq[1,] * fun_l_w(a_lw, fun_a_l(a1_aq[1,] - 0.5, linf_al, k_al, t0_al), b_lw) * feed_prop * feed_cost * 365 # Fix placeholder variable names.

  h_aq[1,] = 0
  hinv_aq[1,] = 1

  r_aq[1,] = r0_aq[1,] * h_aq[1,] + rt0_aq[1,] * hinv_aq[1,]
  c_aq[1,] = c0_aq[1,] * hinv_aq[1,] + fry * nstart * h_aq[1,] + over
  y_aq[1,] = n0_aq[1,] * w0_aq[1,] * h_aq[1,] + nt0_aq[1,] * w0_aq[1,] * hinv_aq[1,]

  # Add iterations.
  for(i in 2:(t_i - t_0 + 1)){
    for(j in 2:(a_i - a_0 + 1)){
      # Fishery.
      #  Numbers for time i and cohort j are numbers of the previous time and cohort less mortalities of the previous time and cohort.
      n[i, j] = ifelse(n[i - 1, j - 1] - m[i - 1, j - 1] - b[i - 1, j - 1] - y[i - 1, j - 1] - g[i - 1, j - 1] > 0,
                       n[i - 1, j - 1] - m[i - 1, j - 1] - b[i - 1, j - 1] - y[i - 1, j - 1] - g[i - 1, j - 1],
                       0)

      #  Natural mortalities for time i and cohort j are numbers for the same multipled by a constant factor for marginal mortality.
      m[i, j] = n[i, j] * fun_a_nmort(a_matrix[i, j], a_mat_am, a_old_am, m_juv_am, m_mat_am, m_old_am)

      #  Bycatch for time i and cohort j are numbers for the same after natural mortality multipled by constant bycatch mortality..
      b[i, j] = (n[i, j] - m[i, j]) * fun_a_bmort(a_matrix[i, j], b_b, a_mat_am, n0)

    }

    #  Numbers for time i and first cohort.
    n[i, 1] = rec[i - 1]

    #  Natural mortalities for time i and first cohort.
    m[i, 1] = n[i, 1] * fun_a_nmort(a_matrix[i, 1], a_mat_am, a_old_am, m_juv_am, m_mat_am, m_old_am)

    #  Bycatch mortality for time i and first cohort.
    b[i, 1] = (n[i, 1] - m[i, 1]) * fun_a_bmort(a_matrix[i, 1], b_b, a_mat_am, n0)

    # Effort for time i and all cohorts from past effort, revenues, costs, and a stiffness parameter.
    e[i] = ifelse(e[i - 1] + eta * (r_fi[i - 1] - c_fi[i - 1]) > 0,
                  e[i - 1] + eta * (r_fi[i - 1] - c_fi[i - 1]),
                  0) # Bound positive.

    #  Catches from effort and the rest.
    for(j in 2:(a_i - a_0 + 1)){
      y[i, j] = ifelse((n[i, j] - m[i, j] - b[i, j]) * q * e[i] * fun_l_s(fun_a_l(a_matrix[i, j], linf_al, k_al, t0_al), a_ls, b_ls, m_ls) > 0,
                       (n[i, j] - m[i, j] - b[i, j]) * q * e[i] * fun_l_s(fun_a_l(a_matrix[i, j], linf_al, k_al, t0_al), a_ls, b_ls, m_ls),
                       0)
      y[i, 1] = ifelse((n[i, 1] - m[i, 1] - b[i, 1]) * q * e[i] * fun_l_s(fun_a_l(a_matrix[i, 1], linf_al, k_al, t0_al), a_ls, b_ls, m_ls) > 0,
                       (n[i, 1] - m[i, 1] - b[i, 1]) * q * e[i] * fun_l_s(fun_a_l(a_matrix[i, 1], linf_al, k_al, t0_al), a_ls, b_ls, m_ls),
                       0)
    }

    #  Ghost catches from past effort, etc.
    for(j in 2:(a_i - a_0 + 1)){
      g[i, j] = ifelse((n[i, j] - m[i, j] - b[i, j] - y[i, j]) * g_r * q * e[i - 1] * fun_l_s(fun_a_l(a_matrix[i, j], linf_al, k_al, t0_al), a_ls, b_ls, m_ls) > 0,
                       (n[i, j] - m[i, j] - b[i, j] - y[i, j]) * g_r * q * e[i - 1] * fun_l_s(fun_a_l(a_matrix[i, j], linf_al, k_al, t0_al), a_ls, b_ls, m_ls),
                       0)
      g[i, 1] = ifelse((n[i, 1] - m[i, 1] - b[i, 1] - y[i, j]) * g_r * q * e[i - 1] * fun_l_s(fun_a_l(a_matrix[i, 1], linf_al, k_al, t0_al), a_ls, b_ls, m_ls) > 0,
                       (n[i, 1] - m[i, 1] - b[i, 1] - y[i, j]) * g_r * q * e[i - 1] * fun_l_s(fun_a_l(a_matrix[i, 1], linf_al, k_al, t0_al), a_ls, b_ls, m_ls),
                       0)
    }

    #  Recruitment for time i.
    rec[i] = fun_rec(sum(n[i, 4:(a_i - a_0 + 1)]), a_r, b_r, d_r, f1_r, f2_r)

    # Aquaculture.
    a0_aq[i,] = a0_aq[i - 1,] * hinv_aq[i - 1,] + 1
    w0_aq[i,] = fun_l_w(a_lw, fun_a_l(a0_aq[i,], linf_al, k_al, t0_al), b_lw)
    nm0_aq[i,] = (nstart * h_aq[i - 1,] + n0_aq[i - 1,] * hinv_aq[i - 1,]) * (0.01 * fun_a_aqmort(a0_aq[i,], b1_mort, b2_mort, mmin)) # Note leading mortality.
    ns0_aq[i,] = (nstart * h_aq[i - 1,] + n0_aq[i - 1,] * hinv_aq[i - 1,]) * (1 - 0.01 * fun_a_aqmort(a0_aq[i,], b1_mort, b2_mort, mmin)) # Note leading mortality.
    nt0_aq[i,] = ifelse(ns0_aq[i,] - fun_ns(cage_size, dens, w0_aq[i,]) > 0, ns0_aq[i,] - fun_ns(cage_size, dens, w0_aq[i,]), 0)
    n0_aq[i,] = (nstart * h_aq[i - 1,] + n0_aq[i - 1,] * hinv_aq[i - 1,]) - nm0_aq[i,] - nt0_aq[i,]
    p0_aq[i,] = p_mat[i - 1, a0_aq[i,]] * 1000 # Conversion for price in grams to revenue from kilograms of dry maw.
    rt0_aq[i,] = nt0_aq[i,] * w0_aq[i,] * by1 * by2 * p0_aq[i,] * switch_aq + nt0_aq[i,] * w0_aq[i,] * wy * wp # Trimming revenues.
    r0_aq[i,] = n0_aq[i,] * w0_aq[i,] * by1 * by2 * p0_aq[i,] * switch_aq + n0_aq[i,] * w0_aq[i,] * wy * wp # Harvest revenues.
    c0_aq[i,] = n0_aq[i,] * fun_l_w(a_lw, fun_a_l(a0_aq[i,] - 0.5, linf_al, k_al, t0_al), b_lw) * feed_prop * feed_cost * 365

    a1_aq[i,] = a0_aq[i,] + 1
    w1_aq[i,] = fun_l_w(a_lw, fun_a_l(a1_aq[i,], linf_al, k_al, t0_al), b_lw)
    nm1_aq[i,] = (nstart * h_aq[i - 1,] + n1_aq[i - 1,] * hinv_aq[i - 1,]) * (0.01 * fun_a_aqmort(a1_aq[i,], b1_mort, b2_mort, mmin)) # Note leading mortality.
    ns1_aq[i,] = (nstart * h_aq[i - 1,] + n1_aq[i - 1,] * hinv_aq[i - 1,]) * (1 - 0.01 * fun_a_aqmort(a1_aq[i,], b1_mort, b2_mort, mmin)) # Note leading mortality.
    nt1_aq[i,] = ifelse(ns1_aq[i,] - fun_ns(cage_size, dens, w1_aq[i,]) > 0, ns1_aq[i,] - fun_ns(cage_size, dens, w1_aq[i,]), 0)
    n1_aq[i,] = (nstart * h_aq[i - 1,] + n1_aq[i - 1,] * hinv_aq[i - 1,]) - nm1_aq[i,] - nt1_aq[i,]
    p1_aq[i,] = p_mat[i - 1, a1_aq[i,]] * 1000 # Conversion for price in grams to revenue from kilograms of dry maw.
    rt1_aq[i,] = nt1_aq[i,] * w1_aq[i,] * by1 * by2 * p1_aq[i,] * switch_aq + nt1_aq[i,] * w1_aq[i,] * wy * wp # Trimming revenues.
    r1_aq[i,] = n1_aq[i,] * w1_aq[i,] * by1 * by2 * p1_aq[i,] * switch_aq + n1_aq[i,] * w1_aq[i,] * wy * wp # Harvest revenues.
    c1_aq[i,] = n1_aq[i,] * fun_l_w(a_lw, fun_a_l(a1_aq[i,] - 0.5, linf_al, k_al, t0_al), b_lw) * feed_prop * feed_cost * 365

    h_aq[i,] = ifelse(a0_aq[i,] > ceiling(a_sale), # Wrapper for minimum sale age.
                     ifelse(r0_aq[i,] - fry * nstart > rt0_aq[i,] - c0_aq[i,] + disc * (r1_aq[i,] - fry * nstart), # Faustmann.
                            1,
                            0),
                     0)
    hinv_aq[i,] = (h_aq[i,] - 1) ^ 2

    y_aq[i,] = n0_aq[i,] * w0_aq[i,] * h_aq[i,] + nt0_aq[i,] * w0_aq[i,] * hinv_aq[i,]
    r_aq[i,] = r0_aq[i,] * h_aq[i,] + rt0_aq[i,] * hinv_aq[i,]
    c_aq[i,] = c0_aq[i,] * hinv_aq[i,] + fry * nstart * h_aq[i,] + over

    # Prices in matrix.
    for(j in 1:(a_i - a_0 + 1)){
      p_mat[i, j] = fun_p(sum(fun_l_w(a_lw, fun_a_l(a_matrix[i, ], linf_al, k_al, t0_al), b_lw) * y[i, ] * by1 * by2, # Fishery production.
                              (n0_aq[i,] * w0_aq[i,] * by1 * by2 * h_aq[i,] + nt0_aq[i,] * w0_aq[i,] * by1 * by2 * hinv_aq[i,]) * switch_aq * sub) # Aquaculture production.
                          / 1000, # Conversion to tonnes.
                          fun_l_w(a_lw, fun_a_l(a_matrix[i, j], linf_al, k_al, t0_al), b_lw) * by1 * by2,
                          a_ma,
                          b_ma,
                          c_ma * dem) * loss
      p_mat[i, j] = ifelse(p_mat[i, j] > 0, p_mat[i, j], 0)
    }

    # Revenues.
    r_fi[i] = sum(p_mat[i,] * fun_l_w(a_lw, fun_a_l(a_matrix[i, ], linf_al, k_al, t0_al), b_lw) * y[i, ] * by1 * by2 * 1000) # Constant for conversion to grams of buche.

    # Costs.
    c_fi[i] = e[i] * c_2017 + e[i] * switch_en * multi_en * c_enf
  }

  # Tidy results: numbers, recruitment, catches, effort, revenues, costs, profits.
  #  Numbers.
  tidyn = melt(n)
  tidyn$var = "Numbers"
  #  Catches.
  tidyy = melt(y)
  tidyy$var = "Catches"
  #  Poaching Profit.
  tidypi_fi = rename(data.frame(matrix(NA,
                                       nrow = t_i - t_0 + 1,
                                       ncol = 4)),
                     Var1 = X1,
                     Var2 = X2,
                     value = X3,
                     var = X4)
  tidypi_fi$Var1 = seq(1, t_i - t_0 + 1)
  tidypi_fi$Var2 = NA
  tidypi_fi$value = r_fi - c_fi
  tidypi_fi$var = "Poaching Profit"
  #  Poaching Revenue.
  tidyr_fi = rename(data.frame(matrix(NA,
                                       nrow = t_i - t_0 + 1,
                                       ncol = 4)),
                     Var1 = X1,
                     Var2 = X2,
                     value = X3,
                     var = X4)
  tidyr_fi$Var1 = seq(1, t_i - t_0 + 1)
  tidyr_fi$Var2 = NA
  tidyr_fi$value = r_fi
  tidyr_fi$var = "Poaching Revenue"
  # Aquaculture Profit.
  tidypi_aq = rename(data.frame(matrix(NA,
                                       nrow = t_i - t_0 + 1,
                                       ncol = 4)),
                     Var1 = X1,
                     Var2 = X2,
                     value = X3,
                     var = X4)
  tidypi_aq$Var1 = seq(1, t_i - t_0 + 1)
  tidypi_aq$Var2 = NA
  tidypi_aq$value = rowSums(r_aq) - rowSums(c_aq)
  tidypi_aq$var = "Aquaculture Profit"
  # Aquaculture Revenue.
  tidyr_aq = rename(data.frame(matrix(NA,
                                       nrow = t_i - t_0 + 1,
                                       ncol = 4)),
                     Var1 = X1,
                     Var2 = X2,
                     value = X3,
                     var = X4)
  tidyr_aq$Var1 = seq(1, t_i - t_0 + 1)
  tidyr_aq$Var2 = NA
  tidyr_aq$value = rowSums(r_aq)
  tidyr_aq$var = "Aquaculture Revenue"
  #  Prices. Using prices for a 13y/o fish for easy reference. It's representativish.
  tidyp = rename(data.frame(matrix(NA,
                                   nrow = t_i - t_0 + 1,
                                   ncol = 4)),
                 Var1 = X1,
                 Var2 = X2,
                 value = X3,
                 var = X4)
  tidyp$Var1 = seq(1, t_i - t_0 + 1)
  tidyp$Var2 = NA
  tidyp$value = p_mat[, 13]
  tidyp$var = "Price"
  #  Poaching Effort.
  tidye = rename(data.frame(matrix(NA, nrow = t_i - t_0 + 1,
                                   ncol = 4)),
                 Var1 = X1,
                 Var2 = X2,
                 value = X3,
                 var = X4)
  tidye$Var1 = seq(1, t_i - t_0 + 1)
  tidye$Var2 = NA
  tidye$value = e
  tidye$var = "Effort"
  #  Poaching Cost per Metric Ton.
  tidyc_fi = rename(data.frame(matrix(NA,
                                       nrow = t_i - t_0 + 1,
                                       ncol = 4)),
                     Var1 = X1,
                     Var2 = X2,
                     value = X3,
                     var = X4)
  tidyc_fi$Var1 = seq(1, t_i - t_0 + 1)
  tidyc_fi$Var2 = NA
  tidyc_fi$value = c_fi / sum((fun_l_w(a_lw, fun_a_l(a_matrix[1,], linf_al, k_al, t0_al), b_lw) * y * by1 * by2) / 1000)
  tidyc_fi$var = "Poaching Cost per Metric Ton"
  #  Aquaculture Cost per Metric Ton.
  tidyc_aq = rename(data.frame(matrix(NA, nrow = t_i - t_0 + 1,
                                      ncol = 4)),
                    Var1 = X1,
                    Var2 = X2,
                    value = X3,
                    var = X4)
  tidyc_aq$Var1 = seq(1, t_i - t_0 + 1)
  tidyc_aq$Var2 = NA
  tidyc_aq$value = rowSums(c_aq, na.rm = TRUE) / ((rowSums(y_aq, na.rm = TRUE) * by1 * by2) / 1000) # Summing cages and converting from kilograms to tonnes.
  tidyc_aq$var = "Aquaculture Cost per Metric Ton"
  #  Everything!
  tidy = bind_rows(tidyn, tidyy, tidypi_fi, tidyr_fi, tidypi_aq, tidyr_aq, tidyp, tidye, tidyc_fi, tidyc_aq)
  tidy = rename(tidy, Year = Var1, Age = Var2, Result = value, Variable = var)
  
  # Get results.
  return(tidy)
  
}
