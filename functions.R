# ---- functions ----

# Ages to lengths - Von Bertalanffy Growth Function.
fun_a_l = function(a, linf, k, t0){l = linf * (1 - exp(-k * (a - t0)))}

# Lengths to weights.
fun_l_w = function(a, l, b){w = a * l ^ b}

# Ages to natural mortalities.
fun_a_nmort = function(a, a_mat, a_old, m_juv, m_mat, m_old){s = ifelse(a < a_mat, m_juv, ifelse(a < a_old, m_mat, m_old))}

# Lengths to selectivities.
fun_l_s = function(l, a, b, m){s = a / (1 + exp(b - m * l))}

# Ages to bycatch mortalities.
fun_a_bmort = function(a, b_b, a_mat, n0){b = ifelse(a < a_mat, 0.20, 0)}#(b_b / round(a_mat)) / n0[round(a_mat)], 0)}

# Numbers at age to recruitment - Shepherd Recruitment Function.
fun_rec = function(n, a_rec, b_rec, d_rec){n0 = (a_rec * n) / ((1 + n / b_rec) ^ d_rec)}

# Production and grams to price in multivariate inverse demand specification.
fun_p = function(q, g, a_ma, b_ma, c_ma){p = q * a_ma + g ^ b_ma + c_ma}

# Ages to natural mortalities in aquaculture.
fun_a_aqmort = function(a, b1, b2){m = b1 * exp(b2 * a * 12) + 1} # Turn that 1 into a parameter for the minimum mortality.

# Weights to optimal stocking densities in numbers.
fun_ns = function(cage_size_aq, dens_aq, w){ns = (cage_size_aq * dens_aq) / w}