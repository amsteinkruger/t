# ---- fun ----
    
    fun = function(par){
      
      # Name inputs.
      for(i in 1:nrow(par)){assign(rownames(par)[i], par[i,])}
      # Run intermediate set-up.
      #  Fishery.
      #   Numbers in 2017.
      n0 = fi_biom_dat %>%
        mutate(n = 1000 * bprop_2017 * Biomasa / fun_l_w(a_lw, fun_a_l(Edad, linf_al, k_al, t0_al), b_lw)) %>% 
        select(n)
      n0 = n0[[1]]
      #   Catchability.
      # F = qENS > q = F / ENS; N is in numbers, F is in tonnes, and S is in proportions, so conversions are in order.
      q = 1000 * f_2017 / sum(n0 * fun_l_w(a_lw, fun_a_l(seq(a_0, a_i), linf_al, k_al, t0_al), b_lw) * fun_l_s(fun_a_l(seq(a_0, a_i), linf_al, k_al, t0_al), a_ls, b_ls, m_ls) * e_2017)
      #  Aquaculture.
      #   Cohort count at first saleable size is the ratio of density in kgm^-3 to size in kg.
      nsale = fun_ns(cage_size_aq, dens_aq, sale_size_aq)
      #   Initial cohort count is density at first saleable size, plus cumulative mortality at first saleable age.
      #   Casually, nstart = nsale + mort(a(l(wsale))).
      #   Cumulative mortality to first saleable age:
      a_sale = (t0_al_aq - 1 / k_al_aq * (log(1 - ((sale_size_aq / a_lw) ^ (1 / b_lw)) / linf_al_aq))) # Inverse Von Bertalanffy.
      #   Initial stock to reach optimal density at first salable size:
      nstart = nsale * (100 / (100 - fun_a_aqmort(a_sale, b1_mort_aq, b2_mort_aq, mmin_aq))) * 
        (100 / (100 - fun_a_aqmort(a_sale, b1_mort_aq, b2_mort_aq, mmin_aq))) * 
        (100 / (100 - fun_a_aqmort(a_sale, b1_mort_aq, b2_mort_aq, mmin_aq)))
      
      # Build objects to fill.
      #  Fishery.
      n = matrix(nrow = t_i - t_0 + 1, ncol = a_i - a_0 + 1) # Build a matrix of numbers at age.
      m = matrix(nrow = t_i - t_0 + 1, ncol = a_i - a_0 + 1) # Build a matrix of natural mortalities at age.
      b = matrix(nrow = t_i - t_0 + 1, ncol = a_i - a_0 + 1) # Build a matrix of bycatch at age.
      y = matrix(nrow = t_i - t_0 + 1, ncol = a_i - a_0 + 1) # Build a matrix of catch at age.
      p_mat = matrix(nrow = t_i - t_0 + 1, ncol = a_i - a_0 + 1) # Build a matrix of prices at age.
      a_matrix = matrix(nrow = a_i - a_0 + 1, ncol = t_i - t_0 + 1) # Build a matrix of ages for reference in functions. Transposed.
      rec = as.numeric(vector(length = a_i - a_0 + 1))  # Build a vector of recruitment at age.
      e = as.numeric(vector(length = t_i - t_0 + 1))  # Build a vector of effort. This is the variable for optimization in the economic component.
      r_fi = as.numeric(vector(length = t_i - t_0 + 1))  # Build a vector of total revenues.
      c_fi = as.numeric(vector(length = t_i - t_0 + 1))  # Build a vector of total costs.
      #  Aquaculture.
      a0_aq = as.numeric(vector(length = t_i - t_0 + 1))
      a1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      h_aq = as.numeric(vector(length = t_i - t_0 + 1))
      hinv_aq = as.numeric(vector(length = t_i - t_0 + 1))
      l0_aq = as.numeric(vector(length = t_i - t_0 + 1)) #x
      w0_aq = as.numeric(vector(length = t_i - t_0 + 1)) #x
      nm0_aq = as.numeric(vector(length = t_i - t_0 + 1))
      ns0_aq = as.numeric(vector(length = t_i - t_0 + 1))
      nt0_aq = as.numeric(vector(length = t_i - t_0 + 1))
      n0_aq = as.numeric(vector(length = t_i - t_0 + 1))
      rt0_aq = as.numeric(vector(length = t_i - t_0 + 1)) #x
      y0_aq = as.numeric(vector(length = t_i - t_0 + 1)) #x
      p0_aq = as.numeric(vector(length = t_i - t_0 + 1)) 
      rmaw0_aq = as.numeric(vector(length = t_i - t_0 + 1))   
      rround0_aq = as.numeric(vector(length = t_i - t_0 + 1)) #x
      r0_aq = as.numeric(vector(length = t_i - t_0 + 1)) 
      c0_aq = as.numeric(vector(length = t_i - t_0 + 1))
      l1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      w1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      nm1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      ns1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      nt1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      n1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      rt1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      y1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      p1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      rmaw1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      rround1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      r1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      c1_aq = as.numeric(vector(length = t_i - t_0 + 1))
      r_aq = as.numeric(vector(length = t_i - t_0 + 1))
      c_aq = as.numeric(vector(length = t_i - t_0 + 1))
      
      # Add initial values.
      #  Fishery.
      a_matrix[, 1:(t_i - t_0 + 1)] = seq(a_0, a_i) # Matrix of ages. 
      a_matrix = t(a_matrix) # Transposing matrix of ages.
      n[1,] = n0 # Age distribution for first year, e.g. 2017.
      m[1,] = n[1,] * fun_a_nmort(a_matrix[1,], a_mat_am, a_old_am, m_juv_am, m_mat_am, m_old_am) # Natural mortalities.
      b[1,] = (n[1,] - m[1,]) * fun_a_bmort(a_matrix[1,], b_b, a_mat_am, n0) # Bycatch mortalities by cohort for first year.
      e[1] = e_2017 # Effort in boats/season for 2017.     
      y[1,] = (n[1,] - m[1,] - b[1,]) * q * e[1] * fun_l_s(fun_a_l(a_matrix[1,], linf_al, k_al, t0_al), a_ls, b_ls, m_ls) # Catch for first year by cohort.
      p_mat[1,] = fun_p(sum(fun_l_w(a_lw, fun_a_l(a_matrix[1, ], linf_al, k_al, t0_al), b_lw) * y[1, ] * by1 * by2) / 1000, # Prices from tonnes of production and grams of maw at age. Placeholder names.
                        fun_l_w(a_lw, fun_a_l(a_matrix[1, ], linf_al, k_al, t0_al), b_lw) * by1 * by2 * 1000, 
                        a_ma, b_ma, c_ma) * loss
      r_fi[1] = sum(p_mat[1,] * fun_l_w(a_lw, fun_a_l(a_matrix[1, ], linf_al, k_al, t0_al), b_lw) * y[1, ] * by1 * by2 * 1000) # Constant for conversion to grams of buche.
      c_fi[1] = e[1] * c_2017 # Costs for first year.
      rec[1] = fun_rec(sum(n[1, 2:(t_i - t_0 + 1)]), a_r, b_r, d_r) # Recruitment for first year.
      eta = (e[1] * eta_limit) / (r_fi[1] - c_fi[1]) # Parameter to restrict changes in effort.
      
      #  Aquaculture.
      #   Current.
      a0_aq[1] = a_0
      l0_aq[1] = fun_a_l(a0_aq[1], linf_al_aq, k_al_aq, t0_al_aq)
      w0_aq[1] = fun_l_w(a_lw, l0_aq[1], b_lw)
      nm0_aq[1] = nstart * (0.01 * fun_a_aqmort(a0_aq[1], b1_mort_aq, b2_mort_aq, mmin_aq))
      ns0_aq[1] = nstart * (1 - 0.01 * fun_a_aqmort(a0_aq[1], b1_mort_aq, b2_mort_aq, mmin_aq)) # Note leading mortality.
      nt0_aq[1] = ifelse(ns0_aq[1] - fun_ns(cage_size_aq, dens_aq, w0_aq[1]) > 0, ns0_aq[1] - fun_ns(cage_size_aq, dens_aq, w0_aq[1]), 0)
      n0_aq[1] = nstart - nm0_aq[1] - nt0_aq[1]
      rt0_aq[1] = nt0_aq[1] * w0_aq[1] * f_z * g_z # Fix placeholder variable names.
      y0_aq[1] = w0_aq[1] * n0_aq[1] * by1 * by2
      p0_aq[1] = p_mat[1, round(a0_aq[1] + 0.5)] * 1000 # Conversion for price in grams to revenue from kilograms of dry maw.
      rmaw0_aq[1] = y0_aq[1] * n0_aq[1] * p0_aq[1]
      rround0_aq[1] = w0_aq[1] * f_z * g_z # Fix placeholder variable names.
      r0_aq[1] = (rmaw0_aq[1] + rround0_aq[1])
      c0_aq[1] = n0_aq[1] * w0_aq[1] * h_z * j_z * 365 + k_z # Fix placeholder variable names.
      
      #   Led.
      a1_aq[1] = a0_aq[1] + 1
      l1_aq[1] = fun_a_l(a1_aq[1], linf_al_aq, k_al_aq, t0_al_aq)
      w1_aq[1] = fun_l_w(a_lw, l1_aq[1], b_lw)
      # Since this implementation of mortality/survival and trimming require iteration to work, the corresponding lead variables are spaghetti.
      nm1_aq[1] = (nstart * (1 - 0.01 * fun_a_aqmort(a1_aq[1] - 1, b1_mort_aq, b2_mort_aq, mmin_aq)) - 
                  ifelse(nstart * (1 - 0.01 * fun_a_aqmort(a1_aq[1] - 1, b1_mort_aq, b2_mort_aq, mmin_aq) - fun_ns(cage_size_aq, dens_aq, w1_aq[1])) > 0, 
                         nstart * (1 - 0.01 * fun_a_aqmort(a1_aq[1] - 1, b1_mort_aq, b2_mort_aq, mmin_aq) - fun_ns(cage_size_aq, dens_aq, w1_aq[1])), 
                         0)) * (0.01 * fun_a_aqmort(a1_aq[1], b1_mort_aq, b2_mort_aq, mmin_aq))
      ns1_aq[1] = (nstart * (1 - 0.01 * fun_a_aqmort(a1_aq[1] - 1, b1_mort_aq, b2_mort_aq, mmin_aq)) - 
                  ifelse(nstart * (1 - 0.01 * fun_a_aqmort(a1_aq[1] - 1, b1_mort_aq, b2_mort_aq, mmin_aq) - fun_ns(cage_size_aq, dens_aq, w1_aq[1])) > 0, 
                         nstart * (1 - 0.01 * fun_a_aqmort(a1_aq[1] - 1, b1_mort_aq, b2_mort_aq, mmin_aq) - fun_ns(cage_size_aq, dens_aq, w1_aq[1])), 
                         0)) * (1 - 0.01 * fun_a_aqmort(a1_aq[1], b1_mort_aq, b2_mort_aq, mmin_aq))
      nt1_aq[1] = ifelse(ns1_aq[1] - fun_ns(cage_size_aq, dens_aq, w1_aq[1]), ns1_aq[1] - fun_ns(cage_size_aq, dens_aq, w1_aq[1]), 0)
      n1_aq[1] = fun_ns(cage_size_aq, dens_aq, w1_aq[1])
      rt1_aq[1] = nt1_aq[1] * w1_aq[1] * f_z * g_z # Fix placeholder variable names.
      y1_aq[1] = w1_aq[1] * n1_aq[1] * by1 * by2 # Fix placeholder variable names.
      p1_aq[1] = p_mat[1, round(a1_aq[1] + 0.5)] * 1000 # Conversion for price in grams to revenue from kilograms of dry maw.
      rmaw1_aq[1] = y1_aq[1] * p1_aq[1]
      rround1_aq[1] = w1_aq[1] * f_z * g_z
      r1_aq[1] = (rmaw1_aq[1] + rround1_aq[1])
      c1_aq[1] = n1_aq[1] * w1_aq[1] * h_z * j_z * 365 + k_z # Fix placeholder variable names.
      
      h_aq[1] = 0
      hinv_aq[1] = 1
      r_aq[1] = r0_aq[1] * h_aq[1]# + rt0_aq[1] * hinv_aq[1]
      c_aq[1] = c0_aq[1] * hinv_aq[1]# + l_z * nstart * h_aq[1]
      
      # Add iterations.
      for(i in 2:(t_i - t_0 + 1)){
        for(j in 2:(a_i - a_0 + 1)){
          # Fishery.
          #  Numbers for time i and cohort j are numbers of the previous time and cohort less mortalities of the previous time and cohort.   
          n[i, j] = ifelse(n[i - 1, j - 1] - m[i - 1, j - 1] - b[i - 1, j - 1] - y[i - 1, j - 1] > 0, 
                           n[i - 1, j - 1] - m[i - 1, j - 1] - b[i - 1, j - 1] - y[i - 1, j - 1],
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
        e[i] = e[i - 1] + eta * (r_fi[i - 1] - c_fi[i - 1])
        
        #  Catches from effort and the rest.
        for(j in 2:(a_i - a_0 + 1)){
          y[i, j] = ifelse((n[i, j] - m[i, j] - b[i, j]) * q * e[i] * fun_l_s(fun_a_l(a_matrix[i, j], linf_al, k_al, t0_al), a_ls, b_ls, m_ls) > 0,
                           (n[i, j] - m[i, j] - b[i, j]) * q * e[i] * fun_l_s(fun_a_l(a_matrix[i, j], linf_al, k_al, t0_al), a_ls, b_ls, m_ls),
                            0)
          y[i, 1] = ifelse((n[i, 1] - m[i, 1] - b[i, 1]) * q * e[i] * fun_l_s(fun_a_l(a_matrix[i, 1], linf_al, k_al, t0_al), a_ls, b_ls, m_ls) > 0,
                           (n[i, 1] - m[i, 1] - b[i, 1]) * q * e[i] * fun_l_s(fun_a_l(a_matrix[i, 1], linf_al, k_al, t0_al), a_ls, b_ls, m_ls),
                            0)
        }
        
        #  Recruitment for time i.
        rec[i] = fun_rec(sum(n[i, 2:(a_i - a_0 + 1)]), a_r, b_r, d_r)
        
        # Aquaculture.
        a0_aq[i] = a0_aq[i - 1] * hinv_aq[i - 1] + 1
        a1_aq[i] = a0_aq[i] + 1
        
        l0_aq[i] = fun_a_l(a0_aq[i], linf_al_aq, k_al_aq, t0_al_aq)
        w0_aq[i] = fun_l_w(a_lw, l0_aq[i], b_lw)
        nm0_aq[i] = (nstart * h_aq[i - 1] + n0_aq[i - 1] * hinv_aq[i - 1]) * (0.01 * fun_a_aqmort(a0_aq[i], b1_mort_aq, b2_mort_aq, mmin_aq)) # Note leading mortality.
        ns0_aq[i] = (nstart * h_aq[i - 1] + n0_aq[i - 1] * hinv_aq[i - 1]) * (1 - 0.01 * fun_a_aqmort(a0_aq[i], b1_mort_aq, b2_mort_aq, mmin_aq)) # Note leading mortality.
        nt0_aq[i] = ifelse(ns0_aq[i] - fun_ns(cage_size_aq, dens_aq, w0_aq[i]) > 0, ns0_aq[i] - fun_ns(cage_size_aq, dens_aq, w0_aq[i]), 0)
        n0_aq[i] = (nstart * h_aq[i - 1] + n0_aq[i - 1] * hinv_aq[i - 1]) - nm0_aq[i] - nt0_aq[i]
        rt0_aq[i] = nt0_aq[i] * w0_aq[i] * f_z * g_z # Fix placeholder variable names.
        y0_aq[i] = w0_aq[i] * n0_aq[i] * by1 * by2
        p0_aq[i] = p_mat[i - 1, a0_aq[i]] * 1000 # Conversion for price in grams to revenue from kilograms of dry maw.
        rmaw0_aq[i] = y0_aq[i] * p0_aq[i]
        rround0_aq[i] = w0_aq[i] * f_z * g_z # Fix placeholder variable names.
        r0_aq[i] = (rmaw0_aq[i] + rround0_aq[i])
        c0_aq[i] = n0_aq[1] * w0_aq[1] * h_z * j_z * 365 + k_z # Fix placeholder variable names.
        
        l1_aq[i] = fun_a_l(a1_aq[i], linf_al_aq, k_al_aq, t0_al_aq)
        w1_aq[i] = fun_l_w(a_lw, l1_aq[i], b_lw)
        nm1_aq[i] = (nstart * h_aq[i - 1] + n1_aq[i - 1] * hinv_aq[i - 1]) * (0.01 * fun_a_aqmort(a1_aq[i], b1_mort_aq, b2_mort_aq, mmin_aq)) # Note leading mortality.
        ns1_aq[i] = (nstart * h_aq[i - 1] + n1_aq[i - 1] * hinv_aq[i - 1]) * (1 - 0.01 * fun_a_aqmort(a1_aq[i], b1_mort_aq, b2_mort_aq, mmin_aq)) # Note leading mortality.
        nt1_aq[i] = ifelse(ns1_aq[i] - fun_ns(cage_size_aq, dens_aq, w1_aq[i]) > 0, ns1_aq[i] - fun_ns(cage_size_aq, dens_aq, w1_aq[i]), 0)
        n1_aq[i] = (nstart * h_aq[i - 1] + n1_aq[i - 1] * hinv_aq[i - 1]) - nm1_aq[i] - nt1_aq[i]
        rt1_aq[i] = nt1_aq[i] * w1_aq[i] * f_z * g_z # Fix placeholder variable names.
        y1_aq[i] = w1_aq[i] * n1_aq[i] * by1 * by2
        p1_aq[i] = p_mat[i - 1, a1_aq[i]] * 1000 # Conversion for price in grams to revenue from kilograms of dry maw.
        rmaw1_aq[i] = y1_aq[i] * p1_aq[i]
        rround1_aq[i] = w1_aq[i] * f_z * g_z # Fix placeholder variable names.
        r1_aq[i] = (rmaw1_aq[i] + rround1_aq[i])
        c1_aq[i] = n1_aq[1] * w1_aq[1] * h_z * j_z * 365 + k_z # Fix placeholder variable names.
        
        # Mind wrapper for hard-coding lower bound age at harvest.
        h_aq[i] = ifelse(a0_aq[i] > 0, ifelse((r0_aq[i] - l_z * nstart) > (disc_aq * (r1_aq[i] - c0_aq[i] + rt0_aq[i])), 1, 0), 0) # Think hard about lags here.
        hinv_aq[i] = (h_aq[i] - 1) ^ 2
        
        r_aq[i] = (r0_aq[i] + rt0_aq[i] * hinv_aq[i])
        c_aq[i] = c0_aq[i] * hinv_aq[i] + l_z * nstart * h_aq[i]
        
        # Prices in matrix. Use this one. Think harder about the lag problem.
        for(j in 1:(a_i - a_0 + 1)){
          p_mat[i, j] = fun_p(sum(
                                  fun_l_w(a_lw, fun_a_l(a_matrix[i, ], linf_al, k_al, t0_al), b_lw) * y[i, ] * by1 * by2 + # Fishery production.
                                  switch_aq * 50 * (y0_aq[i] * by1 * by2 * h_aq[i] + nt0_aq[i] * w0_aq[i] * by1 * by2 * hinv_aq[i]) # Aquaculture production.
                                  ) 
                                  / 1000,
                              fun_l_w(a_lw, fun_a_l(a_matrix[i, j], linf_al, k_al, t0_al), b_lw) * by1 * by2 * 1000, 
                              a_ma, b_ma, c_ma) * loss
        }
        
        # Revenues.
        r_fi[i] = sum(p_mat[i,] * fun_l_w(a_lw, fun_a_l(a_matrix[i, ], linf_al, k_al, t0_al), b_lw) * y[i, ] * by1 * by2 * 1000) # Constant for conversion to grams of buche.
        
        # Costs.
        c_fi[i] = e[i] * c_2017
      }
      
      # Tidy results: years, ages, and values into a dataframe. Work in efforts, revenues, costs, profits, and biomass sometime.
      # HEY PIPE THIS %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% 
      #  Numbers.
      tidyn = melt(n)
      tidyn$var = "Numbers"
      #  Catches.
      tidyy = melt(y)
      tidyy$var = "Catches"
      #  Poaching Effort.
      tidye = rename(data.frame(matrix(NA, nrow = t_i - t_0 + 1, ncol = 4)), Var1 = X1, Var2 = X2, value = X3, var = X4)
      tidye$Var1 = seq(1, t_i - t_0 + 1)
      tidye$Var2 = NA
      tidye$value = e
      tidye$var = "Effort"
      #tidye = select(tidye, tidye$Var1, tidye$Var2, var)
      #  Poaching Revenue.
      tidyr = rename(data.frame(matrix(NA, nrow = t_i - t_0 + 1, ncol = 4)), Var1 = X1, Var2 = X2, value = X3, var = X4)
      tidyr$Var1 = seq(1, t_i - t_0 + 1)
      tidyr$Var2 = NA
      tidyr$value = r_fi
      tidyr$var = "Revenue"
      #  Poaching Cost.
      tidyc = rename(data.frame(matrix(NA, nrow = t_i - t_0 + 1, ncol = 4)), Var1 = X1, Var2 = X2, value = X3, var = X4)
      tidyc$Var1 = seq(1, t_i - t_0 + 1)
      tidyc$Var2 = NA
      tidyc$value = c_fi
      tidyc$var = "Cost"
      #  Everything!
      tidy = bind_rows(tidyn, tidyy, tidye, tidyr, tidyc)
      tidy$group = ifelse(tidy$Var2 < a_mat_am, "Machorro", ifelse(tidy$Var2 < a_old_am, "Pre-Adulto", "Adulto"))
      tidy = rename(tidy, Year = Var1, Age = Var2, Result = value, Variable = var, Group = group)
      
      # Get results.
      return(tidy)
      
    }
