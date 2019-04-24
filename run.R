# ---- run ----
# Build a home for results of runs.
results = list()

# Loop through parameter sets.
for(i in 1:12){par = select(pars, i)
              output = fun(par)
              output$Run = i
              output$Scenario = ifelse(output$Run < 4 | output$Run < 10 & output$Run > 6, "w/ Aquaculture", "w/o Aquaculture")
              output$Estimate = ifelse(output$Run == 1 | output$Run == 4 | output$Run == 7 | output$Run == 10, "Central", "Outer")
              results[[i]] = output}

# Go from list to dataframe for easier processing.
results = bind_rows(results)

# Build a home for results of runs with effort.
results_e = list()

# Loop through parameter sets for effort and price reduction.
for(i in 1:length(pars_arb)){par = select(pars_arb, i)
                             output = fun(par)
                             output$Run = i
                             results_e[[i]] = output}

# Go from list to dataframe for easier processing.
results_e = bind_rows(results_e)