# ---- run ----
# Build a home for results of runs.
results = list()

# Loop through parameter sets.
for(i in 7:n){par = select(pars, i)
              output = fun(par)
              output$Run = i
              output$Scenario = ifelse(output$Run < (7 + n / 2), "w/o Aquaculture", "w/ Aquaculture") # Band-Aid.
              results[[i]] = output}

# Go from list to dataframe for easier processing.
results = bind_rows(results)