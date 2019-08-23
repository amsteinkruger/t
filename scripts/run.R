# ---- run ----

# Clock model runtime.
watch_go = proc.time()

# Build a home for results of runs.
results_0 = vector("list", n)
results_1 = vector("list", n)

# Loop through parameter sets.
for(i in 1:n){par = select(pars_0, i)
                    output = fun(par)
                    output$Run = i
                    output$Scenario = "Status Quo" # Band-Aid: This would be better outside of the loop.
                    output$Cages = par["c_cages",] # Band-Aid: This carries one parameter through, but compact code to carry all through would be nice.
                    results_0[[i]] = output}

for(i in 1:n){par = select(pars_1, i)
                    output = fun(par)
                    output$Run = i
                    output$Scenario = "Counterfactual" # Band-Aid: This would be better outside of the loop.
                    output$Cages = par["c_cages",] # Band-Aid: This carries one parameter through, but compact code to carry all through would be nice.
                    results_1[[i]] = output}

# Go from list to dataframe for easier processing.
results = bind_rows(results_0, results_1)

# Clock model runtime.
watch_stop = proc.time() - watch_go

# Just how slow is this loop?
print(watch_stop) # 105.3833m, n = 10000?, 2019/7/3.