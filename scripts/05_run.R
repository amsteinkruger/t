# House results.
results_0 = vector("list", n)
results_1 = vector("list", n)
results_2 = vector("list", n)
results_3 = vector("list", n)

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
                    output$Scenario = "Aquaculture Intervention" # Band-Aid: This would be better outside of the loop.
                    output$Cages = par["c_cages",] # Band-Aid: This carries one parameter through, but compact code to carry all through would be nice.
                    results_1[[i]] = output}

for(i in 1:n){par = select(pars_2, i)
                    output = fun(par)
                    output$Run = i
                    output$Scenario = "Enforcement Intervention" # Band-Aid: This would be better outside of the loop.
                    output$Cages = par["c_cages",] # Band-Aid: This carries one parameter through, but compact code to carry all through would be nice.
                    results_2[[i]] = output}

for(i in 1:n){par = select(pars_3, i)
                    output = fun(par)
                    output$Run = i
                    output$Scenario = "Aquaculture and Enforcement Interventions" # Band-Aid: This would be better outside of the loop.
                    output$Cages = par["c_cages",] # Band-Aid: This carries one parameter through, but compact code to carry all through would be nice.
                    results_3[[i]] = output}

# Go from list to dataframe for easier processing.
results = bind_rows(results_0,
                    results_1,
                    results_2,
                    results_3)