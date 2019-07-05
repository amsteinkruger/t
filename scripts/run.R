# ---- run ----

# Clock model runtime.
watch_go = proc.time()

# Build a home for results of runs.
results = vector("list", n)

# Loop through parameter sets.
for(i in 7:n){par = select(pars, i)
              output = fun(par)
              output$Run = i
              output$Scenario = ifelse(output$Run < (7 + n / 2), "w/o Aquaculture", "w/ Aquaculture") # Band-Aid.
              results[[i]] = output}

# Go from list to dataframe for easier processing.
results = bind_rows(results)

# Clock model runtime.
watch_stop = proc.time() - watch_go

# Just how slow is this loop?
print(watch_stop) # 105.3833m, 2019/7/3.