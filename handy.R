
results_handy = filter(results, Variable == "Aquaculture Profit" | Variable == "taq" | Variable == "naq" | Variable == "waq")
results_handy = filter(results_handy, Estimate == "Central", Scenario == "w/o Aquaculture")
results_handy = select(results_handy, Year, Result, Variable)
write.csv(results_handy, "results_handy.csv")


