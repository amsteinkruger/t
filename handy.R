# Run out a one-off set of results for one cage with fixed prices, etc.
#results_handy = filter(results, Variable == "Aquaculture Profit" | Variable == "taq" | Variable == "naq" | Variable == "waq")
#results_handy = filter(results_handy, Estimate == "Central", Scenario == "w/o Aquaculture")
#results_handy = select(results_handy, Year, Result, Variable)
#write.csv(results_handy, "results_handy.csv")

# without fixing prices + miscellaneous other tweaks:
results_handy_dat = read_csv("results_handy.csv")

plot_handy = 
  ggplot(results_handy_dat) +
  geom_col(aes(Year, Result, fill = Variable)) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~Variable) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "") +
  theme_classic()

#print(plot_handy)