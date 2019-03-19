# Run out a one-off set of results for one cage with fixed prices, etc.
#results_handy = filter(results, Variable == "Aquaculture Profit" | Variable == "taq" | Variable == "naq" | Variable == "waq")
#results_handy = filter(results_handy, Estimate == "Central", Scenario == "w/o Aquaculture")
#results_handy = select(results_handy, Year, Result, Variable)
#write.csv(results_handy, "results_handy.csv")

# without fixing prices + miscellaneous other tweaks:

#library(pubr)
library(gridExtra)

results_output <- read_csv("results_output.csv")
results_profit <- read_csv("results_profit.csv")


plot_output = 
  ggplot(results_output) +
  geom_col(aes(Year, Result), fill = "red") +  #fill = Variable
  scale_fill_brewer(palette = "Set1") +
  #facet_wrap(~Variable) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "Kilograms") +
  theme_classic()

#print(plot_handy)

plot_output

plot_profit = 
  ggplot(results_profit) +
  geom_col(aes(Year, Result), fill = "blue") + #fill = Variable
  scale_fill_brewer(palette = "Set1") +
  #facet_wrap(~Variable) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "USD 2018") +
  theme_classic()

plot_profit


#output figure
ggarrange(plot_outfit, plot_profit,
          ncol = 2, nrow = 1)

ggsave("output_profit.png",
       width = 6.6,
       height = 3,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")
