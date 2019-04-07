# Run out a one-off set of results for one cage with fixed prices, etc.

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

# Quick rerun.
results_handy = read_csv("results_handy_but_really.csv")

results_handy = results_handy %>% 
  mutate(Profit = ifelse(Plot == "Profit" & Result > 0, 1, ifelse(Plot == "Profit", 0, NA))) %>% 
  mutate(Result = ifelse(Plot == "Profit", Result * 0.000001, Result))

plot_handy_prod = 
  ggplot(filter(results_handy, Plot == "Yield" & Product == "Meat")) +
  geom_col(aes(Year, Result), fill = "#04859B") +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0, 250), breaks = c(0, 50, 100, 150, 200, 250)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "Production (Tonnes)") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),plot.title = element_text(hjust=1, vjust=1, face = 'bold'),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))

plot_handy_pi = 
  ggplot(filter(results_handy, Plot == "Profit")) +
  geom_col(aes(Year, Result, fill = as.factor(Profit))) + 
  scale_fill_manual(values = c("#EF5645", "#0BA89A")) +
  scale_y_continuous(labels = scales::comma, expand = c(0.1, 0.1), breaks = c(-2.5, 0, 2.5, 5, 7.5, 10)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "Profit (Millions USD2018)") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),plot.title = element_text(hjust=1, vjust=1, face = 'bold'),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.position="none")

#output figure
ggarrange(plot_handy_prod, plot_handy_pi,
          ncol = 2, nrow = 1)

ggsave("handy_5x225.png",
       width = 5,
       height = 2.25,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("handy_10x7.png",
       width = 10,
       height = 7,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")