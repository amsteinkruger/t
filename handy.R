# Run out a one-off set of results for one cage with fixed prices, etc.

library(pubr)
library(gridExtra)
library(showtext)

# Load Avenir from an open text file in the working directory.
#  real quick heads-up: package showtext works for certain graphic devices so that saved files display Avenir but previews in RStudio do not.
#  This is fixable and I do not know how.
font_paths(".")
font_add("avenir", "avenir.otf")

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
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))

plot_handy_pi = 
  ggplot(filter(results_handy, Plot == "Profit")) +
  geom_col(aes(Year, Result, fill = as.factor(Profit))) + 
  scale_fill_manual(values = c("#EF5645", "#0BA89A")) +
  scale_y_continuous(labels = scales::comma, expand = c(0.1, 0.1), breaks = c(-2.5, 0, 2.5, 5, 7.5, 10)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "Millions US$") +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.position = "none")

# Create a handy theming object for font sizes.
bigfont = theme(axis.text = element_text(size = 36), axis.title = element_text(size = 54))
lilfont = theme(axis.text = element_text(size = 18), axis.title = element_text(size = 27))

plot_handy_prod_lil = plot_handy_prod + lilfont
plot_handy_pi_lil = plot_handy_pi + lilfont
plot_handy_prod_big = plot_handy_prod + bigfont
plot_handy_pi_big = plot_handy_pi + bigfont

#output figure
#ggarrange(plot_handy_prod, plot_handy_pi,
#          ncol = 2, nrow = 1)

ggsave("handyprod_25x225.png",
       plot_handy_prod_lil,
       width = 2.4,
       height = 2.75,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("handypi_25x225.png",
       plot_handy_pi_lil,
       width = 2.4,
       height = 2.75,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("handyprod_5x7.png",
       plot_handy_prod_big,
       width = 5,
       height = 3.7125,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("handypi_5x7.png",
       plot_handy_pi_big,
       width = 5,
       height = 3.7125,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")
