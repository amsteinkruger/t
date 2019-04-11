# ---- vis ----

# Set-up.
library(ggpubr)
library(gridExtra)
library(showtext)

# Load Avenir from an open text file in the working directory.
#  real quick heads-up: package showtext works for certain graphic devices so that saved files display Avenir but previews in RStudio do not.
#  This is fixable and I do not know how.
font_paths(".")
font_add("avenir", "avenir.otf")
showtext_auto()

# Set up theme elements for convenient font sizing.
bigfont = theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20))
lilfont = theme(axis.text = element_text(size = 8), axis.title = element_text(size = 10))

# Stock and catch of reproductive biomass in numbers and mass.
results_sum = filter(results, Age > 3) %>%
  mutate(Biomass = fun_l_w(pars$default[4], fun_a_l(Age - 0.5, pars$default[1], pars$default[2], pars$default[3]),  pars$default[5]) / 1000 * Result) %>% 
  group_by(Year, Variable, Run, Scenario, Estimate) %>% # Run, 
  summarize(SumNum = sum(Result), SumBio = sum(Biomass)) %>% 
  mutate(LogNum = log(SumNum + 1), LogBio = log(SumBio + 1)) %>% 
  ungroup() %>% 
  mutate(Run = as.factor(Run)) %>% 
  unite("Estimate | Scenario", Estimate, Scenario, sep = " | ", remove = FALSE)

# Plot the summary numbers.
#  Check whether inputs are tonnes or numbers.
plot_nfig = 
  ggplot(filter(results_sum, Variable == "Numbers")) +
  geom_hline(yintercept = 25108, linetype = "dotted", size = 0.95) +
  geom_hline(yintercept = 10044, linetype = "dotted", size = 0.95) +
  geom_line(aes(x = Year + 2016, y = SumBio, group = Run, color = Scenario, linetype = Estimate), size = 1.25) +
  scale_color_manual(values = c("#04859B", "#003660")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  #scale_x_continuous(expand = c(0, 0), labels = scales::comma) +  
  labs(x = "Year", y = "Tonnes of Biomass") +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none")

plot_nfig_big = plot_nfig + bigfont
plot_nfig_lil = plot_nfig + lilfont

ggsave("plot_nfig_5x225.png", 
       plot_nfig_lil, 
       width = 2.5, 
       height = 2.25,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("plot_nfig_10x7.png", 
       plot_nfig_big, 
       width = 10, 
       height = 7,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("plot_nfig_25x225.png", 
       plot_nfig_lil, 
       width = 2.5, 
       height = 2.25,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("plot_nfig_5x7.png", 
       plot_nfig_big, 
       width = 5, 
       height = 7,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

# Data wrangling for a cleaner dataframe.
results_sum_pi = results %>% 
  filter(Estimate == "Central" & Variable == "Poaching Revenue" |
           Estimate == "Central" & Variable == "Poaching Cost" |
           Estimate == "Central" & Variable == "Poaching Profit")

# Profits for in both scenarios, sort of.
plot_pi = 
  ggplot(filter(results_sum_pi, Variable == "Poaching Profit")) +
  geom_col(aes(x = Year, y = Result, fill = Variable), position = "dodge") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 10), labels = scales::comma) +  
  labs(x = "Year", y = "Fat Stacks") +
  theme_classic()

print(plot_pi)

# Revenues and costs for both sectors in both scenarios.
#plot_rc =
#  ggplot(filter(results_sum_pi))

#ggsave("plot_pi.png", plot_pi, dpi = 300, width = 6.5, height = 6.5)

# Revenues and costs for the central estimate.
plot_rc = 
  ggplot(filter(results_sum_pi, Variable == "Poaching Revenue" | Variable == "Poaching Cost")) +
  geom_col(aes(x = Year, y = Result, fill = Variable), position = "dodge") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 10), labels = scales::comma) +  
  labs(x = "Year", y = "Fat Stacks") +
  theme_classic()

#ggsave("plot_rc.png", plot_rc, dpi = 300, width = 6.5, height = 6.5)

# Goto's Bit

# Wrangle effort.
results_e_sum = filter(results_e, Variable == "Effort") %>% 
  group_by(Run) %>% 
  summarize(mean = mean(Result)) %>% 
  ungroup() %>% 
  mutate("Annual Tonnes" = Run - 1) %>% 
  rename("Mean Effort" = mean) %>% 
  mutate("25% Reduction" = (`Mean Effort`[1] * 0.75)) %>% 
  mutate("50% Reduction" = (`Mean Effort`[1] * 0.50)) %>% 
  mutate("90% Reduction" = (`Mean Effort`[1] * 0.10))  


# Plot efforts.
plot_earb_big = 
  ggplot(results_e_sum, aes(`Annual Tonnes`, `Mean Effort`)) +
  geom_line(aes(`Annual Tonnes`, `25% Reduction`), linetype = "dashed", color = "red") +
  geom_line(aes( `Annual Tonnes`, `50% Reduction`), linetype = "dashed", color = "red") +
  geom_line(aes( `Annual Tonnes`, `90% Reduction`), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 169.3064, linetype = "dashed", color = "blue") +
  geom_line(color = "black", size = 1) +
  annotate("text", x = 8, y = 123, label = "25%", size = 5, family = "avenir") +
  annotate("text", x = 8, y = 80, label = "50%", size = 5, family = "avenir") +
  annotate("text", x = 8, y = 10, label = "90%", size = 5, family = "avenir") +
  annotate("text", x = 8, y = 163, label = "3%", size = 5, family = "avenir") +
  ylim(0, 200) +
  scale_x_discrete(expand = c(0, 0), limits = c(0, 15)) + 
  #scale_y_continuous(expand = c(0, 0))+
  labs(x = "Tonnes of Dry Buche", y = "Effort (Boat Years)") +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") +
  bigfont

plot_earb_lil = 
  ggplot(results_e_sum, aes(`Annual Tonnes`, `Mean Effort`)) +
  geom_line(aes(`Annual Tonnes`, `25% Reduction`), linetype = "dashed", color = "red") +
  geom_line(aes( `Annual Tonnes`, `50% Reduction`), linetype = "dashed", color = "red") +
  geom_line(aes( `Annual Tonnes`, `90% Reduction`), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 169.3064, linetype = "dashed", color = "blue") +
  geom_line(color = "black", size = 1) +
  annotate("text", x = 8, y = 123, label = "25%", size = 2, family = "avenir") +
  annotate("text", x = 8, y = 80, label = "50%", size = 2, family = "avenir") +
  annotate("text", x = 8, y = 10, label = "90%", size = 2, family = "avenir") +
  annotate("text", x = 8, y = 163, label = "3%", size = 2, family = "avenir") +
  ylim(0, 200) +
  scale_x_discrete(expand = c(0, 0), limits = c(0, 15)) + 
  #scale_y_continuous(expand = c(0, 0))+
  labs(x = "Tonnes of Dry Buche", y = "Effort (Boat Years)") +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") +
  lilfont

# Plot prices.
results_p_sum = filter(results_e, Variable == "Price") %>% 
  group_by(Run) %>% 
  summarize(mean = mean(Result)) %>% 
  ungroup() %>% 
  mutate("Annual Tonnes" = Run - 1)  %>% 
  rename("Mean Price" = mean) %>% 
  mutate("25% Reduction" = (`Mean Price`[1] * 0.75)) %>% 
  mutate("50% Reduction" = (`Mean Price`[1] * 0.50)) %>% 
  mutate("90% Reduction" = (`Mean Price`[1] * 0.10)) 

plot_parb_big = 
  ggplot(results_p_sum, aes(`Annual Tonnes`, `Mean Price`)) +
  geom_line(aes(`Annual Tonnes`, `25% Reduction`), linetype = "dashed", color = "red") +
  geom_line(aes( `Annual Tonnes`, `50% Reduction`), linetype = "dashed", color = "red") +
  geom_line(aes( `Annual Tonnes`, `90% Reduction`), linetype = "dashed", color = "red") +
  geom_line(color = "black", size = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 5.776472, linetype = "dashed", color = "blue")+
  annotate("text", x = 8, y = 5.5, label = "6%", size = 5, family = "avenir")+
  annotate("text", x = 8, y = 4.3, label = "25%", size = 5, family = "avenir") +
  annotate("text", x = 8, y = 2.8, label = "50%", size = 5, family = "avenir") +
  annotate("text", x = 8, y = 0.4, label = "90%", size = 5, family = "avenir") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 16), breaks = c(0, 15)) +
  scale_y_discrete(limits = c(0, 10)) +
  labs(x = "Tonnes of Dry Buche", y = "Price per Gram (USD2018)") +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") +
  bigfont

plot_parb_lil = 
  ggplot(results_p_sum, aes(`Annual Tonnes`, `Mean Price`)) +
  geom_line(aes(`Annual Tonnes`, `25% Reduction`), linetype = "dashed", color = "red") +
  geom_line(aes( `Annual Tonnes`, `50% Reduction`), linetype = "dashed", color = "red") +
  geom_line(aes( `Annual Tonnes`, `90% Reduction`), linetype = "dashed", color = "red") +
  geom_line(color = "black", size = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 5.776472, linetype = "dashed", color = "blue")+
  annotate("text", x = 8, y = 5.5, label = "6%", size = 2, family = "avenir")+
  annotate("text", x = 8, y = 4.3, label = "25%", size = 2, family = "avenir") +
  annotate("text", x = 8, y = 2.8, label = "50%", size = 2, family = "avenir") +
  annotate("text", x = 8, y = 0.4, label = "90%", size = 2, family = "avenir") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 16), breaks = c(0, 15)) +
  scale_y_discrete(limits = c(0, 10)) +
  labs(x = "Tonnes of Dry Buche", y = "Price per Gram (USD2018)") +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") +
  lilfont

#output figure
#ggarrange(plot_parb, plot_earb,
#          ncol = 2, nrow = 1)

ggsave("earb_25x225.png",
       plot_earb_lil,
       width = 2.5,
       height = 2.25,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("earb_5x7.png",
       plot_earb_big,
       width = 5,
       height = 7,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("parb_25x225.png",
       plot_parb_lil,
       width = 2.5,
       height = 2.25,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("parb_5x7.png",
       plot_parb_big,
       width = 5,
       height = 7,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

# Suddenly, a Kobe plot!
# Read in data prepared in Excel.
dat = read_csv("tma_kobe.csv")

# Mutate in the mean of the two higher f-ratio estimates for later.
dat = mutate(dat, fnormmid = (fnorm75 + fnorm50) / 2)
# Rejigger data for time series plotting.
datmelt = melt(dat, id = 1)

# Plot middle value as a line in a Kobe format with polygons for each 'zone' of ratios.
kobe =
  ggplot() +
  # Green.
  geom_rect(aes(xmin = 0.5, ymin = 0, xmax= 2, ymax = 1), fill = "#7A8D39") +
  # Yellow | Upper / Right
  geom_rect(aes(xmin = 0.5, ymin = 1, xmax= 2, ymax = 2.25), fill = "#FEBC11") + 
  # Yellow | Lower / Left
  geom_rect(aes(xmin = 0, ymin = 0, xmax= 1, ymax = 1), fill = "#FEBC11") + 
  # Red.
  geom_rect(aes(xmin = 0, ymin = 1, xmax= 1, ymax = 2.25), fill = "#EF5645") +
  geom_point(data = dat, aes(brat, fnormmid)) +#, colour = y)) +
  geom_path(data = dat, aes(brat, fnormmid), alpha = 0.5, size = 0.8) +#, colour = y)) +
  geom_text(data = filter(dat, y == 1925), aes(brat, fnormmid, label = y),hjust = 0, vjust = -0.25) +
  geom_text(data = filter(dat, y == 1942), aes(brat, fnormmid, label = y),hjust= 0, vjust = -0.25) +
  geom_text(data = filter(dat, y == 1966), aes(brat, fnormmid, label = y),hjust= 0, vjust = -0.25) +
  geom_text(data = filter(dat, y == 2017), aes(brat, fnormmid, label = y),hjust= 1, vjust = -0.25) +
  scale_colour_gradient(low = "gray50", high = "gray0") +
  scale_x_continuous(limits = c(0, 2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 2.25), expand = c(0, 0)) +
  xlab("Biomass Ratio") +
  ylab("Fishing Mortality Ratio") +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none")

kobelil = kobe + lilfont
kobebig = kobe + bigfont

# Save.
ggsave("kobelil.png",
       kobelil,
       width = 5,
       height = 2.25,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("kobebig.png",
       kobebig,
       width = 10,
       height = 7,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

# And now, your market graph(s).
mar_mut = mutate(ma_dat, hat = fun_p(q, g, nlm_tidy[[1, 2]], nlm_tidy[[2, 2]], nlm_tidy[[3, 2]]))

mar_g =
  ggplot(mar_mut) +
  geom_point(aes(g, p), colour = "#04859B") +
  geom_segment(aes(x = g, y = p, xend = g, yend = hat), colour = "#04859B") +
  geom_point(aes(g, hat), colour = "#003660") +
  labs(x = "Grams of Dry Buche", y = "US$2018 / Gram") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0, 100), expand = c(0, 0)) +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none")

mar_q = 
  ggplot(mar_mut) +
  geom_boxplot(aes(as.factor(q), hat), colour = "#003660", fill = "transparent") +
  geom_jitter(aes(as.factor(q), p), colour = "#04859B") +
  labs(x = "Tonnes of Dry Buche at Market", y = "US$2018 / Gram") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0, 100), expand = c(0, 0)) +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none")

mar_g_big = mar_g + bigfont
mar_g_lil = mar_g + lilfont
mar_q_big = mar_q + bigfont
mar_q_lil = mar_q + lilfont

ggsave("mar_g_lil.png",
       mar_g_lil,
       width = 2.5,
       height = 2.25,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("mar_g_big.png",
       mar_g_big,
       width = 5,
       height = 7,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("mar_q_lil.png",
       mar_q_lil,
       width = 2.5,
       height = 2.25,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("mar_q_big.png",
       mar_q_big,
       width = 5,
       height = 7,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")