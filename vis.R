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
bigfont = theme(axis.text = element_text(size = 36), axis.title = element_text(size = 54))
lilfont = theme(axis.text = element_text(size = 18), axis.title = element_text(size = 27))

# Stock and catch of reproductive biomass in numbers and mass.
results_sum =  results %>% #filter(results, Age > 3)
  mutate(Biomass = fun_l_w(pars$default[4], fun_a_l(Age - 0.5, pars$default[1], pars$default[2], pars$default[3]),  pars$default[5]) / 1000 * Result) %>% 
  group_by(Year, Variable, Run, Scenario) %>% # Run, #, Estimate
  summarize(SumNum = sum(Result), SumBio = sum(Biomass)) %>% 
  #mutate(LogNum = log(SumNum + 1), LogBio = log(SumBio + 1)) %>% 
  ungroup() %>% 
  mutate(Run = as.factor(Run)) #%>% 
 # unite("Estimate | Scenario", Estimate, Scenario, sep = " | ", remove = FALSE)

View(filter(results_sum, Year == "10", Run == 1 | Run == 4, Variable == "Catches" | Variable == "Numbers" | Variable == "Effort"))

# Plot the summary numbers.
#  Check whether inputs are tonnes or numbers.
plot_nfig_big = 
  ggplot(filter(results_sum, Variable == "Numbers")) + # & Estimate == "Central"
  geom_hline(yintercept = 12136, size = 0.95, color = "#EF5645") +
  geom_line(aes(x = Year + 2016, y = SumBio * 0.33, group = Run), size = 1.15, alpha = 0.0025) + #, linetype = Estimate #, color = Scenario
  scale_color_manual(values = c("#04859B", "#003660")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20000), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0.25)) +  
  labs(x = "Year", y = "Tonnes of Biomass") +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") +
  bigfont

plot_nfig_lil = 
  ggplot(filter(results_sum, Variable == "Numbers" & Estimate == "Central")) +
  geom_hline(yintercept = 25108, size = 0.5, color = "#7A8D39") +
  geom_hline(yintercept = 10044, size = 0.5, color = "#EF5645") +
  geom_line(aes(x = Year + 2016, y = SumBio, group = Run, color = Scenario, linetype = Estimate), size = 0.55) +
  scale_color_manual(values = c("#04859B", "#003660")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0.25)) +  
  labs(x = "Year", y = "Tonnes of Biomass") +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") +
  lilfont

ggsave("plot_nfig_5x225.png", 
       plot_nfig_lil, 
       width = 2.4, 
       height = 2.75,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("plot_nfig_10x7.png", 
       plot_nfig_big, 
       width = 7, 
       height = 4.6375,
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
  geom_line(aes(`Annual Tonnes`, `25% Reduction`), linetype = "dotted", color = "#EF5645", size = 1.15) +
  geom_line(aes( `Annual Tonnes`, `50% Reduction`), linetype = "dotted", color = "#EF5645", size = 1.15) +
  geom_line(aes( `Annual Tonnes`, `90% Reduction`), linetype = "dotted", color = "#EF5645", size = 1.15) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "#0BA89A", size = 1.15) +
  geom_hline(yintercept = 169.3064, linetype = "dotted", color = "#0BA89A", size = 1.15) +
  geom_line(color = "black", size = 1, size = 1.15) +
  annotate("text", x = 8, y = 123, label = "25%", size = 15, family = "avenir") +
  annotate("text", x = 8, y = 80, label = "50%", size = 15, family = "avenir") +
  annotate("text", x = 8, y = 10, label = "90%", size = 15, family = "avenir") +
  annotate("text", x = 8, y = 163, label = "4%", size = 15, family = "avenir") +
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
  geom_line(aes(`Annual Tonnes`, `25% Reduction`), linetype = "dotted", color = "#EF5645", size = 0.50) +
  geom_line(aes( `Annual Tonnes`, `50% Reduction`), linetype = "dotted", color = "#EF5645", size = 0.50) +
  geom_line(aes( `Annual Tonnes`, `90% Reduction`), linetype = "dotted", color = "#EF5645", size = 0.50) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "#0BA89A", size = 0.50) +
  geom_hline(yintercept = 169.3064, linetype = "dotted", color = "#0BA89A", size = 0.50) +
  geom_line(color = "black", size = 0.55) +
  annotate("text", x = 8, y = 123, label = "25%", size = 7, family = "avenir") +
  annotate("text", x = 8, y = 80, label = "50%", size = 7, family = "avenir") +
  annotate("text", x = 8, y = 10, label = "90%", size = 7, family = "avenir") +
  annotate("text", x = 8, y = 163, label = "4%", size = 7, family = "avenir") +
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
  labs(x = "Tonnes of Dry Buche", y = "Gram Price") +
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
  labs(x = "Tonnes of Dry Buche", y = "Gram Price") +
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
       width = 2.40,
       height = 2.75,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("earb_5x7.png",
       plot_earb_big,
       width = 7,
       height = 4.6375,
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
kobebig =
  ggplot() +
  # Green.
  geom_rect(aes(xmin = 1, ymin = 0, xmax= 2, ymax = 1), fill = "#7A8D39") +
  # Yellow | Upper / Right
  geom_rect(aes(xmin = 1, ymin = 1, xmax= 2, ymax = 2.25), fill = "#FEBC11") + 
  # Yellow | Lower / Left
  geom_rect(aes(xmin = 0, ymin = 0, xmax= 1, ymax = 1), fill = "#FEBC11") + 
  # Red.
  geom_rect(aes(xmin = 0, ymin = 1, xmax= 1, ymax = 2.25), fill = "#EF5645") +
  geom_point(data = dat, aes(brat, fnormmid), size = 2.5) +
  geom_path(data = dat, aes(brat, fnormmid), alpha = 0.5, size = 1.5) +#, colour = y)) +
  geom_text(data = filter(dat, y == 1925), aes(brat, fnormmid, label = y), hjust = 0, vjust = -0.5, size = 20) +
  geom_text(data = filter(dat, y == 1942), aes(brat, fnormmid, label = y), hjust = 0, vjust = -0.5, size = 20) +
  geom_text(data = filter(dat, y == 1966), aes(brat, fnormmid, label = y), hjust = 0, vjust = -0.5, size = 20) +
  geom_text(data = filter(dat, y == 2017), aes(brat, fnormmid, label = y), hjust = 1, vjust = -0.5, size = 20) +
  scale_colour_gradient(low = "gray50", high = "gray0") +
  scale_x_continuous(limits = c(0, 2.05), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 2.25), expand = c(0, 0)) +
  xlab("Biomass Ratio (B / BMSY)") +
  ylab("Fishing Mortality Ratio (F / FMSY)") +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") + 
  bigfont

kobelil =
  ggplot() +
  # Green.
  geom_rect(aes(xmin = 1, ymin = 0, xmax= 2, ymax = 1), fill = "#7A8D39") +
  # Yellow | Upper / Right
  geom_rect(aes(xmin = 1, ymin = 1, xmax= 2, ymax = 2.25), fill = "#FEBC11") + 
  # Yellow | Lower / Left
  geom_rect(aes(xmin = 0, ymin = 0, xmax= 1, ymax = 1), fill = "#FEBC11") + 
  # Red.
  geom_rect(aes(xmin = 0, ymin = 1, xmax= 1, ymax = 2.25), fill = "#EF5645") +
  geom_point(data = dat, aes(brat, fnormmid), size = 1) +
  geom_path(data = dat, aes(brat, fnormmid), alpha = 0.5, size = 0.75) +#, colour = y)) +
  geom_text(data = filter(dat, y == 1925), aes(brat, fnormmid, label = y), hjust = 0, vjust = -0.5, size = 7) +
  geom_text(data = filter(dat, y == 1942), aes(brat, fnormmid, label = y), hjust = 0, vjust = -0.5, size = 7) +
  geom_text(data = filter(dat, y == 1966), aes(brat, fnormmid, label = y), hjust = 0, vjust = -0.5, size = 7) +
  geom_text(data = filter(dat, y == 2017), aes(brat, fnormmid, label = y), hjust = 1, vjust = -0.5, size = 7) +
  scale_colour_gradient(low = "gray50", high = "gray0") +
  scale_x_continuous(limits = c(0, 2.05), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 2.25), expand = c(0, 0)) +
  xlab("Biomass Ratio") +
  ylab("Fishing Mortality Ratio") +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") + 
  lilfont

# Save.
ggsave("kobelil.png",
       kobelil,
       width = 2.75,
       height = 2.75,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("kobebig.png",
       kobebig,
       width = 8,
       height = 8,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

# And now, your market graph(s).
mar_mut = mutate(ma_dat, hat = fun_p(q, g, nlm_tidy[[1, 2]], nlm_tidy[[2, 2]], nlm_tidy[[3, 2]]))

mar_g =
  ggplot(mar_mut) +
  geom_point(aes(g, p), colour = "#04859B", size = 2.5) +
  geom_segment(aes(x = g, y = p, xend = g, yend = hat), colour = "#04859B", size = 1.25) +
  geom_point(aes(g, hat), colour = "#003660", size = 2.5) +
  labs(x = "Grams of Buche", y = "Price per Gram") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0, 100), expand = c(0, 0)) +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") + 
  bigfont

mar_g_lil = 
  ggplot(mar_mut) +
  geom_point(aes(g, p), colour = "#04859B", size = 0.75, fill = NA) +
  geom_segment(aes(x = g, y = p, xend = g, yend = hat), colour = "#04859B", size = 0.50) +
  geom_point(aes(g, hat), colour = "#003660", size = 0.75) +
  labs(x = "Grams of Buche", y = "Price per Gram") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0, 100), expand = c(0, 0)) +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") + 
  lilfont

mar_q_big = 
  ggplot(mar_mut) +
  geom_boxplot(aes(as.factor(q), hat), colour = "#003660", fill = "transparent", size = 1.25) +
  geom_jitter(aes(as.factor(q), p), colour = "#04859B", size = 2.5) +
  labs(x = "Tonnes of Buche at Market", y = "Gram Price") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0, 100), expand = c(0, 0)) +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") + 
  bigfont

mar_q_lil = 
  ggplot(mar_mut) +
  geom_boxplot(aes(as.factor(q), hat), colour = "#003660", fill = "transparent", size = 0.5) +
  geom_jitter(aes(as.factor(q), p), colour = "#04859B", size = 0.75) +
  labs(x = "Tonnes of Buche", y = "Price per Gram") +
  scale_y_continuous(breaks = seq(0, 100, by = 50), limits = c(0, 100), expand = c(0, 0)) +
  theme_classic(base_family = "avenir") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none") + 
  lilfont

ggsave("mar_g_lil.png",
       mar_g_lil,
       width = 2.40,
       height = 2.75,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("mar_g_big.png",
       mar_g,
       width = 8,
       height = 8,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("mar_q_lil.png",
       mar_q_lil,
       width = 2.40,
       height = 2.75,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("mar_q_big.png",
       mar_q_big,
       width = 8,
       height = 8,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

# Why not make additional plots? That would be great. That would be fantastic. Let's do it.
bc_dat = read_csv("bc_dat.csv")

plot_yb = 
  ggplot(bc_dat) +
  geom_line(aes(y, b), color = "#003660") +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(1925, 2017)) +
  labs(y = "Stock (Tonnes)") +
  theme_classic(base_family = "avenir") +
  theme(axis.title.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

plot_yc = 
  ggplot(bc_dat) +
  geom_line(aes(y, c), color = "#04859B") +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(1925, 2017)) +
  labs(y = "Catch (Tonnes)") +
  theme_classic(base_family = "avenir") +
  theme(axis.title.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

plot_yb = plot_yb + lilfont
plot_yc = plot_yc + lilfont

ggsave("yb.png",
       plot_yb,
       width = 1.2,
       height = 2.75,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

ggsave("yc.png",
       plot_yc,
       width = 1.2,
       height = 2.75,
       units = c("in"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

