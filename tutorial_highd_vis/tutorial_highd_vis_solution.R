# Solutions to YOUR TURNs

# Extend the last plot to examine two years, 2008 and 2012. 

tb_fr %>% filter(year %in% c(2008, 2012)) %>%
  ggplot(aes(x=age, y=count, fill=age)) +
  geom_bar(stat="identity", position="dodge") + 
  facet_grid(year~sex) +
  scale_fill_brewer("", palette="Dark2") +
  ggtitle("Arrangement B and Year")

tb_fr %>% filter(year %in% c(2008, 2012)) %>%
  ggplot(aes(x=year, y=count, fill=year)) +
  geom_bar(stat="identity", position="dodge") + 
  facet_grid(age~sex) +
  scale_fill_distiller("", palette="Dark2") 

tb_fr %>% filter(year %in% c(2008, 2012)) %>%
  ggplot(aes(x=sex, y=count, fill=year)) +
  geom_bar(stat="identity", position="fill") + 
  facet_wrap(~age, ncol=6) +
  scale_fill_distiller("", palette="Dark2") 

tb_fr %>% filter(year %in% c(2008, 2012)) %>%
  ggplot(aes(x=year, y=count, fill=sex)) +
  geom_bar(stat="identity", position="fill") + 
  facet_wrap(~age, ncol=6) +
  scale_fill_brewer("", palette="Dark2") 

# --------
# How *might you* generate the nulls?

# I think that I would fit a model count~year*gender*age and simulate from this model
# Would only plot the counts for females, 2534 by year for each of the simulated data sets.

# --------
# Re-make the plot with side-by-side boxplots on the lower triangle, 
# for the combo variables, and the density plots in the upper triangle.

australia_PISA2012 %>% 
  filter(!is.na(dishwasher)) %>% 
  ggpairs(columns=c(3, 15, 16, 21, 26), 
          lower = list(continuous = my_fn, combo = "box"), 
          upper = list(combo = "facetdensity"))

# --------
# Plot all the coutries in one plot, as transparent lines. 

tb_burden %>% 
  ggplot(aes(x=year, y=e_mort_exc_tbhiv_100k)) + 
  geom_line(aes(group=country), alpha=0.3) +
  geom_smooth(se=F) 

# --------
# Make a 3D torus, followed by  a 4D torus

tor <- data.frame(torus(p = 3, n = 1000)$points)
animate_xy(tor, axes="bottomleft")

