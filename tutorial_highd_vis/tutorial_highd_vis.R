## ----load libraries------------------------------------------------------
library(knitr)
library(tidyverse)
library(here)
library(nullabor)
library(forecast)
library(readxl)
library(GGally)
library(broom)
library(plotly)
library(RColorBrewer)
library(tourr)
library(spinifex)
library(geozoo)
library(mvtnorm)
library(randomForest)


## ----install and load a libary to make nicer maps of the USA-------------
# This package makes a nicer representation of the USA state map
#  with Hawaii and Alaska included
# remotes::install_github("wmurphyrd/fiftystater")
library(fiftystater)

## ------------------------------------------------------------------------
## # Extra libraries to make my notes more fun
## library(magick)
## # remotes::install_github("emitanaka/anicon")
## library(anicon)
## # remotes::install_github("sctyner/memer")
## library(memer)

## ----section header 1, eval=FALSE----------------------------------------
## # *******************
## # Basic visualisation
## # *******************

## ----read and wrangle TB data--------------------------------------------
tb <- read_csv("data/TB_notifications_2019-07-01.csv")
tb <- tb %>% select(country, iso3, g_whoregion, year, contains("new_sp_")) %>%
  gather("stuff", "count", new_sp_m04:new_sp_fu) %>%
  separate(stuff, c("stuff1", "stuff2", "sex_age"), split="_") %>%
  select(-stuff1, -stuff2) %>%
  mutate(sex = str_sub(sex_age, 1, 1), 
         age = str_sub(sex_age, 2, str_length(sex_age))) %>%
  select(-sex_age) 
tb_fr <- tb %>% filter(country == "France") %>%
  filter(!(age %in% c("u", "04", "014", "514")))

## ----starting to plot, echo=TRUE, fig.height=3---------------------------
ggplot(tb_fr, aes(x=year, y=count, fill=sex)) +
  geom_bar(stat="identity") + 
  facet_wrap(~age, ncol=6)

## ----colour and axes fixes, echo=TRUE, fig.height=3----------------------
# This makes shorter tick mark labels, and color blind proof scale
ggplot(tb_fr, aes(x=year, y=count, fill=sex)) +
  geom_bar(stat="identity") + 
  facet_wrap(~age, ncol=6) +
  scale_fill_brewer("", palette="Dark2") +
  scale_x_continuous("year", breaks=seq(1995, 2012, 5), labels=c("95", "00", "05", "10"))

## ----compare proportions of males/females, echo=TRUE, fig.height=3-------
# Fill the bars, note the small change to the code
ggplot(tb_fr, aes(x=year, y=count, fill=sex)) +
  geom_bar(stat="identity", position="fill") + 
  facet_wrap(~age, ncol=6) +
  scale_fill_brewer("", palette="Dark2") +
  scale_x_continuous("year", breaks=seq(1995, 2012, 5), labels=c("95", "00", "05", "10"))

## ----side-by-side bars of males/females, fig.height=3, eval=FALSE, echo=FALSE----
## # This code does something strange to the axis tick marks
## # We will skip it for now
## #ggplot(tb_fr, aes(x=year, y=count, fill=sex)) +
## #  geom_bar(stat="identity", position="dodge") +
## #  facet_wrap(~age, ncol=6) +
## #  scale_fill_brewer("", palette="Dark2") +
## #  scale_x_continuous("year", breaks=seq(1995, 2012, 5), labels=c("95", "00", "05", "10"))

## ----compare counts of males/females, echo=TRUE--------------------------
# Make separate plots for males and females, focus on counts by category
ggplot(tb_fr, aes(x=year, y=count, fill=sex)) +
  geom_bar(stat="identity") + 
  facet_wrap(sex~age, ncol=6) +
  scale_fill_brewer("", palette="Dark2") +
  scale_x_continuous("year", breaks=seq(1995, 2012, 5), labels=c("95", "00", "05", "10"))

## ----rose plot of males/females, echo=TRUE-------------------------------
# How to make a pie instead of a barchart - not straight forward
ggplot(tb_fr, aes(x=year, y=count, fill=sex)) +
  geom_bar(stat="identity") + 
  facet_wrap(sex~age, ncol=6) +
  scale_fill_brewer("", palette="Dark2") +
  scale_x_continuous("year", breaks=seq(1995, 2012, 5), labels=c("95", "00", "05", "10")) +
  coord_polar()

## ----stacked barchart of males/females, echo=TRUE, fig.height=8----------
# Step 1 to make the pie
ggplot(tb_fr, aes(x = 1, y = count, fill = factor(year))) +
  geom_bar(stat="identity", position="fill") + 
  facet_wrap(sex~age, ncol=6) +
  scale_fill_viridis_d("", option="inferno") +
  theme(legend.position="bottom")

## ----pie chart of males/females, echo=TRUE, fig.height=8-----------------
# Now we have a pie, note the mapping of variables
# and the modification to the coord_polar
ggplot(tb_fr, aes(x = 1, y = count, fill = factor(year))) +
  geom_bar(stat="identity", position="fill") + 
  facet_wrap(sex~age, ncol=6) +
  scale_fill_viridis_d("", option="inferno") +
  theme(legend.position="bottom") +
  coord_polar(theta="y")

## ----focus on one year gender, side-by-side bars of males/females, echo=TRUE, fig.height=3----
tb_fr %>% filter(year == 2012) %>%
  ggplot(aes(x=sex, y=count, fill=sex)) +
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~age, ncol=6) +
  scale_fill_brewer("", palette="Dark2") +
  ggtitle("Arrangement A")

## ----focus on one year age, side-by-side bars of age group, echo=TRUE, fig.height=3----
tb_fr %>% filter(year == 2012) %>%
  ggplot(aes(x=age, y=count, fill=age)) +
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~sex, ncol=6) +
  scale_fill_brewer("", palette="Dark2") +
  ggtitle("Arrangement B")


## ----section header 2, eval=FALSE----------------------------------------
## # ***********************
## # Inference with graphics
## # ***********************

## ----lineup 1, fig.height=8, fig.width=8---------------------------------
# Make a lineup of the mtcars data, 20 plots, one is the data, 
# and the others are null plots. Which one is different?
set.seed(20190709)
ggplot(lineup(null_permute('mpg'), mtcars), aes(mpg, wt)) +
  geom_point() +
  facet_wrap(~ .sample)

## ----lineup 2------------------------------------------------------------
# A different lineup of the mtcars data, still using permutation to generate the nulls
ggplot(lineup(null_permute('cyl'), mtcars),
       aes(mpg, .sample, colour = factor(cyl))) +
       geom_point()

## ----lineup 3, fig.height=6----------------------------------------------
# Assessing model fit, using a lineup of residual plots, 19 are nulls, and one is the 
# residual plot. Is there structure in the residual plot that identifies it as having 
# less than random variation. Nulls are generated by `rotating` residuals after model fit.
tips <- read_csv("http://www.ggobi.org/book/data/tips.csv")
x <- lm(tip ~ totbill, data = tips)
tips.reg <- data.frame(tips, .resid = residuals(x), .fitted = fitted(x))
ggplot(lineup(null_lm(tip ~ totbill, method = 'rotate'), tips.reg)) +
  geom_point(aes(x = totbill, y = .resid)) +
  facet_wrap(~ .sample)

## ----lineup 4------------------------------------------------------------
# Assessing time series model fit using simulation to produce null plots.
data(aud)
l <- lineup(null_ts("rate", auto.arima), aud)
ggplot(l, aes(x=date, y=rate)) + geom_line() +
  facet_wrap(~.sample, scales="free_y") +
  theme(axis.text = element_blank()) +
  xlab("") + ylab("")

## ----make a map lineup, fig.height=6, fig.width=10-----------------------
# Read xlsx spreadsheet on cancer incidence in USA, for a more
# complex lneup example, a lineup of maps
incd <- read_xlsx("data/IncRate.xlsx", skip=6, sheet=2) %>%
  filter(!(State %in% c("All U.S. combined", "Kansas"))) %>%
  select(State, `Melanoma of the skin / Both sexes combined`) %>%
  rename(Incidence=`Melanoma of the skin / Both sexes combined`) %>%
  mutate(Incidence = as.numeric(substr(Incidence, 1, 3)))

# State names need to coincide between data sets
incd <- incd %>% mutate(State = tolower(State))

# Make lineup of cancenr incidence
incd_lineup <- lineup(null_permute('Incidence'), incd, n=12)

# Join cancer incidence data to map polygons
incd_map <- left_join(fifty_states, filter(incd_lineup, .sample==1),
                      by=c("id"="State"))
for (i in 2:12) {
  x <- left_join(fifty_states, filter(incd_lineup, .sample==i),
                      by=c("id"="State"))
  incd_map <- bind_rows(incd_map, x)
}
# Remove Kansas - it was missing the cancer data
incd_map <- incd_map %>% filter(!is.na(.sample))

# Plot the maps as a lineup
ggplot(incd_map) + 
  geom_polygon(aes(x=long, y=lat, fill = Incidence, group=group)) + 
  expand_limits(x = incd_map$long, y = incd_map$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  scale_fill_distiller(palette="YlGn", type="seq", direction=1) +
  theme(legend.position = "none", 
        panel.background = element_blank()) +
  facet_wrap(~.sample, ncol=4)

## ----compute p-value, echo=TRUE------------------------------------------
# Compute the p-value associated with this lineup, and the observer responses
pvisual(20, 150, 12)

## ----outbreak check, echo=TRUE-------------------------------------------
# This is a potentially really tough example to generate null plots for. 
# Here is the plot again to stimulate your brain cells.
# What would you do to make a null plot to detect temporal anomalies?
tb_fr %>% filter(sex=="f", age=="2534") %>%
  ggplot(aes(x=year, y=count, fill=sex)) +
  geom_bar(stat="identity") + 
  scale_fill_brewer("", palette="Dark2") +
  theme(legend.position="none")

## ----section header 3, eval=FALSE----------------------------------------
## # ***********************************
## # Plotting multiple dimensions simply
## # ***********************************

## ----scatterplot matrix, echo=TRUE, fig.width=8, fig.height=8------------
# Make a simple scatterplot matrix of a classic data set
data(flea)
ggscatmat(flea, columns = 1:6)

## ----scatterplot matrix with colour, echo=TRUE, fig.width=8, fig.height=8----
# Re-make mapping colour to species (class)
data(flea)
ggscatmat(flea, columns = 1:6, color = "species") +
  theme(legend.position="bottom")

## ----generalised pairs plot, echo=TRUE, fig.width=8, fig.height=8--------
# Matrix plot when variables are not numeric
data(australia_PISA2012)
australia_PISA2012 <- australia_PISA2012 %>%
  mutate(desk = factor(desk), room = factor(room),
         study = factor(study), computer = factor(computer),
         software = factor(software), internet = factor(internet),
         literature = factor(literature), poetry= factor(poetry),
         art = factor(art), textbook = factor(textbook),
         dictionary = factor(dictionary),
         dishwasher = factor(dishwasher))
australia_PISA2012 %>% 
  filter(!is.na(dishwasher)) %>% 
  ggpairs(columns=c(3, 15, 16, 21, 26))

## ----generalised pairs plot enhance plots, echo=TRUE, fig.width=8, fig.height=8----
# Modify the defaults, set the transparency of points since there is a lot of data
australia_PISA2012 %>% 
  filter(!is.na(dishwasher)) %>% 
  ggpairs(columns=c(3, 15, 16, 21, 26), 
          lower = list(continuous = wrap("points", alpha=0.05)))

## ----generalised pairs plot enhance more, echo=TRUE, fig.width=8, fig.height=8----
# Make a special style of plot to put in the matirx
my_fn <- function(data, mapping, method="loess", ...){
      p <- ggplot(data = data, mapping = mapping) + 
      geom_point(alpha=0.2, size=1) + 
      geom_smooth(method="lm", ...)
      p
}
australia_PISA2012 %>% 
  filter(!is.na(dishwasher)) %>% 
  ggpairs(columns=c(3, 15, 16, 21, 26), 
          lower = list(continuous = my_fn))

## ----reponse vs predictors, echo=TRUE, fig.width=8, fig.height=3---------
# This is a variation of the scatterplot matrix when there are two sets of variables.
# A simple example is when there is one response variable, and multiple predictors.
australia_PISA2012 %>% 
  filter(!is.na(desk)) %>% 
  filter(!is.na(room)) %>% 
  filter(!is.na(study)) %>% 
  filter(!is.na(gender)) %>% 
  ggduo(columnsX=c(3, 4, 5, 6, 1), columnsY=21, 
        types=list(continuous = wrap("points", alpha=0.1)))

## ----how to pull the tb data directly, eval=FALSE------------------------
## # library(getTBinR)
## # tb_burden <- get_tb_burden(verbose = FALSE)
## # dict <- get_data_dict(verbose = FALSE)
## # save(tb_burden, file="data/tb_burden.rda")

## ----explore tb mortality trends, echo=TRUE, fig.width=8, fig.height=8----
# A more complex example of using the scatterplot matrix to explore
# a large collection of time series. Compute statistics for each time
# series, which might be called tignostics, and plot these. Explore 
# the scatterplot matrix for anomalies and clusters. 
load("data/tb_burden.rda")
# Fit a model for each country, and extract statistics
tb_reg1 <- tb_burden %>%
  group_by(country) %>%
  nest() %>%
  mutate(model = map(data, ~lm(e_mort_exc_tbhiv_100k ~ year, data = .x) %>% 
                       tidy)) %>% 
  unnest(model) %>%
  select(country, term, estimate) %>%
  spread(term, estimate) %>%
  rename(intercept = `(Intercept)`, slope=year)
tb_reg2 <- tb_burden %>%
  group_by(country) %>%
  nest() %>%
  mutate(model = map(data, ~lm(e_mort_exc_tbhiv_100k ~ year, data = .x) %>% 
                       glance)) %>% 
  unnest(model) %>%
  select(country, r.squared, sigma, BIC, deviance)
tb_reg <- left_join(tb_reg1, tb_reg2)
# Drop the 0 deviance, 0 sigma countries
tb_reg <- tb_reg %>% filter(sigma > 0, BIC > -400)
ggscatmat(tb_reg, columns=3:7)

## ----explore tb mortality trends interactively, echo=TRUE, fig.width=8, fig.height=6----
# Add interaction to find the id's for countries that are anomalies
tb_reg_m <- as.data.frame(tb_reg[,3:7])
rownames(tb_reg_m) <- tb_reg$country
tb_reg_m %>% ggpairs() %>% ggplotly()

## ----plot the countries that have decreasing mortality trend, echo=TRUE, fig.width=8, fig.height=5----
# Use a dotplot with model overlaid, to better match analysis conducted
declining <- tb_reg %>% filter(slope < -3.5)
tb_burden %>% filter(country %in% declining$country) %>%
  ggplot(aes(x=year, y=e_mort_exc_tbhiv_100k)) + 
    geom_point() +
    geom_smooth(method="lm", se=F) +
  facet_wrap(~country, scales = "free_y")

## ----explore tb mortality trends problem countries, echo=TRUE, fig.width=8, fig.height=3----
increasing <- tb_reg %>% filter(slope > 1, r.squared > 0.5)
tb_burden %>% filter(country %in% increasing$country) %>%
  ggplot(aes(x=year, y=e_mort_exc_tbhiv_100k)) + 
    geom_point() +
    geom_smooth(method="lm", se=F) +
  facet_wrap(~country, scales = "free_y")


## ----section header 4, eval=FALSE----------------------------------------
## # ************************
## # Tours of high-dimensions
## # ************************

## ----code for generating a 6D tour, eval=FALSE, echo=TRUE----------------
## # The tour requires making many plots, and updating.
## # The RStudio graphics device doesn't work well
## # Open a graphics window
## # quartz()  # Mac OSX
## # X11()     # Windows
animate_xy(flea[,1:6], axes = "off")

## ----eval=FALSE----------------------------------------------------------
## render_gif(flea[,1:6], grand_tour(), display_xy(axes="off"),
##            frames=200,
##            gif_file="images/flea_tour.gif")


## ----make a 5D cube, eval=FALSE, echo=TRUE-------------------------------
cube <- cube.face(p = 5)
animate_xy(cube$points)

## ----make a 5D sphere, eval=FALSE, echo=TRUE-----------------------------
sphere <- data.frame(sphere.hollow(p = 5)$points)
animate_xy(sphere)

## ----make a 5D simplex, eval=FALSE, echo=TRUE----------------------------
simp <- simplex(p = 5)
animate_xy(simp$points, edges=as.matrix(simp$edges), axes="bottomleft")

## ----simulate sample from a 5D standard normal, eval=FALSE, echo=TRUE----
x <- data.frame(rmvnorm(500, mean = rep(0, 5)))
animate_xy(x, axes="bottomleft")

## ----simulate sample from a 5D t, eval=FALSE, echo=TRUE------------------
x <- data.frame(rmvt(500, sigma = diag(5)))
animate_xy(x, axes="bottomleft")

## ----simulate sample from a mixture of 5D standard normal, eval=FALSE, echo=TRUE----
simp <- simplex(p = 5)$points*5
x1 <- data.frame(rmvnorm(100, mean=simp[1,]))
x2 <- data.frame(rmvnorm(100, mean=simp[2,]))
x3 <- data.frame(rmvnorm(100, mean=simp[3,]))
x4 <- data.frame(rmvnorm(100, mean=simp[4,]))
x5 <- data.frame(rmvnorm(100, mean=simp[5,]))
x <- bind_rows(x1, x2, x3, x4, x5)
animate_xy(x, axes="bottomleft")
animate_xy(x, axes="bottomleft", tour_path=guided_tour(holes()))

## ----guided tour 6D, eval=FALSE, echo=TRUE-------------------------------
animate_xy(flea[,1:6], guided_tour(lda_pp(flea$species)), axes="bottomleft")

## ----generate a sequence to rotate a variable out of a projection, eval=FALSE, echo=TRUE----
## # When you find a low dimensional projection from some other technique
## # such as principal component analysis, linear discriminant analysis or
## # projection pursuit, it is useful to examine the sensitivity of structure
## # to variables in the projection. This is the purpose of the manual tour.
## # Take a variable, and rotate it out of the projection and see if the structure
## # persists or disappears.
flea_std <- tourr::rescale(flea[, 1:6])

rb <- basis_random(n = ncol(flea_std))
mtour <- manual_tour(basis = rb, manip_var = 4)
sshow <- array2df(array = mtour, data = flea_std)
render_plotly(slides = sshow)

render_plotly(slides = sshow, col = col_of(flea$species),
  fps = 2)

## ----read and tour on in a classic data set, echo=TRUE, eval=FALSE-------
olive <- read_csv("http://www.ggobi.org/book/data/olive.csv") %>%
  rename(name=X1)
olive <- olive %>%
  filter(region == 1) %>%
  mutate(area = factor(area))
pal <- brewer.pal(4, "Dark2")
col <- pal[olive$area]
# drop eicosenoic, all low for south
animate_xy(olive[,4:10], axes="bottomleft", col=col)
# Drop Sicily
animate_xy(olive[olive$area!=4,4:10], axes="bottomleft", col=col[olive$area!=4])

## ----Fit a randomForest model an examine the vote matrix, echo=TRUE, eval=FALSE----
olive_rf <- randomForest(area~., data=olive[,-c(1, 2, 11)], importance=TRUE, proximity=TRUE)
olive_rf
vt <- data.frame(olive_rf$votes)
vt$area <- olive$area
ggscatmat(vt, columns=1:4, col="area") +
  scale_colour_brewer("", palette="Dark2")
proj <- t(geozoo::f_helmert(4)[-1,])
vtp <- as.matrix(vt[,-5])%*%proj
vtp <- data.frame(vtp, area=vt$area)
ggscatmat(vtp, columns=1:3, col="area") +
  scale_colour_brewer("", palette="Dark2")
pal <- brewer.pal(4, "Dark2")
col <- pal[as.numeric(vtp[, 4])]
animate_xy(vtp[,1:3], col=col, axes = "bottomleft")
# Add simplex
simp <- simplex(p=3)
sp <- data.frame(simp$points)
colnames(sp) <- c("x1", "x2", "x3")
colnames(vtp) <- c("x1", "x2", "x3")
vtp_s <- bind_rows(sp, vtp[,1:3])
animate_xy(vtp_s, col=col, axes = "bottomleft", edges=as.matrix(simp$edges), center=TRUE)

## ----tb pca analysis, eval=FALSE, echo=TRUE------------------------------
library(naniar) # Have missings!
tb_burden_wide <- tb_burden %>%
  select(country, g_whoregion, year, e_mort_exc_tbhiv_100k) %>%
  spread(year, e_mort_exc_tbhiv_100k) %>%
  filter(complete.cases(.)) %>%
  rename(region = g_whoregion) %>%
  mutate(country = factor(country), region = factor(region))
# vis_miss(tb_burden_wide)
tb_pca <- prcomp(tb_burden_wide[,-c(1:2)], scale=FALSE, retx=TRUE)
screeplot(tb_pca, type="line")
tb_pcs <- bind_cols(as_tibble(tb_pca$x), tb_burden_wide[,1:2])
ggscatmat(tb_pcs, columns=1:3, color="region")
# quartz()
# X11()
pal <- brewer.pal(6, "Dark2")
col <- pal[as.numeric(as.factor(tb_pcs$region))]
animate_xy(tb_pcs[,1:4], col=col, axes = "bottomleft")

## ----tb mortality line plots, eval=FALSE, echo=TRUE----------------------
tb_burden <- tb_burden %>%
  rename(region = g_whoregion)
ggplot(tb_burden, aes(x=year, y=e_mort_exc_tbhiv_100k, 
                      group=country, colour=region)) +
  geom_line() + facet_wrap(~region)

## ----eval=FALSE, echo=FALSE----------------------------------------------
# Function to generate edges of dendrogram
hierfly <- function(data, h=NULL, metric="euclidean", method="ward.D2", scale=TRUE) {
  if (scale) data <- rescaler(data)
  id <- 1:nrow(data)
  cat <- sapply(data, is.factor)
  if (is.null(h))
    h <- hclust(dist(data[,!cat], metric), method)
  #h <- hclust(dist(data, metric), method)
  data_hc <- data

  data_hc$ORDER <- order(h$order)
  data_hc$HEIGHT <- 0
  data_hc$LEVEL <- 0
  data_hc$POINTS <- 1

  #newr_df <- NULL
  data_hc <- as.data.frame(data_hc)
  for (i in 1:nrow(h$merge)) {
    newr <- combinerows(data_hc[as.character(-h$merge[i,]),], cat)
    #newr <- combinerows(data_hc[as.character(-h$merge[i,]),], rownames(data))
    #newr$id <- nrow(data_hc) + i
    newr$HEIGHT <- h$height[i]
    newr$LEVEL <- i
    rownames(newr) <- as.character(-i)

    data_hc <- rbind(data_hc, newr)
  }
  data_hc$id <- 1:nrow(data_hc)
  data_hc$node <- (as.numeric(rownames(data_hc)) < 0) + 0

  vars <- c("ORDER", "POINTS")

  leaves <- subset(data_hc, node == 0)
  nodes <- subset(data_hc, node == 1)

  # < 0 = observation, > 0 = cluster
  edges <- h$merge
  edges[edges > 0] <- edges[edges > 0] + nrow(leaves)
  edges[edges < 0] <- abs(edges[edges < 0])
  edges <- as.matrix(edges)

  return(list(data=data_hc, edges=edges))
}

# Utility functions
combinerows <- function(df, cat) {
  same <- function(x) if (length(unique(x)) == 1) x[1] else x[2]
  points <- df$POINTS

  cont <- as.data.frame(lapply(df[, !cat, drop=FALSE] * points,
                               sum)) / sum(points)
  cat <- as.data.frame(lapply(df[, cat, drop=FALSE], same))

  df <- if (nrow(cont) > 0 && nrow(cat) > 0) {
    cbind(cont, cat)
  } else if (nrow(cont) > 0) {
    cont
  } else {
    cat
  }
  df$POINTS <- sum(points)
  df
}

rescaler <- function(df) {
  is_numeric <- vapply(df, is.numeric, logical(1))
  df[is_numeric] <- lapply(df[is_numeric], rescale01)
  df
}

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

## ----eval=FALSE, echo=TRUE-----------------------------------------------
# Load the utility functions: hierfly, combinerows, rescaler
tb_hc <- hclust((dist(tb_pcs[,1:4])), method="ward.D2")
plot(tb_hc)
tb_clw <- tb_pcs[,1:4] %>% mutate(cl = factor(cutree(tb_hc, 5)))
tb_w_hfly <- hierfly(data=tb_clw, h=tb_hc, scale=FALSE)
pal <- brewer.pal(6, "Dark2")
col <- pal[as.numeric(tb_w_hfly$data$cl)]
shp <- c(16, 6)
pch <- shp[tb_w_hfly$data$node+1]
# quartz()
# X11()
animate_xy(tb_w_hfly$data[,1:4], col=col, pch=pch,
           axes = "bottomleft",
           edges=tb_w_hfly$edges, edges.col=col[210:417])

## ----nonlinear dimension reduction, eval=FALSE, echo=TRUE----------------
# remotes::install_github("sa-lee/sneezy")
library(gganimate) # required for printing tours
library(sneezy)
# Read a benchmark data set
spheres <- subset(multi, key %in% c("A", "D"))
labels <- ifelse(spheres$key == "A", "sub-gaussian", "10-d 2x cluster")
spheres <- as.matrix(spheres[, -c(1,2)])

## # t-SNE plot
set.seed(1010010)
coords <- basic_tsne(spheres, perplexity = 30)
pl <- ggplot(as.data.frame(coords$Y), aes(V1, V2)) +
  geom_point(aes(colour = labels)) +
  coord_equal() +
  scale_color_brewer(palette = "Dark2") +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

pl +  add_triangles(coords)

# in data space, with a triangulation of the points from the tSNE view
# quartz()
# X11()
pal <- c("#1B9E77", "#D95F02")[as.integer(as.factor(labels))]
sneezy_triangles(spheres, coords, col = pal)

## ----tour again for comparison, eval=FALSE-------------------------------
# quartz()
# X11()
pal <- brewer.pal(4, "Dark2")
col <- pal[olive$area]
# drop eicosenoic, all low for south
animate_xy(olive[,4:10], axes="bottomleft", col=col)
# Drop Sicily
animate_xy(olive[olive$area!=4,4:10], axes="bottomleft", col=col[olive$area!=4])

## ----make a parallel coordinate plot of the olive data, eval=FALSE, echo=TRUE----
ggparcoord(olive, columns=4:10, groupColumn=3, order="anyClass") +
  scale_colour_brewer("", palette="Dark2")

## ----make a heatmap of the olive data, eval=FALSE, echo=TRUE-------------
library(superheat)
superheat(olive[,4:10], scale=TRUE,
          pretty.order.rows=TRUE, pretty.order.cols=TRUE,
          row.dendrogram = TRUE)

## ----plotly tourr, eval=FALSE, echo=TRUE---------------------------------
flea_std <- rescale(tourr::flea[,1:6])
tpath    <- save_history(flea_std, max = 3)

pg <- play_tour_path(tour_path = tpath, data = flea_std, angle = .15)
pg
save_html(pg, file="mytour.html")

## ----saving a tour as an animated fig using gganimate, eval=FALSE, echo=TRUE----
library(gganimate)
render_gif(flea[,1:6], grand_tour(), display_xy(axes="off"),
           frames=200,
           gif_file="mytour.gif")
