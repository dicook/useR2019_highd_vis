WIFI: USER

pwd: user!2019

# Visualising high-dimensional data 
## Tutorial notes for useR! 2019

The workshop is interactive, bring your laptop set up with the **latest versions of R (>3.5) and RStudio**, and these R packages:

```
install.packages(c("knitr", "tidyverse", "here", "nullabor", "forecast", "readxl", "GGally", "broom", "plotly", "tourr", "spinifex", "geozoo", "mvtnorm", "randomForest", "RColorBrewer"))
```

Also install:

```
remotes::install_github("wmurphyrd/fiftystater")
```

If you have signed up for this tutorial it will be helpful to **download a copy of these materials ahead of time**, by downloading the `tutorial_highd_vis.zip` file above. Unzip the file into a project folder, to work through during the tutorial. It contains these files:

- `data/TB_notifications_2019-07-01.csv`
- `data/tb_burden.rda`
- `data/TB_data_dictionary_2019-07-01.csv`
- `data/IncRate.xlsx`
- `tutorial_highd_vis.Rproj`
- `tutorial_highd_vis.R`

**Please have these on your computer by tutorial start (Tue Jul 9).**

This is the planned structure for the tutorial:

A. Review basic visualisation and inference with graphics: This part
covers making plots using the grammar of graphics and how this fits
into statistical inference. We will use the packages ggplot2 and
nullabor. 

B. Plotting multiple dimensions in a single static plot, adding
interaction: The building blocks to viewing high-dimensions are
generalised pairs plots and parallel coordinate plots, available in
the R package GGally. There are many variations and options that will
be discussed, along with making these interactive with the plotly package.

C. Using dynamic plots (tours) to examine models in the data space,
beyond 3D: This part will cover the use of tours to examine
multivariate spaces, in relation to dimension reduction techniques
like principal component analysis and t-SNE, supervised and
unsupervised classification models. We will also examine
high-dimension, low-sample size problems. The tourr and spinifex
packages will be used. 

Materials are designed for an intermediate audience, users who are familiar
with R, basic visualisation and tidyverse tools, and who would like to
improve their knowledge about data visualisation. 

*Looking forward to meeting you all! And excited to be at useR! 2019.*
