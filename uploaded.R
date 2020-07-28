# 2.1

# Use scatterplots to examine the bivariate relationship between logged GDP per capita and life expectancy as well as
# between logged GDP per capita and illiteracy. Be sure to add informative axis labels. Also, compute the correlation
# separately for each bivariate relationship. Briefly comment on the results. To remove missing data when applying the cor
# function, set use argument to "complete.obs".


# Code for scatterplot examining bivariate relationship between logged GDP per capita and life expectancy (Figure 2.1.1):
plot(resources$logGDPcp, resources$life,
     xlab = "Logged GDP per Capita", ylab = "Life Expectancy", main = "Figure 2.1.1")

# Calculating the correlation between logged GDP per capita and life expectancy:
cor(resources$logGDPcp, resources$life, use = "complete.obs")
# 0.846728

# This shows that there is a strong positive correlation between logged GDP per capita and life expectancy, meaning the higher
# the logged GDP per capita there is in a country, the higher the life expectancy of its inhabitants is.


# Code for scatterplot examining bivariate relationship between logged GDP per capita and illiteracy (Figure 2.1.2):
plot(resources$logGDPcp, resources$illit,
     xlab = "Logged GDP per Capita", ylab = "Percentage of Population that is Illiterate", main = "Figure 2.1.2")

# Calculating the correlation between logged GDP per capita and levels of illiteracy:
cor(resources$logGDPcp, resources$illit, use = "complete.obs")
# -0.6701697

# This shows that there is a relative strong negative correlation between logged GDP per capita and levels of illiteracy,
# meaning the higher the logged GDP per capita there is in a country, the lower the levels of illiteracy the country has
# (so its inhabitants are more literate overall).



# 2.2

# We focus on the following subset of the variables: regime, oil, logGDPcp, and illit. Remove observations that have
# missing values in any of these variables. Using the scale() function, scale these variables so that each variable has
# a mean of zero and a standard deviation of one. Fit the k-means clustering algorithm with two clusters. How many observations
# are assigned to each cluster? What are the characteristics of each cluster?


# Removing observations with missing values in any variables:
full.resources <- na.omit(resources)

# Scaling each variable:
full.resources$scale.regime <- scale(full.resources$regime)
full.resources$scale.oil <- scale(full.resources$oil)
full.resources$scale.logGDPcp <- scale(full.resources$logGDPcp)
full.resources$scale.illit <- scale(full.resources$illit)

# Fitting a k-means clustering algorithm with two clusters:
cluster.resources <- kmeans(full.resources[, c("scale.regime", "scale.oil", "scale.logGDPcp", "scale.illit")], centers = 2)

# Working out how many observations are assigned to each cluster:
cluster.resources$size
# 138 103
# First cluster has 138 observations, second cluster has 103 observations.


# Characteristics of each cluster:

# Working out average center point for each variable within each cluster:
cluster.resources$centers
#     scale.regime  scale.oil   scale.logGDPcp  scale.illit
# 1    0.5965426    -0.2169156  0.5188724       -0.6943778
# 2   -0.7992512    0.2906248   -0.6951883      0.9303315


# Working out dispersion of data points around cluster center:
cluster.resources$withinss
# For first cluster: 189.9839, for second cluster: 397.2990
# This shows the data points are more closely grouped for the first cluster.



# 2.3

# Using the clusters obtained above, modify the scatterplot between logged GDP per capita and illiteracy rate in the
# following manner. Use different colors for the clusters so that we can easily tell the cluster membership of each
# observation. In addition, make the size of each circle proportional to the oil variable so that oil-rich countries
# stand out. Briefly comment on the results.

# Full code for the Figure 2.3.1:
cluster.resources <- kmeans(full.resources[, c("scale.regime", "scale.oil", "scale.logGDPcp", "scale.illit")], centers = 2)
cluster1 <- cluster.resources$cluster == "1"
full.resources$cluster <- ifelse(cluster.resources$cluster == cluster1, 1, 2)
plot(x = full.resources$scale.logGDPcp, y = full.resources$scale.illit, col = full.resources$cluster + 1,
     cex = full.resources$scale.oil + 1, xlab = "Logged GDP per Capita (Scaled)",
     ylab = "Percentage of Population that is Illiterate (Scaled)", main = "Figure 2.3.1")

# This graph gives an indication of which cluster each observation is in (one cluster being green, the other red), alongside
# how oil-rich each observation is, with the larger the circle representing the more oil-rich a country is.
# From this, we can see that countries with higher GDP generally have lower levels of illiteracy, with the reverse also being true.
# However, the more oil-rich a country is does not necessarily equate to high GDP or low levels of illiteracy. From the graph, it
# infact appears that while oil-rich countries do generally have higher GDP, but also generally higher levels of illiteracy, though
# this observation is not definite as this conclusion has been drawn purely from the graph, which does not show this conclusively
# as the oil-rich nations are grouped with the first cluster, so we do not know if there is any statistical differnce between these
# oil-rich nations and the others in the cluster.



# 2.4

# Repeat the previous two questions but this time with three clusters instead of two. How are the results different? 
# Which clustering model would you prefer and why?


# Fitting a k-means clustering algorithm with two clusters:
cluster.resources.3 <- kmeans(full.resources[, c("scale.regime", "scale.oil", "scale.logGDPcp", "scale.illit")], centers = 3)

# Working out how many observations are assigned to each cluster:
cluster.resources.3$size
# 80 113  48
# First cluster has 80 observations, second cluster has 113 and the third cluster has 48.


# Characteristics of each cluster:

# Working out average center point for each variable within each cluster:
cluster.resources.3$centers
#     scale.regime  scale.oil   scale.logGDPcp  scale.illit
# 1   -0.6002002    -0.2937231  -1.0476440      1.0759032
# 2    0.8816408    -0.2213713  0.6070070       -0.7106129
# 3   -1.0751959    1.0106833   0.3170776       -0.1202709


# Working out dispersion of data points around cluster center:
cluster.resources.3$withinss
# For first cluster: 116.8968, for second: 102.9855 and for third: 221.8763.
# This shows the data points are comparitely poorly grouped for the third cluster.


# Full code for the Figure 2.4.1:
cluster.resources.3 <- kmeans(full.resources[, c("scale.regime", "scale.oil", "scale.logGDPcp", "scale.illit")], centers = 3)

full.resources$cluster3 <- ifelse(cluster.resources.3$cluster == 1, 1, 
                                  ifelse(cluster.resources.3$cluster == 2, 2, 3))
plot(x = full.resources$scale.logGDPcp, y = full.resources$scale.illit, col = full.resources$cluster3 + 1,
     cex = full.resources$scale.oil + 1, xlab = "Logged GDP per Capita (Scaled)",
     ylab = "Percentage of Population that is Illiterate (Scaled)", main = "Figure 2.4.1")

# This graph gives an indication of which cluster each observation is in (one cluster being green, another red, and the final
# cluster blue), alongside how oil-rich each observation is, with the larger the circle representing the more oil-rich a country is.
# From this, we can see that countries with higher GDP generally have lower levels of illiteracy, with the reverse also being true.
# However, the more oil-rich a country is tends to equate to a comparitely higher GDP compared to other nations with similar
# levels of illiteracy, also meaning comparitely higher levels of illiteracy compared to other nations with similar levels of GDP.

# Compared to our analysis with just two clusters, which tried to draw conclusions from inconclusive evidence on the graph, with
# three clusters it becomes clear that oil-rich countries can be analysed as a separate group from the other nations.
# This new graph allows us to confidently conclude that oil-rich countries have their GDP boosted as a result of this oil-based
# wealth, but this is not reflected in their literacy rates.

# Also, the clusters in this graph are grouped much more closely (shown by the use of cluster.resources.3$withinss), suggesting
# the data better suits being plotted with three clusters, and also meaning the graph is clearer and conclusions can be drawn
# more confidently from the it.

# As a result, in this scenario I prefer using three clusters to two.

# Ultimately, this data suggests that oil-rich nations do not use their resources in a way which decreases levels of illiteracy,
# like nations with high GDP but that are less oil-rich do.