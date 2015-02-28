
#
#' ---
#' title:  "form605_analysis file"
#' author: "Bill Alpert, Barron's, 1.212.416.2742, william.alpert@barrons.com"
#' date:   "last changed February 27, 2015"
#' ---
#'

# DISCLAIMER: Barron's is sharing these files as pieces of journalism, in an attempt to make our reporting more transparent and our research reproducible.  We wrote them with care, but Dow Jones provides them as is and makes no guarantees.

# 16FEB2015 changes;

# Fixed weighting of averages for effective-over-quoted spread ratio,
# to weight for spread and not just share volume.
# Otherwise, the EQ score of 50 for 100 shares is the same no matter what symbol you are trading.
# This had created an apples to oranges comparison because it doesn't account for the difference
# in spreads between the wholesalers.  By adding the spread you eliminate that issue.


# 08JAN2015 changes:

#  * I settled on using the sum of the market-center plus away-executed share counts (my variables
#    mc_exec_shrs + away_exec_shrs, or on the raw 605 form, fields F10 + F11) for share-weighting
#    denominators.
#
#  * I'll focus on market-orders, not marketable-limit-orders
#
#  * worked out weighted-average algorithms for firm-wide means or medians on a measure like
#    net price-improvement, to appropriately weight liquid vs. illiquid tickers.
#
#  * worked out ticker-level grouping, as a step in figuring out firm-level weighted averages.
#
#  * worked the kinks out of the effective-over-quoted spread ratio calculation.

# 06JAN2015 changes:

#   * I added calculations of effective-over-quoted spreads


# 23DEC2014 changes:

#   * I redefined "f605_mktble_select" to "f605_mktble_df," which includes
#     all columns in the analyzed data frame, allowing more kinds of analysis.
#
#   * Most importantly, I fixed a parenthesis mistake in my formulae for net-price improvement, in
#     rows 88-100. The results seem more plausible to those who've reviewed the analysis.
#
#   * I investigate net price-improvement, using different denominators in the share-weighting
#     1) just market center-executed orders. This is the most sensible choice.
#     2) market center-executed plus away-executed shares,
#        because some market centers hold themselves accountable for away-executed shares
#     3) total-orders minus cancelled-orders, an alternative to 2), above.
#
#   * There seems to be a big difference between the price improvement shown for market orders
#     and for marketable limit orders.
#
#   * I'm trying to decide between mean and median of all stocks, as my measure of
#     net price improvment.
#
#   * Some Form 605s report for multiple market centers within a holding company


# -----------
# set the working directory, for Windows or Mac
# -----------

os_name = Sys.info()[['sysname']]
if(os_name == "Windows"){
  setwd("C:/rule605")
}else if(os_name =="Darwin"){  # OS X
  setwd("~/rule605")
}else{                         # Linux
  stop("I'm a penguin.")
}

# You should use the "install_packages.R" file to first install the 'big vis" package
# which we use for speedy computation of weighted-medians. It's a development-stage
# package, as of this writing, and therefore not as easy to install as the following ones.

# Any other package required by the script below is given here
inst_pkgs = load_pkgs = c("plyr", "dplyr", "tidyr", "ggplot2", "magrittr", "bigvis")

# Check to see if the packages are already installed
inst_pkgs = inst_pkgs[!(inst_pkgs %in%
                          installed.packages()[, "Package"])]

# install any missing packages
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load required pacakges
pkgs_loaded = lapply(load_pkgs, require, character.only = T)


# get the merged, cleaned Form 605 data frame produced by the 'form605_merge_data.R' file,
# which has the last three months' 605 filings, filtered to include just 'market orders'
# and 'marketable limit orders'.

f605_mktble_df <- form_605_mktble

# ----------

# THE DATA FRAME's TABLE FORMAT
#
# -----------

# Each row in the raw form 605 has execution data for each combination of
# ticker & order-type & order-size. My R data frame 'f605_mktble_df' retains this format.

# ----------

# ----------
#
# Data tweaks
#
# ----------

# Rename the levels of order_size and order_type from code numbers to meaningful labels. Note that we'll filter out from our calculations all order types, except "mkt_ordr" and "mktble_lmt_ordr"

levels(f605_mktble_df$order_size) <- c("100-499", "500-1999", "2000-4999", "5000+")

levels(f605_mktble_df$order_type) <- c("mkt_ordr", "mktbl_lmt_ordr", "insd_qt_lmt_ordr",
                                       "at_qt_lmt_ordr", "nr_qt_lmt_ordr")

# Change the class of the market_center column from character to factor,
# to see if there are more than one market centers reported in the Form 605.
# Some holding companies report several market centers on one form.

f605_mktble_df[,"market_center"] <- as.factor(f605_mktble_df[,"market_center"])

# Check for multiple market centers

unique(f605_mktble_df$market_center)


# some convenience groupings that we might use in calculating our measures

monthly <- dplyr::group_by(f605_mktble_df, date)

size    <- dplyr::group_by(f605_mktble_df, order_size)

stock   <- dplyr::group_by(f605_mktble_df, ticker)

# We create a new column, 'mcaway_exec_shrs',
# that holds the sum of market-executed plus away-ex-shares shares,
# (my variables mc_exec_shrs + away_exec_shrs, or on the raw 605 form, fields F10 + F11)
# for use as a share-weighting denominator.

# I combine them because some firms report the values separately,
# while others include away-executed shares in their market center-executed share count
# and leave the away-executed share field empty.

f605_mktble_df <- f605_mktble_df %>%
  mutate(mcaway_exec_shrs = mc_exec_shrs + away_exec_shrs)


# ##########
#
# Create execution measures
#
# ##########


# ----------
#
# A WEIGHTING FORMULA FOR ROW-LEVEL NET PRICE-IMPROVEMENT
#
# ----------

# OUR OBJECTIVE: Market participants want summary measures of execution quality
# that appropriately weight liquid and illiquid trades.

# The numerator: 100 * ((px_improved_shrs * px_improved_avg_amt) + (at_quote_shrs * 0) -  (outside_quote_shrs * outside_quote_avg_amt))

# Multiplying by 100 gives a result in cents per share.

# Shares executed at the quote will zero out of the calculation.


# Create the row level net_pi_numerator

f605_mktble_df <- f605_mktble_df %>%
  mutate(net_pi_numerator = 100 * ((px_improved_shrs * px_improved_avg_amt) + (at_quote_shrs * 0) -  (outside_quote_shrs * outside_quote_avg_amt))
  )

# For a row level weighted net-pi, divide the net_pi_numerator by all executed shares in the row.

# The denominator: As discussed above in lines 133ff,
# the denominator is (mc_exec_shrs + away_exec_shrs),
# a sum that I've put in the column variable 'mcaway_exec_shrs'.

# We create a column with these row-level share-weighted net_pi measures.
# Each one is weighted by the proportionate counts of shares improved and disimproved.

# Units are cents per share.

f605_mktble_df <- f605_mktble_df %>%
  mutate(net_pi_mcaway = 100 * ((px_improved_shrs * px_improved_avg_amt) + (at_quote_shrs * 0) -  (outside_quote_shrs * outside_quote_avg_amt)) / mcaway_exec_shrs
  )


# ##########
#
# net_pi measures, as computed by many industry participants
#
# ##########


# I've been told that many market-centers, brokers and third-party execution-quality applications
# calculate a weighted-mean for a market-center's overall net-price-improvement by weighting each row's
# reported amounts of (dis)improvement by the row-level share amounts [yielding the value that I
# dubbed the net_pi_numerator] and then dividing the column-sum of those values by the column-sum
# of executed shares [which I've dubbed mcaway_exec_shrs, including all the row's executions].
#
# I use that method here and the results seem to match what industry folk report,
# summing the net_pi_numerators I created for each row,
# then dividing by the sum of all rows' executed shares.


# mean 'net_pi' for all stocks, all months

net_pi_mcaway_all_mean <- f605_mktble_df %>%
  summarise(net_pi_mcaway_all_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )

# mean 'net_pi' by month

net_pi_mcaway_monthly_mean <- f605_mktble_df %>%
  group_by(date) %>%
  summarise(net_pi_mcaway_monthly_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )


# mean 'net_pi' by order type, all stocks, all order sizes, all months

net_pi_mcaway_type_mean <- f605_mktble_df %>%
  group_by(order_type) %>%
  summarise(net_pi_mcaway_type_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )


# mean 'net_pi' by order size, all stocks, both order types, all months

net_pi_mcaway_size_mean <- f605_mktble_df %>%
  group_by(order_size) %>%
  summarise(net_pi_mcaway_size_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )


# #########
#
# We're probably going to look only at market-orders, so filter for just that order type
#
# #########


# mean 'net_pi' for all stocks, all months, just market orders

net_pi_mcaway_morder_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  summarise(net_pi_mcaway_morder_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )

# mean 'net_pi' by month, just market orders

net_pi_mcaway_morder_monthly_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  group_by(date) %>%
  summarise(net_pi_mcaway_morder_monthly_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )



# mean 'net_pi' by order size, all stocks, all months, just market orders

net_pi_mcaway_morder_size_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  group_by(order_size) %>%
  summarise(net_pi_mcaway_morder_size_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )


# in S&P 500

net_pi_mcaway_morder_sp500_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(in_sp500 == TRUE) %>%
  summarise(net_pi_mcaway_morder_sp500_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )

# not in S&P 500

net_pi_mcaway_morder_notsp500_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(in_sp500 == FALSE) %>%
  summarise(net_pi_mcaway_morder_notsp500_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )

# in Russell 1000

net_pi_mcaway_morder_r1000_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(in_r1000 == TRUE) %>%
  summarise(net_pi_mcaway_morder_r1000_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )

# not in Russell 1000

net_pi_mcaway_morder_notr1000_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(in_r1000 == FALSE) %>%
  summarise(net_pi_mcaway_morder_notr1000_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )

# on NYSE

net_pi_mcaway_morder_nyse_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(on_nyse == TRUE) %>%
  summarise(net_pi_mcaway_morder_nyse_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )

# on  Nasdaq

net_pi_mcaway_morder_nasdaq_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(on_nasdaq == TRUE) %>%
  summarise(net_pi_mcaway_morder_nasdaq_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )

# on AMEX

net_pi_mcaway_morder_amex_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(on_amex == TRUE) %>%
  summarise(net_pi_mcaway_morder_amex_mean = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )

# for Banc of America (ticker: BAM) shares:

net_pi_mcaway_morder_BAM  <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(ticker == "BAM") %>%
  summarise(net_pi_mcaway_morder_BAM = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )

# for Sirius XM (ticker: SIRI) shares:

net_pi_mcaway_morder_SIRI <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(ticker == "SIRI") %>%
  summarise(net_pi_mcaway_morder_SIRI = (sum(net_pi_numerator) / sum(mcaway_exec_shrs))
  )

# # ##########
# #
# # Some alternative ways to calculate net-pi
# #
# # ##########

# The weighted mean calculated as above throws away information about row-level and even ticker-level
# trading volume that would help us understand the distribution of price-improvement across stocks.

# It's not obvious how to show boxplots, standard deviations, percentage quantiles
# around such an average.

# People seem to have calculated the weighted-mean that way
# because they don't want illiquid names to get undue weight, and perhaps
# because the calculation is easily done in a spreadsheet or SQL database.

# Consider some alternatives.

# If we want to weight a firmwide average in proportion to liquidity,
# Form 605s report trading volumes for each row, giving us tens of thousands of weights to use.
# A firm's volume in a ticker/order_type/order_size may not be a perfect proxy for liquidity,
# but it's not bad.

# I understand that some firms aggregate the table by ticker, combining all of a ticker's orders,
# then averaging those 8,000-odd ticker-level measures with ticker-volumes as the weights, to get
# firmwide numbers.

# Again, I think that's throwing away information. If our aim is to weight a firm-level average
# in proportion to trading volume, with as much precision as a Form 605 allows,
# a ticker-level aggregation of trading volumes obscures information provided in the raw Form 605,
# where we're given volumes for each order_size and order_type in a ticker (although we're admittedly
# not interested in most of the order_types reported).

# In any event, I'll calculate weighted averages using both
# ticker-level weightings and row-level weightings, and we'll see if there's a difference.


# On lines 187ff, we created 'net_pi_mcaway', the per-row share-weighted price-improvement,
# with the denominator being the row's executed shares (at and away from the market center).
# The column has up to 20 such values for each size/order combo of each ticker.

# First, let's calculate a conventional mean of that column,
# for all a firm's orders in the period under study.
# How does this simple mean compare to the weighted-mean calculated above, in lines 211ff ?

net_pi_mcaway_mean_column <- f605_mktble_df %>%
  summarise(net_pi_mcaway_mean_column = mean(net_pi_mcaway))

# Now for 150,000+ rows in three months' reports, compute a column-wise weighted mean of
# each row-weighted average net_pi_mcaway (created on line 187).

# The weights for the columnwise mean will be each row's 'mcaway_exec_shrs'.

# There are two levels of weighting here. First, the row-weighted average net-pi that gives
# appropriate weights to improved and disimproved shares, respectively.

# Second, the column-wise weighting of all the row-level net-pi's,
# with the weights being each row's executed share volume (my 'mcaway_exec_shrs').

# This weighting uses trading volume as a proxy for liquidity (which is never observable anyway),
# so that low-volume rows get less weight than high-volume rows.

# In the weighted.mean() below, the first parenthetical term is what we're averaging, the second
# supplies the weights.


net_pi_mcaway_morder_wmean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  summarise(net_pi_mcaway_morder_wmean = weighted.mean(net_pi_mcaway, mcaway_exec_shrs)
  )


# This version of the weighted mean yields the same answer as the "conventional" version on line 211.

net_pi_mcaway_morder_sp500_wmean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(in_sp500 == TRUE) %>%
  summarise(net_pi_mcaway_morder_sp500_wmean = weighted.mean(net_pi_mcaway, mcaway_exec_shrs)
  )

# # A table showing equivalency to "industry standard" weighted mean
#
# net_pi_eq_df <- cbind(net_pi_mcaway_morder_mean, net_pi_mcaway_morder_wmean, net_pi_mcaway_morder_sp500_mean, net_pi_mcaway_morder_sp500_wmean)
#
# net_pi_eq_df <- net_pi_eq_df %>%
#   gather(average, net_pi)
#
# rownames(net_pi_eq_df) <- c("indsty_std_morder_wgtd_mean", "mkt_orders_share_wgtd_mean", "indsty_std_mkt_orders_sp500_wgtd_mean", "mkt_orders_sp500_share_wgtd mean")
#
# # net_pi_eq_tbl <- net_pi_eq_df %>%
# #   select(net_pi)


# #########
#
# A Weighted Median net-pi
#
# #########

# Now for all stocks in a column, compute a weighted _median_ of
# each row-weighted net_pi_mcaway (see line 187 above).

# For the column's median, each row is weighted by that row's mcaway_exec_shrs,
# so low-volume rows get the least weight.


net_pi_mcaway_all_wmed <- f605_mktble_df %>%
  summarise(net_pi_mcaway_all_wmed = weighted.median(net_pi_mcaway, mcaway_exec_shrs)
  )

net_pi_mcaway_all_wIQR <- f605_mktble_df %>%
  summarise(net_pi_mcaway_all_wIQR = weighted.IQR(net_pi_mcaway, mcaway_exec_shrs)
  )

# the same weighted median of rows, but just market orders

net_pi_mcaway_morder_wmed <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  summarise(net_pi_mcaway_morder_wmed = weighted.median(net_pi_mcaway, mcaway_exec_shrs)
  )


# weighted median of rows, by order size, just market orders

net_pi_mcaway_morder_size_wmed <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  group_by(order_size) %>%
  summarise(net_pi_mcaway_morder_size_wmed = weighted.median(net_pi_mcaway, mcaway_exec_shrs)
  )


# weighted median of rows, for S&P 500 members, just market orders

net_pi_mcaway_morder_sp500_wmed <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(in_sp500 == T) %>%
  summarise(net_pi_mcaway_morder_sp500_wmed = weighted.median(net_pi_mcaway, mcaway_exec_shrs)
  )

# weighted median of rows, for nyse_listings, just market orders

net_pi_mcaway_morder_nyse_wmed <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(on_nyse = T) %>%
  summarise(net_pi_mcaway_morder_nyse_wmed = weighted.median(net_pi_mcaway, mcaway_exec_shrs)
  )

# weighted median of rows, for nasdaq_listings, just market orders

net_pi_mcaway_morder_nasdaq_wmed <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(on_nasdaq = T) %>%
  summarise(net_pi_mcaway_morder_nasdaq_wmed = weighted.median(net_pi_mcaway, mcaway_exec_shrs)
  )


# assemble some of these alternative measures of net-pi averages in a table:


net_pi_df <- cbind(net_pi_mcaway_morder_wmean, net_pi_mcaway_morder_sp500_wmean, net_pi_mcaway_morder_wmed, net_pi_mcaway_morder_sp500_wmed)

net_pi_df <- net_pi_df %>%
  gather(average, net_pi)

rownames(net_pi_df) <- c("mkt_orders_share_wgtd_mean", "mkt_orders_sp500_share_wgtd mean", "mkt_orders_share_wgtd_median", "mkt_orders_sp500_share_wgtd_median")

net_pi_tbl <- net_pi_df %>%
  dplyr::select(net_pi)


# ##########
#
# GRAPHICS USED IN REPORTS
#
# ##########

# weighted medians and other quantiles of net-pi on market orders, by order_size
df_sz_499 <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr" , order_size == "100-499")

df_sz_499_w <- t(data.frame(weighted.quantile(df_sz_499$net_pi_mcaway, df_sz_499$mcaway_exec_shrs)))
rownames(df_sz_499_w) <- "100-499"
colnames(df_sz_499_w) <- c("0%", "25%", "50%", "75%", "100%")

df_sz_1999 <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr" , order_size == "500-1999")

df_sz_1999_w <- t(data.frame(weighted.quantile(df_sz_1999$net_pi_mcaway, df_sz_1999$mcaway_exec_shrs)))
rownames(df_sz_1999_w) <- "500-1999"
colnames(df_sz_1999_w) <- c("0%", "25%", "50%", "75%", "100%")


df_sz_4999 <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr" , order_size == "2000-4999")

df_sz_4999_w <- t(data.frame(weighted.quantile(df_sz_4999$net_pi_mcaway, df_sz_4999$mcaway_exec_shrs)))
rownames(df_sz_4999_w) <- "2000-4999"
colnames(df_sz_4999_w) <- c("0%", "25%", "50%", "75%", "100%")

df_sz_5000 <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr" , order_size == "5000+")

df_sz_5000_w <- t(data.frame(weighted.quantile(df_sz_5000$net_pi_mcaway, df_sz_5000$mcaway_exec_shrs)))
rownames(df_sz_5000_w) <- "5000+"
colnames(df_sz_5000_w) <- c("0%", "25%", "50%", "75%", "100%")


df_sz_all_w <- data.frame(rbind(df_sz_499_w, df_sz_1999_w, df_sz_4999_w, df_sz_5000_w))
colnames(df_sz_all_w) <- c("0%", "25%", "50%", "75%", "100%")
df_sz_all_w$order_size <- ordered(rownames(df_sz_all_w), levels = c("100-499", "500-1999", "2000-4999", "5000+"))

# weighted mean of net_pi on market orders, by order_size

df_sz_all_wmean <-ddply(f605_mktble_df[f605_mktble_df$order_type == "mkt_ordr",], .(order_size),
                        summarize,
                        wmean=round(weighted.mean(net_pi_mcaway, mcaway_exec_shrs, na.rm=TRUE), 2))

rownames(df_sz_all_wmean) <- c("100-499", "500-1999", "2000-4999", "5000+")

df_sz_all_wmean$order_size<- ordered(rownames(df_sz_all_wmean), levels = c("100-499", "500-1999", "2000-4999", "5000+"))

# draw the plot
net_pi_wmed_morders_sz_bxplt <- ggplot(df_sz_all_w, aes(x = order_size, ymin = `0%`, lower = `25%`,
                                                        middle = `50%`, upper = `75%`, ymax = `100%`)) +
  geom_boxplot(stat = "identity", colour = I("#3366FF")) +
  geom_point(data=df_sz_all_wmean, aes(x=order_size, y=wmean), shape = 23, color = "red",
             size = 3, fill ="white",inherit.aes=FALSE) +
  geom_text(data=df_sz_all_wmean, aes(x=order_size, y=wmean),label = "wgtd-mean", color = "red", hjust = +1.1, size = 4, inherit.aes=FALSE) +
  ggtitle("Net Price-Improvement on market orders, by order size") +
  coord_cartesian(ylim=c(0, 1))



# ########################################
# ########################################
#
# Ratio of effective-spread over quoted-spread
#
# ########################################
# ########################################


# In this section, we create a few columns of variables that we'll use later in our aggregate effective-over-quoted spreads

# for each row, create a variable avg_quoted_spread_mcaway, i.e. ask price minus bid price,
# using our row-wise share-weighted net-price-improvement measure,
# where the denominator was the sum of orders executed both at and away from the market-center
# We multiply the effective-spread by 100, to go from dollars to cents like the net-pi number.
# Adding twice the net_pi gets us to the quoted spread.
# Units are cents per share:

f605_mktble_df <- f605_mktble_df %>%
  mutate(avg_quoted_spread_mcaway = (100*avg_effec_spread + 2*net_pi_mcaway))

# Now for each row, create a variable effective_over_quoted_mc, aka. E/Q,
# using the just-created avg_quoted_spread_mcaway as the denominator:

f605_mktble_df <- f605_mktble_df %>%
  mutate(effective_over_quoted_mcaway = (100* avg_effec_spread / avg_quoted_spread_mcaway))


# ##########
#
# Firm-level averages for Effective-over-Quoted
#
# ##########



# A simple mean 'effective_over_quoted' for all stocks, all months

effective_over_quoted_morder_mcaway_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  summarise(effective_over_quoted_morder_mcaway_mean = mean(effective_over_quoted_mcaway, na.rm = T)
  )

# A simple mean 'effective_over_quoted' for all S&P 500 stocks, all months

effective_over_quoted_sp_morder_mcaway_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(in_sp500 == T) %>%
  summarise(effective_over_quoted_morder_mcaway_mean = mean(effective_over_quoted_mcaway, na.rm = T)
  )

# Simple mean 'effective_over_quoted' by month

effective_over_quoted_morder_mcaway_monthly_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  group_by(date) %>%
  summarise(effective_over_quoted_morder_mcaway_monthly_mean = mean(effective_over_quoted_mcaway, na.rm = T)
  )


# Simple mean 'effective_over_quoted' by order type, all stocks, all order sizes, all months

effective_over_quoted_mcaway_type_mean <- f605_mktble_df %>%
  group_by(order_type) %>%
  summarise(effective_over_quoted_mcaway_type_mean = mean(effective_over_quoted_mcaway, na.rm = T)
  )


# Simple mean 'effective_over_quoted' by order size, all stocks, both order types, all months

effective_over_quoted_morder_mcaway_size_mean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  group_by(order_size) %>%
  summarise(effective_over_quoted_morder_mcaway_size_mean = mean(effective_over_quoted_mcaway, na.rm = T)
  )


# #########
#
# A Weighted-MEAN of the Effective-over-Quoted Spread ratio
# We're probably going to look only at market-orders, so filter for just that order type
#
# #########


# Weighted-MEAN of 'effective_over_quoted' for all stocks, all months, just market orders,
# where each row is weighted by
# the product of its executed shares ('mcaway_exec_shrs', lines 133ff above) AND the quoted spread,
# (my variable 'avg_quoted_spread_mcaway') to adjust for difference between wholesalers' spreads.

effective_over_quoted_morder_mcaway_wmean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  summarise(effective_over_quoted_morder_mcaway_wmean = weighted.mean(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway),na.rm = T)
  )

# mean 'effective_over_quoted' by month, just market orders

effective_over_quoted_morder_monthly_mcaway_wmean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  group_by(date) %>%
  summarise(effective_over_quoted_morder_monthly_mcaway_wmean = weighted.mean(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway),na.rm = T)
  )


# mean 'effective_over_quoted' by order size, all stocks, all months, just market orders

effective_over_quoted_morder_size_mcaway_wmean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  group_by(order_size) %>%
  summarise(effective_over_quoted_morder_size_mcaway_wmean = weighted.mean(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm = T)
  )

# If a holding company's form 605 filing reports on more than one market center:

effective_over_quoted_morder_mcenter_mcaway_wmean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  group_by(market_center) %>%
  summarise(effective_over_quoted_morder_mcenter_mcaway_wmean = weighted.mean(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm = T)
  )


# in S&P 500

effective_over_quoted_morder_sp500_mcaway_wmean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(in_sp500 == TRUE) %>%
  summarise(effective_over_quoted_morder_sp500_mcaway_wmean = weighted.mean(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm = T)
  )


# NOT in S&P 500

effective_over_quoted_morder_notsp500_mcaway_wmean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(in_sp500 == FALSE) %>%
  summarise(effective_over_quoted_morder_notsp500_mcaway_wmean = weighted.mean(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm = T)
  )

# on nyse

effective_over_quoted_morder_nyse_mcaway_wmean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(on_nyse == TRUE) %>%
  summarise(effective_over_quoted_morder_nyse_mcaway_wmean = weighted.mean(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm = T)
  )

# on nasdaq

effective_over_quoted_morder_nasdaq_mcaway_wmean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(on_nasdaq == TRUE) %>%
  summarise(effective_over_quoted_morder_nasdaq_mcaway_wmean = weighted.mean(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm = T)
  )

# in S&P 500 and on nyse

effective_over_quoted_morder_sp_nyse_mcaway_wmean <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(in_sp500 == TRUE) %>%
  filter(on_nyse == TRUE) %>%
  summarise(effective_over_quoted_morder_sp_nyse_mcaway_wmean = weighted.mean(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm = T)
  )

# #########
#
# A Weighted-MEDIAN of the Effective-over-Quoted Spread ratio
# We're probably going to look only at market-orders, so filter for just that order type
#
# #########


# Weighted-MEDIAN 'effective_over_quoted' for all stocks, all months, just market orders,
# where each row is weighted by its executed shares ('mcaway_exec_shrs', lines 133ff above)

effective_over_quoted_morder_mcaway_wmed <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  summarise(effective_over_quoted_morder_mcaway_wmed = weighted.median(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway),na.rm = T)
  )

# median 'effective_over_quoted' by month, just market orders

effective_over_quoted_morder_monthly_mcaway_wmed <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  group_by(date) %>%
  summarise(effective_over_quoted_morder_monthly_mcaway_wmed = weighted.median(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway),na.rm = T)
  )



# median 'effective_over_quoted' by order size, all stocks, all months, just market orders

effective_over_quoted_morder_size_mcaway_wmed <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  group_by(order_size) %>%
  summarise(effective_over_quoted_morder_size_mcaway_wmed = weighted.median(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm = T)
  )

# If a holding company's form 605 filing reports on more than one market center:

effective_over_quoted_morder_mcenter_mcaway_wmed <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  group_by(market_center) %>%
  summarise(effective_over_quoted_morder_mcenter_mcaway_wmed = weighted.median(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm = T)
  )


# in S&P 500

effective_over_quoted_morder_sp500_mcaway_wmed <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(in_sp500 == TRUE) %>%
  summarise(effective_over_quoted_morder_sp500_mcaway_wmed = weighted.median(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm = T)
  )

# NOT in S&P 500

effective_over_quoted_morder_notsp500_mcaway_wmed <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(in_sp500 == FALSE) %>%
  summarise(effective_over_quoted_morder_notsp500_mcaway_wmed = weighted.median(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm = T)
  )


# on nyse

effective_over_quoted_morder_nyse_mcaway_wmed <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(on_nyse == TRUE) %>%
  summarise(effective_over_quoted_morder_nyse_mcaway_wmed = weighted.median(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm = T)
  )

# on nasdaq

effective_over_quoted_morder_nasdaq_mcaway_wmed <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  filter(on_nasdaq == TRUE) %>%
  summarise(effective_over_quoted_morder_nasdaq_mcaway_wmed = weighted.median(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm = T)
  )

# combine these EoQ measures in a table

eoq_df <- data.frame(cbind(effective_over_quoted_morder_mcaway_wmean, effective_over_quoted_morder_sp500_mcaway_wmean, effective_over_quoted_morder_notsp500_mcaway_wmean, effective_over_quoted_morder_mcaway_wmed, effective_over_quoted_morder_sp500_mcaway_wmed, effective_over_quoted_morder_notsp500_mcaway_wmed))

eoq_df<- eoq_df %>%
  gather(average, eoq)

rownames(eoq_df) <- c("Effective-over_Quoted weighted-mean",
                         "S&P 500 Effective-over_Quoted weighted-mean",
                         "nonS&P 500 Effective-over_Quoted weighted-mean",
                         "Effective-over_Quoted weighted-median",
                         "S&P 500 Effective-over_Quoted weighted-median",
                         "non-S&P 500 Effective-over_Quoted weighted-median")

eoq_tbl <- eoq_df %>%
  dplyr::select(eoq)

# ##########
#
# GRAPHICS USED IN EoQ REPORT
#
# ##########

# create a weighting variable

f605_mktble_df <- f605_mktble_df %>%
  mutate(eoq_wgt = (mcaway_exec_shrs * avg_quoted_spread_mcaway))

# weighted medians and other quantiles of net-pi on market orders, by order_size
df_eoq_sz_499 <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr" , order_size == "100-499")

df_eoq_sz_499_w <- t(data.frame(weighted.quantile(df_eoq_sz_499$effective_over_quoted_mcaway, df_eoq_sz_499$eoq_wgt, na.rm=T)))
rownames(df_eoq_sz_499_w) <- "100-499"
colnames(df_eoq_sz_499_w) <- c("0%", "25%", "50%", "75%", "100%")

df_eoq_sz_1999 <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr" , order_size == "500-1999")

df_eoq_sz_1999_w <- t(data.frame(weighted.quantile(df_eoq_sz_1999$effective_over_quoted_mcaway, df_eoq_sz_1999$eoq_wgt, na.rm=T)))
rownames(df_eoq_sz_1999_w) <- "500-1999"
colnames(df_eoq_sz_1999_w) <- c("0%", "25%", "50%", "75%", "100%")


df_eoq_sz_4999 <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr" , order_size == "2000-4999")

df_eoq_sz_4999_w <- t(data.frame(weighted.quantile(df_eoq_sz_4999$effective_over_quoted_mcaway, df_eoq_sz_4999$eoq_wgt, na.rm=T)))
rownames(df_eoq_sz_4999_w) <- "2000-4999"
colnames(df_eoq_sz_4999_w) <- c("0%", "25%", "50%", "75%", "100%")

df_eoq_sz_5000 <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr" , order_size == "5000+")

df_eoq_sz_5000_w <- t(data.frame(weighted.quantile(df_eoq_sz_5000$effective_over_quoted_mcaway, df_eoq_sz_5000$eoq_wgt, na.rm=T)))
rownames(df_eoq_sz_5000_w) <- "5000+"
colnames(df_eoq_sz_5000_w) <- c("0%", "25%", "50%", "75%", "100%")


df_eoq_sz_all_w <- data.frame(rbind(df_eoq_sz_499_w, df_eoq_sz_1999_w, df_eoq_sz_4999_w, df_eoq_sz_5000_w))
colnames(df_eoq_sz_all_w) <- c("0%", "25%", "50%", "75%", "100%")
df_eoq_sz_all_w$order_size <- ordered(rownames(df_eoq_sz_all_w), levels = c("100-499", "500-1999", "2000-4999", "5000+"))

# weighted mean of net_pi on market orders, by order_size

df_eoq_sz_all_wmean <-ddply(f605_mktble_df[f605_mktble_df$order_type == "mkt_ordr",], .(order_size),
                            summarize,
                            wmean=round(weighted.mean(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway), na.rm=TRUE), 2))

rownames(df_eoq_sz_all_wmean) <- c("100-499", "500-1999", "2000-4999", "5000+")

df_eoq_sz_all_wmean$order_size<- ordered(rownames(df_eoq_sz_all_wmean), levels = c("100-499", "500-1999", "2000-4999", "5000+"))

# draw the plot
eoq_wmed_morders_sz_bxplt <- ggplot(df_eoq_sz_all_w, aes(x = order_size, ymin = `0%`, lower = `25%`,
                                                         middle = `50%`, upper = `75%`, ymax = `100%`)) +
  geom_boxplot(stat = "identity", colour = I("#3366FF")) +
  geom_point(data=df_eoq_sz_all_wmean, aes(x=order_size, y=wmean), shape = 23, color = "red",
             size = 3, fill ="white",inherit.aes=FALSE) +
  geom_text(data=df_eoq_sz_all_wmean, aes(x=order_size, y=wmean),label = "wgtd-mean", color = "red", hjust = +1.1, size = 4, inherit.aes=FALSE) +
  ggtitle("Effective-over-Quoted Spread Ratios on market orders, by order size") +
  coord_cartesian(ylim=c(0, 1))



# ##########
#
# Ticker-wise calculations
#
# ##########

# ----------
#
# A table grouped by ticker
#
# ----------

# Create a new table, f605_mktble_ticker_df, with each row the sum or the weighted-mean
# of the relevant variables for each ticker.

# Filter first, to include only market_orders


f605_morder_t_df <- f605_mktble_df %>%
  filter(order_type == "mkt_ordr") %>%
  group_by(ticker, order_type) %>%
  summarise(mcaway_exec_shrs_t = sum(mcaway_exec_shrs),
            avg_effec_spread_t = weighted.mean(avg_effec_spread, mcaway_exec_shrs),
            px_improved_shrs_t = sum(px_improved_shrs),
            px_improved_avg_amt_t = weighted.mean(px_improved_avg_amt, px_improved_shrs),
            outside_quote_shrs_t = sum(outside_quote_shrs),
            outside_quote_avg_amt_t = weighted.mean(outside_quote_avg_amt, outside_quote_shrs),
            effective_over_quoted_mcaway_t = weighted.mean(effective_over_quoted_mcaway, (mcaway_exec_shrs * avg_quoted_spread_mcaway))
  )

# By grouping at the ticker level, we've lost information on order_size (and order_type), but
# what about membership in the S&P500 or listing on the NYSE or Nasdaq ?
# Add that membership information, from the f605_mktble_df table.

f605_mktble_member_df <- f605_mktble_df[, c(4, 5, 27, 29:30)] %>%
  filter(order_type == "mkt_ordr") %>%
  unique(MARGIN = 1)

f605_morder_ticker_df <- inner_join(f605_morder_t_df, f605_mktble_member_df, by = "ticker")

# Now create a row-wise share-weighted net_pi_mcaway_t variable for each ticker.
# premultiply by 100, to get values in cents not dollars.

f605_morder_ticker_df <- f605_morder_ticker_df %>%
  mutate(net_pi_mcaway_t = 100 * (((px_improved_shrs_t * px_improved_avg_amt_t) - (outside_quote_shrs_t * outside_quote_avg_amt_t)) / mcaway_exec_shrs_t)
  )

# ##########
#
# TICKER-LEVEL net-pi's
#
# ##########

# What's the un-weighted mean for these ticker-level groups of market orders?

net_pi_mcaway_mean_t <- mean(f605_morder_ticker_df$net_pi_mcaway_t, na.rm = T)

# How about a weighted mean for these ticker-level groups, weighted by each ticker's mcaway_exec_shrs_t ? Is it the same as the other versions of a weighted means ?

net_pi_mcaway_wmean_t <- weighted.mean(f605_morder_ticker_df$net_pi_mcaway_t, f605_morder_ticker_df$mcaway_exec_shrs_t, na.rm = T)

# a weighted mean for the S&P 500 member tickers

f605_morder_ticker_sp500_df <- f605_morder_ticker_df %>%
  filter(in_sp500 == TRUE)

net_pi_mcaway_sp500_wmean_t <- weighted.mean(f605_morder_ticker_sp500_df$net_pi_mcaway_t, f605_morder_ticker_sp500_df$mcaway_exec_shrs_t, na.rm = T)

# Now how about an un-weighted median for these ticker-level groupings ?

net_pi_mcaway_med_t <- median(f605_morder_ticker_df$net_pi_mcaway_t, na.rm=T)

# An un-weighted median for these ticker-level groupings of just S&P 500 member stocks ?

net_pi_mcaway_sp500_med_t <- median(f605_morder_ticker_sp500_df$net_pi_mcaway_t, na.rm=T)

# now compute a (column_wise) weighted-median net_pi of these ticker-level groupings
net_pi_mcaway_wmed_t <- weighted.median(f605_morder_ticker_df$net_pi_mcaway_t, f605_morder_ticker_df$mcaway_exec_shrs_t, na.rm = T)


# What's the weighted-median, net-pi for tickers in the S&P 500 ?

net_pi_mcaway_sp500_wmed_t <- weighted.median(f605_morder_ticker_sp500_df$net_pi_mcaway_t, f605_morder_ticker_sp500_df$mcaway_exec_shrs_t, na.rm = T)


# E/Q measures from ticker table

# a weighted mean E/Q for the S&P 500 member tickers

f605_morder_ticker_sp500_df <- f605_morder_ticker_df %>%
  filter(in_sp500 == TRUE)

eoq_mcaway_sp500_wmean_t <- weighted.mean(f605_morder_ticker_sp500_df$effective_over_quoted_mcaway_t, f605_morder_ticker_sp500_df$mcaway_exec_shrs_t, na.rm = T)

# combine these alternative measures in a table

net_pi_df_t <- data.frame(cbind(net_pi_mcaway_wmean_t, net_pi_mcaway_sp500_wmean_t, net_pi_mcaway_wmed_t, net_pi_mcaway_sp500_wmed_t))

net_pi_df_t <- net_pi_df_t %>%
  gather(average, net_pi)

rownames(net_pi_df_t) <- c("share-wgtd mean", "S&P 500 stks share_wgtd mean",
                           "share-wgtd median", "S&P 500 stks share-wgtd median")

net_pi_tickers <- net_pi_df_t %>%
  dplyr::select(net_pi)


# ##########
#
# Save .RData files
#
# ##########

# save the 'f605_mktble_df' and 'f605_morder_ticker_df' data frame objects
# for later rendering into a markdown document

save(f605_mktble_df, file = "./analysis/f605_mktble_df.RData")

save(f605_morder_ticker_df, file = "./analysis/f605_morder_ticker_df.RData")

# ##########
# set the working directory, for Windows or Mac
os_name = Sys.info()[['sysname']]
if(os_name == "Windows"){
  setwd("C:/rule605")
}else if(os_name =="Darwin"){  # OS X
  setwd("~/rule605")
}else{                         # Linux
  stop("I'm a penguin.")
}

# end
