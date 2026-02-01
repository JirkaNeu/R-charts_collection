library(fmsb)
library(Cairo)


Daten = c(
  Berlin = 7,
  Hamburg = 8,
  Frankfurt = 4,
  Köln = 12,
  München = 2,
  Dresden = 15
)

Daten2 = c(
  Berlin = 3,
  Hamburg = 12,
  Frankfurt = 7,
  Köln = 10,
  München = 8,
  Dresden = 5
)



#all_max = sum(Daten)
all_max = max(Daten) + 2
all_min = 0

all_d_df = as.data.frame(rbind(all_max, all_min, Daten))

all_d_df = rbind(all_d_df, Daten2)

farben = c(rgb(0, 0.4, 1, 0.4),
           rgb(1, 0, 0.5, 0.4))

radarchart(all_d_df,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           cglwd = 1,       # Grid line width
           plty = 1,        # Area line type
           pcol = 4,        # Area line color
           plwd = 2,        # Area line Width
           pfcol = farben   # Area line fill and alpha
)

legend("right",
       legend = c("Daten früher", "Daten später"),
       bty = "n", pch = 20, col = farben,
       text.col = "grey25", pt.cex = 2)


rm(list = ls())
#stop("stop")


set.seed(9)
days1 = sample(c(5:29), size = 7, replace = T)
set.seed(13)
days2 = sample(c(11:39), size = 7, replace = T)
set.seed(17)
days3 = sample(c(11:39), size = 7, replace = T)
set.seed(NULL)

days_23 = days1 |> rev()
days_24 = days2 |> rev()
days_25 = days3 |> rev()

all_max = max(c(days_23, days_24, days_25))
all_min = 0
all_d_df = as.data.frame(rbind(all_max, all_min, days_23, days_24, days_25))
colnames(all_d_df) = c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So") |> rev()

############### Farben ###############
farben1 = c(rgb(0, 0.4, 1, 0.8),
            rgb(0.8, 0.2, 0.5, 0.8),
            rgb(0.2, 0.6, 0.4, 0.8)
)

farben2 = c(rgb(0, 0.4, 1, 0.2),
            rgb(0, 0.4, 1, 0.2),
            rgb(0, 0.4, 1, 0.2)
)

farben3 = c(rgb(0, 0.4, 1, 0.2),
            rgb(0.8, 0.2, 0.5, 0.2),
            rgb(0.2, 0.6, 0.4, 0.2)
)
###############--------###############

Header = "Nutzung pro Wochentag"



radarchart(all_d_df,
           axistype = 1,
           axislabcol="grey35", #Grid label color
           caxislabels=seq(0, all_max, round(all_max/4, 1)), #Grid label manually
           #caxislabels=seq(0, 20, 5), #Grid label manually
           vlcex = 1.2, # Label width
           title = Header,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           cglwd = 1,       # Grid line width
           plty = c(5, 1, 1),        # Area line type
           #pcol = 4,        # Area line color
           pcol = farben1,
           #plwd = 2,        # Area line Width
           plwd = c(1, 2, 3),        # Area line Width
           pfcol = farben3   # Area line fill and alpha
)
#legend("right",
legend(x = .65, y = 1.3,
       legend = c("2023 (ab Aug.)", "2024", "2025"),
       bty = "n", pch = 20, col = farben1,
       text.col = "grey25", pt.cex = 2)




#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-> Cairo <->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
require(Cairo)
CairoWin()


radarchart(all_d_df,
           axistype = 1,
           axislabcol="grey35", #Grid label color
           caxislabels=seq(0, all_max, round(all_max/4, 1)), #Grid label manually
           #caxislabels=seq(0, 20, 5), #Grid label manually
           vlcex = 1.2, # Label width
           title = Header,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           cglwd = 1,       # Grid line width
           plty = c(5, 1, 1),        # Area line type
           #pcol = 4,        # Area line color
           pcol = farben1,
           #plwd = 2,        # Area line Width
           plwd = c(1, 2, 3),        # Area line Width
           pfcol = farben3   # Area line fill and alpha
)
#legend("right",
legend(x = .65, y = 1.3,
       legend = c("2023 (ab Aug.)", "2024", "2025"),
       bty = "n", pch = 20, col = farben1,
       text.col = "grey25", pt.cex = 2)


detach(package:Cairo)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<->-------<->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#










