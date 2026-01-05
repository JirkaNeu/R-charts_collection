library(fmsb)


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


#2do:
#*try cairo-Pac
#*change sorting 