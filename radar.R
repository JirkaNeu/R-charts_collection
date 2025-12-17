library(fmsb)


touren = c(
  Berlin = 7,
  Hamburg = 8,
  Frankfurt = 4,
  Köln = 12,
  München = 2
  )


all_max = sum(touren)
all_min = 0

all_t_df = as.data.frame(rbind(all_max, all_min, touren))





radarchart(all_t_df,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           cglwd = 1,       # Grid line width
           plty = 1,        # Area line type
           pcol = 4,        # Area line color
           plwd = 2,        # Area line Width
           pfcol = rgb(0, 0.4, 1, 0.25)        # Area line fill and alpha
           )

#2do:
#*try cairo-Pac
#*change sorting 