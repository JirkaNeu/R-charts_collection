library(readxl)
library(writexl)
library(Cairo)

#https://r-resources.massey.ac.nz/rgcookbook/
#https://r-charts.com/




name_of_sheet = "Monitoring"
colnum_of_dates = 4
colnum_of_data = 11
print_plot = F
print_xlsx = F

substitute_df = function(){
  message("no data...choose file")
  src_jne = read_xlsx(file.choose(), sheet = name_of_sheet, col_names = T)
  return(src_jne)
}

src_jne = tryCatch({
  setwd(path_of_file)
  read_xlsx(name_of_file, sheet = name_of_sheet, col_names = T)
},
error = function(e) {
  cat("Error: ", conditionMessage(e), "\n")
  substitute_df()
},
warning = function(w) {
  cat("Warning: ", conditionMessage(w), "\n")
  src_jne = FALSE
}
)

core_data = subset(src_jne, select = c(colnum_of_dates, colnum_of_data))
#core_data = data.frame(src_jne[colnum_of_dates], src_jne[colnum_of_data])
colnames(core_data) = c("datum", "gfs")

Daten23 = subset(core_data, format(as.Date(core_data$datum),"%Y") == 2023, select = c(2))
Daten24 = subset(core_data, format(as.Date(core_data$datum),"%Y") == 2024, select = c(2))
Daten25 = subset(core_data, format(as.Date(core_data$datum),"%Y") == 2025, select = c(2))
Daten26 = subset(core_data, format(as.Date(core_data$datum),"%Y") == 2026, select = c(2))

#gfsdata_df = src_jne[colnum_of_data]
AlleJahre = src_jne[colnum_of_data]



get_me_my_results = function(gfsdata_df){

  gfsdata_df = cbind(gfsdata_df, gf1=NA, gf2=NA, gf3=NA, gf4=NA, gf5=NA, gf6=NA)
  
  for (irow in 1:nrow(gfsdata_df)){
    getnums = as.character(gfsdata_df[irow, 1])
    if(is.na(getnums)){getnums = "ignore_me"}
    getnums = gsub(" ", "", getnums)
    getnums = strsplit(getnums, ",", fixed=TRUE)[[1]]
    
    do_sort = T
    for (i in 1:length(getnums)){
      if(getnums[i] != "1" & getnums[i] != "2" & getnums[i] != "3" & getnums[i] != "4" & getnums[i] != "5" & getnums[i] != "6"){
        do_sort = F
      }
    }
    
    if (do_sort == T){
      for (i in 1:length(getnums)){
        switch(getnums[i],
               "1" = {gfsdata_df$gf1[irow] = 1},
               "2" = {gfsdata_df$gf2[irow] = 1},
               "3" = {gfsdata_df$gf3[irow] = 1},
               "4" = {gfsdata_df$gf4[irow] = 1},
               "5" = {gfsdata_df$gf5[irow] = 1},
               "6" = {gfsdata_df$gf6[irow] = 1},
               {NA}#default
        )
      }
    }
  }
  
  sum_gfs = NULL
  for (i in 1:6){
    sum_gfs = append(sum_gfs, sum(na.omit(gfsdata_df[i+1])))
  }
  
  return(sum_gfs)
}


result_gfs = get_me_my_results(AlleJahre)

all_data = c(39, 53, 25, 21, 31, 22)

result_gfs = all_data

#------ base plot ------#
light = "#6BA1BB"
lighter = "#d3f3f5"
dark = "#005478"
Header = "Verteilung der ausgewählten Variablen von 2023 bis 2025"
bars = "Column"

p0 = barplot(result_gfs,
             ylim = c(0, max(result_gfs)+5),
             col = light,
             main = Header,
             xlab = "",
             ylab = "",
             horiz = F,
             #names.arg = paste(bars, 1:6)
)
text(p0, y = result_gfs+2, labels = c(as.character(result_gfs)))
text(p0, y = 2, labels = c(paste("GF", 1:6)))
if (print_plot == T){
  dev.print(device = png, filename = 'gfs_plot0.png', width = 1000)
  dev.off()
}


#------ ggplot ------#
library(ggplot2)
result_gfs_df = data.frame(GF = c(1:6), freq = result_gfs, xlabs = c(paste("GF", 1:6)))
long_labels = c("GF 1: Varible eins",
                "GF 2: Varible zwei",
                "GF 3: Varible drei",
                "GF 4: Varible vier",
                "GF 5: Varible fünf",
                "GF 6: Varible eins")
result_gfs_df = cbind(result_gfs_df, xlabs_long = long_labels)

p1 = ggplot(result_gfs_df, aes(x = xlabs, y = freq)) + 
  geom_bar(stat = "identity", color = "black", fill=light) +
  ylim(0, max(result_gfs)+2) +
  labs(title=Header) +
  geom_text(aes(label=freq), vjust = -1) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    #axis.line.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_blank(),
    axis.title.x=element_blank(), #remove axis title
    axis.title.y=element_blank(), #remove axis title
    #axis.text.x=element_blank(),  #remove axis labels
    axis.text.x = element_text(hjust = 0.5, vjust = 0.5, size = 10),
    axis.text.y=element_blank(),  #remove axis labels
    #axis.ticks.x=element_blank(),  #remove axis ticks
    axis.ticks.y=element_blank()  #remove axis ticks
  )
plot(p1)
if (print_plot == T){
  ggsave("gfs_plot1.png")
}

p2 = ggplot(result_gfs_df, aes(x = reorder(GF, +freq), y = freq)) + 
  geom_bar(stat = "identity", color = "black", fill=light, width = c(.8, .8, .8, .8, .8, .8)) +
  labs(title=Header) +
  geom_text(aes(label=freq), hjust = -1) + # Values outside
  geom_label(aes(label = long_labels), hjust = 1, nudge_y = -.5, fill = lighter, label.size = 0, size = 3) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x=element_blank(), #remove axis title
    axis.title.y=element_blank(), #remove axis title
    axis.text.x=element_blank(),  #remove axis labels
    axis.text.y=element_blank(),  #remove axis labels
    axis.ticks.x=element_blank(),  #remove axis ticks
    axis.ticks.y=element_blank()  #remove axis ticks
  ) + 
  coord_flip()
plot(p2)
if (print_plot == T){
  ggsave("gfs_plot2.png")
}

p3 = ggplot(result_gfs_df, aes(x = reorder(GF, +freq), y = freq)) + 
  geom_bar(stat = "identity", color = "black", fill=light, width = c(.8, .8, .8, .8, .8, .8)) +
  labs(title=Header) +
  geom_text(aes(label=freq), hjust = -1) + # Values outside
  geom_text(aes(label = long_labels), hjust = 1, nudge_y = -.5, col = "white", size = 3) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.title.x=element_blank(), #remove axis title
    axis.title.y=element_blank(), #remove axis title
    axis.text.x=element_blank(),  #remove axis labels
    axis.text.y=element_blank(),  #remove axis labels
    axis.ticks.x=element_blank(),  #remove axis ticks
    axis.ticks.y=element_blank()  #remove axis ticks
  ) + 
  coord_flip()
plot(p3)
if (print_plot == T){
  ggsave("gfs_plot3.png")
}

if (print_xlsx == T){
  output_jne = "gfs_jne.xlsx"
  write_xlsx(result_gfs_df, output_jne)
} else {
  print("No xlsx-output saved.")
}


#-----> make new df <-----#
group_results_df = rbind(
  data.frame(Jahr = rep("2023", 6), GFs = c(paste("GF", 1:6)), Counts = get_me_my_results(Daten23)),
  data.frame(Jahr = rep("2024", 6), GFs = c(paste("GF", 1:6)), Counts = get_me_my_results(Daten24)),
  data.frame(Jahr = rep("2025", 6), GFs = c(paste("GF", 1:6)), Counts = get_me_my_results(Daten25))
)

res23 = c(7, 9, 6, 4, 6, 6)

#-----> make new df <-----#
Header = "Verteilung der ausgewählten Gestaltungsfelder (GF) pro Jahr"

p4 = ggplot(group_results_df, aes(x = GFs, y = Counts, fill = Jahr)) +
  geom_col(position = "dodge", color = "black") +
  ylim(0, max(group_results_df$Counts) + 2) +
  labs(title=Header) +
  geom_text(aes(GFs, label=Counts), position = position_dodge(width = .9), vjust = -.5) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    #axis.line.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_blank(),
    axis.title.x=element_blank(), #remove axis title
    axis.title.y=element_blank(), #remove axis title
    #axis.text.x=element_blank(),  #remove axis labels
    axis.text.x = element_text(hjust = 0.5, vjust = 0.5, size = 10),
    axis.text.y=element_blank(),  #remove axis labels
    #axis.ticks.x=element_blank(),  #remove axis ticks
    axis.ticks.y=element_blank(),  #remove axis ticks
    #--> legend:
    legend.position = c(.90, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )+
  scale_fill_manual(name = "Jahr", labels = c("2023 (ab Sept.)", "2024", "2025"), values = c(rgb(.3, 0.6, 1), rgb(0.9, 0.4, 0.6), rgb(0.4, 0.8, 0.6)))

plot(p4)
if (print_plot == T){
  ggsave("gfs_plot4.png")
}




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



#stop("stop")

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-> Cairo <->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
require(Cairo)
CairoWin()









detach(Cairo)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<->-------<->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#




