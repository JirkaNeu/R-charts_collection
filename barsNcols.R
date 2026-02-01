
library(writexl)
library(Cairo)

print_plot = F
print_xlsx = F


res23 = c(7, 9, 6, 4, 6, 6)
res24 = c(12, 16, 12, 11, 14, 10)
res25 = c(18, 26, 7, 6, 10, 6)

res_all = res23+res24+res25


result_gfs = res_all

#------ base plot ------#
light = "#6BA1BB"
lighter = "#d3f3f5"
dark = "#005478"
Header = "Verteilung der Variablen (GF1 bis GF6) von 2023 bis 2025"
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
long_labels = c("GF 1: 1st varible",
                "GF 2: 2nd varible",
                "GF 3: 3rd varible",
                "GF 4: 4th varible",
                "GF 5: 5th varible",
                "GF 6: 6th varible")
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
  data.frame(Jahr = rep("2023", 6), GFs = c(paste("GF", 1:6)), Counts = res23),
  data.frame(Jahr = rep("2024", 6), GFs = c(paste("GF", 1:6)), Counts = res24),
  data.frame(Jahr = rep("2025", 6), GFs = c(paste("GF", 1:6)), Counts = res25)
)
#-----> make new df <-----#


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




