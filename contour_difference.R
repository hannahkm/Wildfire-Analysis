library(ggplot2)

#comparison of the distribution of points for active/inactive 
commonTheme=list(labs(color="Density",fill="Density",
                      x="Tmax",
                      y="Qa"),
                 theme_bw(),
                 theme(legend.position=c(0.01,0.99),
                       legend.justification=c(0,1)))

hist_top <- ggplot()+geom_density(aes(x=X1[,1], color="red"))+
  geom_density(aes(x=Y1[,1], color="darkblue")) +
  theme(legend.position = "none",          
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'white'))

hist_right <- ggplot()+geom_density(aes(x=X1[,2], color="red"))+
  geom_density(aes(x=Y1[,2], color="darkblue"))+
  coord_flip()+
  theme(legend.position = "none",          
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'white'))

empty <- ggplot() + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))

plot3 <- ggplot() +
  geom_point(data=X1, aes(X1[,1], X1[,2]), shape=1, colour = "red", size=0.2) + 
  geom_point(data=Y1, aes(Y1[,1], Y1[,2]), shape=1, colour = "blue", size=0.2) + 
  guides(alpha="none") + xlim(round(min(Y1[,1], X1[,1])-1),round(max(Y1[,1],X1[,1]))+1)+
  ylim(round(min(Y1[,2],X1[,2])-1),round(max(Y1[,2],X1[,2]))+1)+
  commonTheme

grid.arrange(hist_top, empty, plot3, hist_right, 
             ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

result_t2mmax <- ks.test(X1[,1],as.numeric(Y1[,1]))
result_qv2m <- ks.test(X1[,2],as.numeric(Y1[,2]))

X <- rbind(X1[,1, drop = FALSE],Y1[,1, drop = FALSE])
Y <- rbind(X1[,2, drop = FALSE],Y1[,2, drop = FALSE])

plot4 <- ggplot(rbind(data.frame(X1, group="active"), data.frame(Y1, group="inactive")), 
                aes(x=t2mmax,y=qv2m)) + 
  geom_density2d(aes(color=group), size=0.5, contour=TRUE) +
  scale_color_manual(values=c("active"="red", "inactive"="blue")) +
  #scale_fill_manual(values=c("active" = "red", "inactive"="white")) + 
  theme_minimal() +
  xlim(round(min(Y1[,1], X1[,1])-1),round(max(Y1[,1],X1[,1]))+1) + 
  ylim(round(min(Y1[,2],X1[,2])-1),round(max(Y1[,2],X1[,2]))+1) +
  coord_cartesian(xlim = c(round(min(Y1[,1], X1[,1])-1),round(max(Y1[,1],X1[,1]))+1), 
    ylim = c(round(min(Y1[,2],X1[,2])-1),round(max(Y1[,2],X1[,2]))+1)) +
  theme(legend.position="none")


ggplot(X1, aes(x=X1,y=Y1)) + 
  stat_density2d(data=X1,contour=TRUE) + 
  xlim(-3,3) + ylim(-3,3) + 
  stat_density2d(data=Y1,contour=TRUE)
