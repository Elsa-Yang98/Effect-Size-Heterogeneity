library(ggplot2)
#sim_name="Hiv_fixed"
sim_name="Hiv_varied"
#simulation <- read.table("C:/Users/wanghua/Desktop/Fall2020/Research/Hiv_fdp_tpp_fixed.txt")
#simulation <- read.table("C:/Users/wanghua/Desktop/Fall2020/Research/Hiv_fdp_tpp_fixed_with_noise.txt")
#simulation <- read.table("C:/Users/wanghua/Desktop/Fall2020/Research/Hiv_fdp_tpp_fixed_with_noise_10.txt")
#simulation <- read.table("C:/Users/wanghua/Desktop/Fall2020/Research/Hiv_fdp_tpp_fixed_with_noise_50.txt")


#simulation <- read.table("C:/Users/wanghua/Desktop/Fall2020/Research/Hiv_fdp_tpp_varied.txt")
#simulation <- read.table("C:/Users/wanghua/Desktop/Fall2020/Research/Hiv_fdp_tpp_varied_with_noise.txt")
#simulation <- read.table("C:/Users/wanghua/Desktop/Fall2020/Research/Hiv_fdp_tpp_varied_with_noise_10.txt")
#simulation <- read.table("C:/Users/wanghua/Desktop/Fall2020/Research/Hiv_fdp_tpp_varied_with_noise_50.txt")

#simulation <- read.table("Hiv_fdp_tpp_fixed.txt")
simulation <- read.table("Hiv_fdp_tpp_varied.txt")
#theory <- read.table("C:/Users/24732/Desktop/d=1,e=0.2.txt")
#Upper <- theory[,c(1,2)]
#Lower <- theory[,c(3,4)]
#Upper$Settings <- "Upper"
#Lower$Settings <- "Lower"
#colnames(Lower) <- c("V1","V2","Settings")
`Strongest` <- simulation[,c(1,2)]
`Strongest`$Settings <- "Strongest"
`Less Strong` <- simulation[,c(3,4)]
`Less Strong`$V5 <- "Strong"
colnames(`Less Strong`) <- c("V1","V2","Settings")
`Weaker` <- simulation[,c(5,6)]
`Weaker`$V7 <- "Weak"
colnames(`Weaker`) <- c("V1","V2","Settings")
`Weakest` <- simulation[,c(7,8)]
`Weakest`$V9 <- "Weakest"
colnames(`Weakest`) <- c("V1","V2","Settings")

##Verification
#v_upper <- rbind(`Strongest`, Upper, `Weakest`, Lower)

# ggplot(data=v_upper, aes(x=V1, y=V2, group=Settings,)) +
#   geom_line(size = 1.6, aes(color=Settings)) +
#   xlab("xlab") + ylab("ylab") +
#   theme(axis.text = element_text(size = 30),
#         axis.title =element_text(size = 30,face="bold"),
#         legend.justification = c(0, 1), legend.position = c(0, 1),
#         legend.box.margin=margin(c(6,6,6,6)),
#         legend.title=element_text(size=30), 
#         legend.text=element_text(size=27),
#         legend.key.size = unit(2, "cm")) +
#   ylim(0,0.2) + xlab("TPP") + ylab("FDP") 

##4Simulations
p1 <- rbind(`Weakest`)
p1$Settings <- factor(p1$Settings, levels = c("Weakest"))

p2 <- rbind(`Weaker`,`Weakest`)
p2$Settings <- factor(p2$Settings, levels = c( "Weakest", "Weak"))

p3 <- rbind(`Less Strong`,`Weaker`,`Weakest`)
p3$Settings <- factor(p3$Settings, levels = c("Weakest", "Weak", "Strong"))

p4 <- rbind(`Strongest`,`Less Strong`,`Weaker`,`Weakest`)
p4$Settings <- factor(p4$Settings, levels = c("Weakest", "Weak", "Strong", "Strongest"))



# ggplot(data=p1, aes(x=V1, y=V2, group=Settings,)) +
#   geom_line(size = 1.6, aes(color=Settings)) +
#   xlab("xlab") + ylab("ylab") +
#   theme(axis.text = element_text(size = 30),
#         axis.title =element_text(size = 30,face="bold"),
#         legend.justification = c(0, 1), legend.position = c(0, 1),
#         legend.box.margin=margin(c(6,6,6,6)),
#         legend.title=element_text(size=30), 
#         legend.text=element_text(size=27),
#         legend.key.size = unit(2, "cm")) +
#   ylim(0,0.2) + xlab("TPP") + ylab("FDP")  +  
#   scale_color_manual(values=c("dodgerblue4"))
# 
# ggplot(data=p2, aes(x=V1, y=V2, group=Settings,)) +
#   geom_line(size = 1.6, aes(color=Settings)) +
#   xlab("xlab") + ylab("ylab") +
#   theme(axis.text = element_text(size = 30),
#         axis.title =element_text(size = 30,face="bold"),
#         legend.justification = c(0, 1), legend.position = c(0, 1),
#         legend.box.margin=margin(c(6,6,6,6)),
#         legend.title=element_text(size=30), 
#         legend.text=element_text(size=27),
#         legend.key.size = unit(2, "cm")) +
#   ylim(0,0.2) + xlab("TPP") + ylab("FDP") +  
#   scale_color_manual(values=c("dodgerblue4", "skyblue"))
# 
# ggplot(data=p3, aes(x=V1, y=V2, group=Settings,)) +
#   geom_line(size = 1.6, aes(color=Settings)) +
#   xlab("xlab") + ylab("ylab") +
#   theme(axis.text = element_text(size = 30),
#         axis.title =element_text(size = 30,face="bold"),
#         legend.justification = c(0, 1), legend.position = c(0, 1),
#         legend.box.margin=margin(c(6,6,6,6)),
#         legend.title=element_text(size=30), 
#         legend.text=element_text(size=27),
#         legend.key.size = unit(2, "cm")) +
#   ylim(0,0.2) + xlab("TPP") + ylab("FDP") +  
#   scale_color_manual(values=c("dodgerblue4", "skyblue", "tomato"))


p=ggplot(data=p4, aes(x=V1, y=V2, group=Settings)) +
  geom_line(size = 1.6, aes(color=Settings)) +
  xlab("xlab") + ylab("ylab") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 30),
        axis.title =element_text(size = 30),
        legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin=margin(c(2,2,2,2)),
        legend.title=element_text(size=25), 
        legend.text=element_text(size=25),
        legend.key.size = unit(2, "cm")) +
  ylim(0,0.5) + xlab("TPP") + ylab("FDP") + scale_color_manual(values=c("dodgerblue4", "skyblue", "tomato", "tomato4")) +
  #ggtitle("Fixed-X HIV")
  #ggtitle("Fixed-X HIV with noise")
  #ggtitle("Fixed-X HIV with noise N(0,10^2)")
  #ggtitle("Fixed-X HIV with noise N(0,50^2)")

  ggtitle("Varied-X HIV")
  #ggtitle("Varied-X HIV with noise")
  #ggtitle("Varied-X HIV with noise N(0,10^2)")
  #ggtitle("Varied-X HIV with noise N(0,50^2)")
ggsave(paste0(sim_name,".pdf"), 
       width = 10, height = 8, units = "in")

