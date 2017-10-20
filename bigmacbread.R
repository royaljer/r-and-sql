attach(ubs)

DAsBread <- DAs * Bread
DAsrice <- DAs * Rice
DAsvac <- DAs * Vac
DEmvac <- DEm * Vac
DSaBread <- DSa * Bread

bigmacbread.lm <- lm(Bigmac ~ DAs + DEm + DSa + Bread + Wage + Rice + Vac + DAsrice + DAsvac + DEmvac + DAsBread
                     + DSaBread)

mwage <- mean(Wage)
mvac <- mean(Vac)
mrice <- mean(Rice)

bigmacbetas <- coef(summary(bigmacbread.lm))[1:13]

breadeffect <- bigmacbetas[1] + bigmacbetas[2] + bigmacbetas[3] + 
  bigmacbetas[4] + (bigmacbetas[5] * Bread) + (bigmacbetas[6] * mwage) +
  (bigmacbetas[7] * mrice) + (bigmacbetas[8] * mvac) + (bigmacbetas[9] * DAs * mrice) +
  (bigmacbetas[10] * DAs * mvac) + (bigmacbetas[11] * DEm * mvac) + 
  (bigmacbetas[12] * DAs * Bread) + (bigmacbetas[13] * DSa * Bread)

breadeffect0 <- breadeffect[DAs == 0 & DEm == 0 & DSa == 0]
breadeffect1 <- breadeffect[DAs == 1 & DEm == 0 & DSa == 0]
breadeffect2 <- breadeffect[DAs == 0 & DEm == 1 & DSa == 0]
breadeffect3 <- breadeffect[DAs == 0 & DEm == 0 & DSa == 1]

NWaline <- data.frame(sort(Bread[DAs==0 & DEm==0 & DSa==0]), breadeffect0[order(Bread[DAs==0 & DEm==0 & DSa==0])])
DAsline <- data.frame(sort(Bread[DAs==1 & DEm==0 & DSa==0]), breadeffect1[order(Bread[DAs==1 & DEm==0 & DSa==0])])
DEmline <- data.frame(sort(Bread[DAs==0 & DEm==1 & DSa==0]), breadeffect2[order(Bread[DAs==0 & DEm==1 & DSa==0])])
DSaline <- data.frame(sort(Bread[DAs==0 & DEm==0 & DSa==1]), breadeffect3[order(Bread[DAs==0 & DEm==0 & DSa==1])])

colnames(NWaline) <- c("Bread", "effect")
colnames(DAsline) <- c("Bread", "effect")
colnames(DEmline) <- c("Bread", "effect")
colnames(DSaline) <- c("Bread", "effect")

ggplot(data = bbef, aes(x = Bread, y = breadeffect, fill = colour)) + ylab("Log. Minutes To Buy One BigMac") + 
  xlab("Log. Minutes To Buy 1kg Bread ") +
  ggtitle("Effect of Bread Price on BicMac Price, by Region") +
  geom_line(data = NWaline, aes(Bread, effect), size = 1, colour = "blue") +
  geom_line(data = DAsline, aes(Bread, effect), size = 1, colour = "red") +
  geom_line(data = DEmline, aes(Bread, effect), size = 1, colour = "orange") +
  geom_line(data = DSaline, aes(Bread, effect), size = 1, colour = "green")


