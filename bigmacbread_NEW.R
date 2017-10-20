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

dfbef$Region <- names(dfbef[3:5])[apply(dfbef[3:5], 1, match, x = 1)]
dfbef$Region[is.na(dfbef$Region)] <- "NAw"

ggplot(data = dfbef, aes(x = Bread, y = breadeffect, group = Region, colour = Region)) + ylab("Log. Minutes To Buy One BigMac") + 
  xlab("Log. Minutes To Buy 1kg Bread ") + geom_line(size = 1) +
  ggtitle("Effect of Bread Price on BicMac Price, by Region") +
  scale_color_discrete(name = "Region", breaks = c("NAw","DEm","DAs","DSa"), labels = c("North America\nWestern Europe","Eastern Europe\nMiddle East","Asia","South America\nAfrica"))