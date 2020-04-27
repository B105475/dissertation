#Statistical test comparing figure 3a and 3b




#cattle-cattle
t.test(dutchccf$`%`,luddenccf$`%`,paired=TRUE,conf.level=0.95)

#cattle-human
t.test(dutchchf$`%`,luddenchf$`%`,paired=TRUE,conf.level=0.95)

#human-human
t.test(dutchhhf$`%`,luddenhhf$`%`,paired=TRUE,conf.level=0.95)

#human-poultry
t.test(dutchhpf$`%`,luddenhpf$`%`,paired=TRUE,conf.level=0.95)

#poultry-poultry
t.test(dutchppf$`%`,luddenppf$`%`,paired=TRUE,conf.level=0.95)

#check assumptions of model
Differencecc = dutchccf$`%` - luddenccf$`%`

hist(Differencecc,   
     col="gray", 
     main="Histogram of differences",
     xlab="Differencecc")

Differencech = dutchchf$`%` - luddenchf$`%`
hist(Differencech,   
     col="gray", 
     main="Histogram of differences",
     xlab="Difference")
########################################
#maybe not t test cos that assumes normal distribution


#we need a non parametric test between 2 unpaired groups= Mann-Whitney U test

wilcox.test(dutchccf$`%`,luddenccf$`%`)
#data:  dutchccf$`%` and luddenccf$`%`
#W = 2455, p-value = 0.002644
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(dutchchf$`%`,luddenchf$`%`)
#data:  dutchchf$`%` and luddenchf$`%`
#W = 2425.5, p-value = 0.0009124
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(dutchhhf$`%`,luddenhhf$`%`)
#data:  dutchhhf$`%` and luddenhhf$`%`
#W = 2457, p-value = 0.02136
#alternative hypothesis: true location shift is not equal to 0
wilcox.test(dutchhpf$`%`,luddenhpf$`%`)
#data:  dutchhpf$`%` and luddenhpf$`%`
#W = 2304, p-value = 0.003706
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(dutchppf$`%`,luddenppf$`%`)
#data:  dutchppf$`%` and luddenppf$`%`
#W = 2421, p-value = 0.007985
#alternative hypothesis: true location shift is not equal to 0












