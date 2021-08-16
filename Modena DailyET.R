### Daily ET (mm) estimation from daily SM averages ############################
# ET calculated as difference in daily soil moisture adjusting for increasing root depth
# Reference ET values from the weather station

library(readxl)
library(writexl)

setwd("~/Modena data") # sets the working directory in the folder containing the data file
data=read_excel("Allen/Soil Moisture/2020/Corn 252.xlsx", sheet="SM with RD") # load the SM data from excel

# Corn
### sensor 1/101 #######################################################
#data=data[c(1:3, 5:12)]
data=na.omit(data)
n=length(data$Dates) # Number of days with data
ETo=data$ETo     # Reference ET (mm)
rd=data$Zr          # Root depth (mm)
sm1=data$`1 Acclima`
sm2=data$`2 Acclima`
sm3=data$`3 Acclima`
sm4=data$`4 Acclima`
sm5=data$`5 Acclima`
sm6=data$`6 Acclima`
sm7=data$`7 Acclima`
sm8=data$`8 Acclima`

# sensor depth (mm)

##Depths for Corn 252, 245, 252 
sd1=3*25.4 # sensor 1: from surface to 3 in
sd2=3*25.4 # sensor 2: from surface to 3 in
sd3=6*25.4 # sensor 3: from 3 in to 6 in
sd4=6*25.4 # sensor 4: from 3 in to 6 in
sd5=12*25.4 # sensor 5: from 6 in to 1ft
sd6=24*25.4 # sensor 6: from 1ft to 2ft
sd7=36*25.4 # sensor 7: from 2ft to 3ft
sd8=48*25.4 # sensor 8: from 3ft to 4ft

##Depths for Alfalfa 226, 263
#sd1=3*25.4 # sensor 1: from surface to 3 in
#sd2=6*25.4 # sensor 2: from 6 in to 12 in
#sd3=12*25.4 # sensor 3: from 12 in to 1ft
#sd4=24*25.4 # sensor 4: from 1ft to 2ft
#sd5=36*25.4 # sensor 5: from 2ft to 3ft
#sd6=48*25.4 # sensor 6: from 3ft to 4ft
#sd7=60*25.4 # sensor 7: from 4ft to 5ft

##Depths for Alfalfa 216
#sd1=3*25.4 # sensor 1: from surface to 3 in
#sd2=6*25.4 # sensor 2: from 6 in to 12 in
#sd3=12*25.4 # sensor 3: from 12 in to 1ft
#sd4=24*25.4 # sensor 4: from 1ft to 2ft
#sd5=36*25.4 # sensor 5: from 2ft to 3ft
#sd6=48*25.4 # sensor 6: from 3ft to 4ft
#sd7=60*25.4 # sensor 7: from 4ft to 5ft

# plot root depth with sensor depth
plot(data$Dates, -rd, 
     type='h', col='goldenrod', lwd='3', ylim=c(-1800, 0),
     main='Corn 252 root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
abline(h=-sd5, col='coral2', lwd=2)
abline(h=-sd6, col='steelblue1', lwd=2)
abline(h=-sd7, col='gold2', lwd=2)
abline(h=-sd8, col='olivedrab3', lwd=2)
legend('bottom', lty=1, lwd=3, legend=c('s1', 's2','s3','s4','3 in','3 in','6 in','6 in','s5','s6','s7','s8','1 ft','2 ft','3 ft','4 ft'), col = c(NA,NA,NA,NA,'coral4','steelblue4','gold4','olivedrab4',NA,NA,NA,NA,'coral2','steelblue1','gold2','olivedrab3'), ncol=4)

ET1.101=c() # ET contribution from sensor 1
ET2.101=c() # ET contribution from sensor 2
ET3.101=c() # ET contribution from sensor 3
ET4.101=c() # ET contribution from sensor 4
ETd.101=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { # ET from sensor 1
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.101[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.101[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.101[i]<0) {ET1.101[i]=0}}         
plot(ET1.101, type='h', ylab='Sensor 1 ET', main='Corn 252 ET')

# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.101[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.101[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.101[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.101[i]<0) {ET2.101[i]=0}}         
plot(ET2.101, type='h', ylab='Sensor 2 ET', main='Corn 252 ET')

# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.101[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.101[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.101[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.101[i]<0) {ET3.101[i]=0} }         
plot(ET3.101, type='h', ylab='Sensor 3 ET', main='Corn 252 ET')

# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.101[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.101[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.101[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.101[i]<0) {ET4.101[i]=0} }         
plot(ET4.101, type='h', ylab='Sensor 4 ET', main='Corn 252 ET')

# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.101[i]=0  }
  else { # (sd4 < RD[i])
    ETd.101[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.101[i] < 0) {
    ETd.101[i]=0  }}
plot(ETd.101, type='h', ylab='ET deeper than sensor 4', main='Corn 252 ET')

# Total ET for Corn 252
ET.1=ET1.101+ET2.101+ET3.101+ET4.101+ETd.101
ET.1[1]=NA # can't calculate ET the first day
plot(data$Dates, ET.1,
     ylim=c(0, 30),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for Corn 252', xlab='', ylab='ET (mm)')
lines(data$Dates, ETo, type='l', xlab='2020', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for Corn 252
Kc.1=ET.1/ETo
plot(data$Dates, Kc.1,
     ylim=c(0, 5),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for Corn 252', xlab='', ylab='Kc')

ET5.101=c() # ET contribution from sensor 5
ET6.101=c() # ET contribution from sensor 6
ET7.101=c() # ET contribution from sensor 7
ET8.101=c() # ET contribution from sensor 8
ETd.101=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) { # ET from sensor 5
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.101[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.101[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.101[i]<0) {ET5.101[i]=0}}         
plot(ET5.101, type='h', ylab='Sensor 5 ET', main='Corn 252 ET')

# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.101[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.101[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.101[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.101[i]<0) {ET6.101[i]=0}}         
plot(ET6.101, type='h', ylab='Sensor 6 ET', main='Corn 252 ET')

# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.101[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.101[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.101[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.101[i]<0) {ET7.101[i]=0} }         
plot(ET7.101, type='h', ylab='Sensor 7 ET', main='Corn 252 ET')

# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.101[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.101[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.101[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.101[i]<0) {ET8.101[i]=0} }         
plot(ET8.101, type='h', ylab='Sensor 8 ET', main='Corn 252 ET')

# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.101[i]=0  }
  else { # (sd8 < RD[i])
    ETd.101[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.101[i] < 0) {
    ETd.101[i]=0  }}
plot(ETd.101, type='h', ylab='ET deeper than sensor 8', main ='Corn 252 ET')

# Total ET for Corn 25201
ET.101=ET5.101+ET6.101+ET7.101+ET8.101+ETd.101
ET.101[1]=NA # can't calculate ET the first day
plot(data$Dates, ET.101,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for Corn 252', xlab='', ylab='ET (mm)')
lines(data$Dates, ETo, type='l', xlab='2020', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for Corn 252
Kc.101=ET.101/ETo
plot(data$Dates, Kc.101,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for Corn 252', xlab='', ylab='Kc')

### sensor 2/102 #######################################################
SM.102=data[c(1:3, 14:21)]
SM.102=na.omit(SM.102)
n=length(SM.102$Dates) # Number of days with data
ETo=SM.102$ETo_mm     # Reference ET (mm)
rd=SM.102$Zr_Corn          # Root depth (mm)
sm1=SM.102$sm1.102
sm2=SM.102$sm2.102
sm3=SM.102$sm3.102
sm4=SM.102$sm4.102
sm5=SM.102$sm5.102
sm6=SM.102$sm6.102
sm7=SM.102$sm7.102
sm8=SM.102$sm8.102

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=4.5*305 # sensor 4: from 3.5ft to 4.5ft

ET1.102=c() # ET contribution from sensor 1
ET2.102=c() # ET contribution from sensor 2
ET3.102=c() # ET contribution from sensor 3
ET4.102=c() # ET contribution from sensor 4
ETd.102=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { # ET from sensor 1
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.102[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.102[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.102[i]<0) {ET1.102[i]=0}}         
plot(ET1.102, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.102[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.102[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.102[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.102[i]<0) {ET2.102[i]=0}}         
plot(ET2.102, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.102[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.102[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.102[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.102[i]<0) {ET3.102[i]=0} }         
plot(ET3.102, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.102[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.102[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.102[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.102[i]<0) {ET4.102[i]=0} }         
plot(ET4.102, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.102[i]=0  }
  else { # (sd4 < RD[i])
    ETd.102[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.102[i] < 0) {
    ETd.102[i]=0  }}
plot(ETd.102, type='h')

# Total ET for sensor 2
ET.2=ET1.102+ET2.102+ET3.102+ET4.102+ETd.102
ET.2[1]=NA # can't calculate ET the first day
plot(SM.102$Dates, ET.2,
     ylim=c(0, 25),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 2', xlab='', ylab='ET (mm)')
lines(SM.102$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 2
Kc.2=ET.2/ETo
plot(SM.102$Dates, Kc.2,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 2', xlab='', ylab='Kc')

ET5.102=c() # ET contribution from sensor 5
ET6.102=c() # ET contribution from sensor 6
ET7.102=c() # ET contribution from sensor 7
ET8.102=c() # ET contribution from sensor 8
ETd.102=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.102[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.102[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.102[i]<0) {ET5.102[i]=0}}         
plot(ET5.102, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.102[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.102[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.102[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.102[i]<0) {ET6.102[i]=0}}         
plot(ET6.102, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.102[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.102[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.102[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.102[i]<0) {ET7.102[i]=0} }         
plot(ET7.102, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.102[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.102[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.102[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.102[i]<0) {ET8.102[i]=0} }         
plot(ET8.102, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.102[i]=0  }
  else { # (sd8 < RD[i])
    ETd.102[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.102[i] < 0) {
    ETd.102[i]=0  }}
plot(ETd.102, type='h')

# Total ET for Corn 25202
ET.102=ET5.102+ET6.102+ET7.102+ET8.102+ETd.102
ET.102[1]=NA # can't calculate ET the first day
plot(SM.102$Dates, ET.102,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for Corn 25202', xlab='', ylab='ET (mm)')
lines(SM.102$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for Corn 25202
Kc.102=ET.102/ETo
plot(SM.102$Dates, Kc.102,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for Corn 25202', xlab='', ylab='Kc')

### sensor 3/103 #######################################################
SM.103=data[c(1:3, 23:30)]
SM.103=na.omit(SM.103)
n=length(SM.103$Dates) # Number of days with data
ETo=SM.103$ETo_mm     # Reference ET (mm)
rd=SM.103$Zr_Corn          # Root depth (mm)
sm1=SM.103$sm1.103
sm2=SM.103$sm2.103
sm3=SM.103$sm3.103
sm4=SM.103$sm4.103
sm5=SM.103$sm5.103
sm6=SM.103$sm6.103
sm7=SM.103$sm7.103
sm8=SM.103$sm8.103

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=4.5*305 # sensor 4: from 3.5ft to 4.5ft

ET1.103=c() # ET contribution from sensor 1
ET2.103=c() # ET contribution from sensor 2
ET3.103=c() # ET contribution from sensor 3
ET4.103=c() # ET contribution from sensor 4
ETd.103=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) {
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.103[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.103[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.103[i]<0) {ET1.103[i]=0}}         
plot(ET1.103, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.103[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.103[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.103[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.103[i]<0) {ET2.103[i]=0}}         
plot(ET2.103, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.103[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.103[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.103[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.103[i]<0) {ET3.103[i]=0} }         
plot(ET3.103, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.103[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.103[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.103[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.103[i]<0) {ET4.103[i]=0} }         
plot(ET4.103, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.103[i]=0  }
  else { # (sd4 < RD[i])
    ETd.103[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.103[i] < 0) {
    ETd.103[i]=0  }}
plot(ETd.103, type='h')

# Total ET for sensor 3
ET.3=ET1.103+ET2.103+ET3.103+ET4.103+ETd.103
ET.3[1]=NA # can't calculate ET the first day
plot(SM.103$Dates, ET.3,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 3', xlab='', ylab='ET (mm)')
lines(SM.103$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 3
Kc.3=ET.3/ETo
plot(SM.103$Dates, Kc.3,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 3', xlab='', ylab='Kc')

ET5.103=c() # ET contribution from sensor 5
ET6.103=c() # ET contribution from sensor 6
ET7.103=c() # ET contribution from sensor 7
ET8.103=c() # ET contribution from sensor 8
ETd.103=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.103[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.103[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.103[i]<0) {ET5.103[i]=0}}         
plot(ET5.103, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.103[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.103[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.103[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.103[i]<0) {ET6.103[i]=0}}         
plot(ET6.103, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.103[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.103[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.103[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.103[i]<0) {ET7.103[i]=0} }         
plot(ET7.103, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.103[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.103[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.103[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.103[i]<0) {ET8.103[i]=0} }         
plot(ET8.103, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.103[i]=0  }
  else { # (sd8 < RD[i])
    ETd.103[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.103[i] < 0) {
    ETd.103[i]=0  }}
plot(ETd.103, type='h')

# Total ET for Corn 25203
ET.103=ET5.103+ET6.103+ET7.103+ET8.103+ETd.103
ET.103[1]=NA # can't calculate ET the first day
plot(SM.103$Dates, ET.103,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for Corn 25203', xlab='', ylab='ET (mm)')
lines(SM.103$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for Corn 25203
Kc.103=ET.103/ETo
plot(SM.103$Dates, Kc.103,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for Corn 25203', xlab='', ylab='Kc')

### sensor 4/104 #######################################################
SM.104=data[c(1:3, 32:39)]
SM.104=na.omit(SM.104)
n=length(SM.104$Dates) # Number of days with data
ETo=SM.104$ETo_mm     # Reference ET (mm)
rd=SM.104$Zr_Corn          # Root depth (mm)
sm1=SM.104$sm1.104
sm2=SM.104$sm2.104
sm3=SM.104$sm3.104
sm4=SM.104$sm4.104
sm5=SM.104$sm5.104
sm6=SM.104$sm6.104
sm7=SM.104$sm7.104
sm8=SM.104$sm8.104

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 6.5ft

# plot root depth with sensor depth
plot(SM.104$Dates, -rd, 
     type='h', col='goldenrod', lwd='3', ylim=c(-2000, 0),
     main='Corn root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.104=c() # ET contribution from sensor 1
ET2.104=c() # ET contribution from sensor 2
ET3.104=c() # ET contribution from sensor 3
ET4.104=c() # ET contribution from sensor 4
ETd.104=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) {
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.104[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.104[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.104[i]<0) {ET1.104[i]=0}}         
plot(ET1.104, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.104[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.104[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.104[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.104[i]<0) {ET2.104[i]=0}}         
plot(ET2.104, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.104[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.104[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.104[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.104[i]<0) {ET3.104[i]=0} }         
plot(ET3.104, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.104[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.104[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.104[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.104[i]<0) {ET4.104[i]=0} }         
plot(ET4.104, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.104[i]=0  }
  else { # (sd4 < RD[i])
    ETd.104[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.104[i] < 0) {
    ETd.104[i]=0  }}
plot(ETd.104, type='h')

# Total ET for sensor 4
ET.4=ET1.104+ET2.104+ET3.104+ET4.104+ETd.104
ET.4[1]=NA # can't calculate ET the first day
plot(SM.104$Dates, ET.4,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 4', xlab='', ylab='ET (mm)')
lines(SM.104$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 4
Kc.4=ET.4/ETo
plot(SM.104$Dates, Kc.4,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 4', xlab='', ylab='Kc')

ET5.104=c() # ET contribution from sensor 5
ET6.104=c() # ET contribution from sensor 6
ET7.104=c() # ET contribution from sensor 7
ET8.104=c() # ET contribution from sensor 8
ETd.104=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) { # ET from sensor 5
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.104[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.104[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.104[i]<0) {ET5.104[i]=0}}         
plot(ET5.104, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.104[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.104[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.104[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.104[i]<0) {ET6.104[i]=0}}         
plot(ET6.104, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.104[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.104[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.104[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.104[i]<0) {ET7.104[i]=0} }         
plot(ET7.104, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.104[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.104[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.104[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.104[i]<0) {ET8.104[i]=0} }         
plot(ET8.104, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.104[i]=0  }
  else { # (sd8 < RD[i])
    ETd.104[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.104[i] < 0) {
    ETd.104[i]=0  }}
plot(ETd.104, type='h')

# Total ET for Corn 25204
ET.104=ET5.104+ET6.104+ET7.104+ET8.104+ETd.104
ET.104[1]=NA # can't calculate ET the first day
plot(SM.104$Dates, ET.104,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for Corn 25204', xlab='', ylab='ET (mm)')
lines(SM.104$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for Corn 25204
Kc.104=ET.104/ETo
plot(SM.104$Dates, Kc.104,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for Corn 25204', xlab='', ylab='Kc')

### sensor 6/106 #######################################################
SM.106=data[c(1:3, 41:48)]
SM.106=na.omit(SM.106)
n=length(SM.106$Dates) # Number of days with data
ETo=SM.106$ETo_mm     # Reference ET (mm)
rd=SM.106$Zr_Corn          # Root depth (mm)
sm1=SM.106$sm1.106
sm2=SM.106$sm2.106
sm3=SM.106$sm3.106
sm4=SM.106$sm4.106
sm5=SM.106$sm5.106
sm6=SM.106$sm6.106
sm7=SM.106$sm7.106
sm8=SM.106$sm8.106

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.106$Dates, -rd, 
     type='h', col='goldenrod', lwd='3', ylim=c(-2000, 0),
     main='Corn root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.106=c() # ET contribution from sensor 1
ET2.106=c() # ET contribution from sensor 2
ET3.106=c() # ET contribution from sensor 3
ET4.106=c() # ET contribution from sensor 4
ETd.106=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) {
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.106[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.106[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.106[i]<0) {ET1.106[i]=0}}         
plot(ET1.106, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.106[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.106[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.106[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.106[i]<0) {ET2.106[i]=0}}         
plot(ET2.106, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.106[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.106[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.106[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.106[i]<0) {ET3.106[i]=0} }         
plot(ET3.106, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.106[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.106[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.106[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.106[i]<0) {ET4.106[i]=0} }         
plot(ET4.106, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.106[i]=0  }
  else { # (sd4 < RD[i])
    ETd.106[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.106[i] < 0) {
    ETd.106[i]=0  }}
plot(ETd.106, type='h')

# Total ET for sensor 6
ET.6=ET1.106+ET2.106+ET3.106+ET4.106+ETd.106
ET.6[1]=NA # can't calculate ET the first day
plot(SM.106$Dates, ET.6,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 6', xlab='', ylab='ET (mm)')
lines(SM.106$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 6
Kc.6=ET.6/ETo
plot(SM.106$Dates, Kc.6,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 6', xlab='', ylab='Kc')

ET5.106=c() # ET contribution from sensor 5
ET6.106=c() # ET contribution from sensor 6
ET7.106=c() # ET contribution from sensor 7
ET8.106=c() # ET contribution from sensor 8
ETd.106=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.106[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.106[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.106[i]<0) {ET5.106[i]=0}}         
plot(ET5.106, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.106[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.106[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.106[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.106[i]<0) {ET6.106[i]=0}}         
plot(ET6.106, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.106[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.106[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.106[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.106[i]<0) {ET7.106[i]=0} }         
plot(ET7.106, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.106[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.106[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.106[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.106[i]<0) {ET8.106[i]=0} }         
plot(ET8.106, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.106[i]=0  }
  else { # (sd8 < RD[i])
    ETd.106[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.106[i] < 0) {
    ETd.106[i]=0  }}
plot(ETd.106, type='h')

# Total ET for Corn 25206
ET.106=ET5.106+ET6.106+ET7.106+ET8.106+ETd.106
ET.106[1]=NA # can't calculate ET the first day
plot(SM.106$Dates, ET.106,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for Corn 25206', xlab='', ylab='ET (mm)')
lines(SM.106$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for Corn 25206
Kc.106=ET.106/ETo
plot(SM.106$Dates, Kc.106,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for Corn 25206', xlab='', ylab='Kc')

### sensor 9/109 #######################################################
SM.109=data[c(1:3, 50:57)]
SM.109=na.omit(SM.109)
n=length(SM.109$Dates) # Number of days with data
ETo=SM.109$ETo_mm     # Reference ET (mm)
rd=SM.109$Zr_Corn          # Root depth (mm)
sm1=SM.109$sm1.109
sm2=SM.109$sm2.109
sm3=SM.109$sm3.109
sm4=SM.109$sm4.109
sm5=SM.109$sm5.109
sm6=SM.109$sm6.109
sm7=SM.109$sm7.109
sm8=SM.109$sm8.109

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.109$Dates, -rd, 
     type='h', col='goldenrod', lwd='3', ylim=c(-2000, 0),
     main='Corn root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.109=c() # ET contribution from sensor 1
ET2.109=c() # ET contribution from sensor 2
ET3.109=c() # ET contribution from sensor 3
ET4.109=c() # ET contribution from sensor 4
ETd.109=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { # ET from sensor 1
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.109[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.109[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.109[i]<0) {ET1.109[i]=0}}         
plot(ET1.109, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.109[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.109[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.109[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.109[i]<0) {ET2.109[i]=0}}         
plot(ET2.109, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.109[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.109[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.109[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.109[i]<0) {ET3.109[i]=0} }         
plot(ET3.109, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.109[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.109[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.109[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.109[i]<0) {ET4.109[i]=0} }         
plot(ET4.109, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.109[i]=0  }
  else { # (sd4 < RD[i])
    ETd.109[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.109[i] < 0) {
    ETd.109[i]=0  }}
plot(ETd.109, type='h')

# Total ET for sensor 9
ET.9=ET1.109+ET2.109+ET3.109+ET4.109+ETd.109
ET.9[1]=NA # can't calculate ET the first day
plot(SM.109$Dates, ET.9,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 9', xlab='', ylab='ET (mm)')
lines(SM.109$Dates, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 9
Kc.9=ET.9/ETo
plot(SM.109$Dates, Kc.9,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 9', xlab='', ylab='Kc')

ET5.109=c() # ET contribution from sensor 5
ET6.109=c() # ET contribution from sensor 6
ET7.109=c() # ET contribution from sensor 7
ET8.109=c() # ET contribution from sensor 8
ETd.109=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.109[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.109[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.109[i]<0) {ET5.109[i]=0}}         
plot(ET5.109, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.109[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.109[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.109[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.109[i]<0) {ET6.109[i]=0}}         
plot(ET6.109, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.109[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.109[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.109[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.109[i]<0) {ET7.109[i]=0} }         
plot(ET7.109, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.109[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.109[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.109[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.109[i]<0) {ET8.109[i]=0} }         
plot(ET8.109, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.109[i]=0  }
  else { # (sd8 < RD[i])
    ETd.109[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.109[i] < 0) {
    ETd.109[i]=0  }}
plot(ETd.109, type='h')

# Total ET for Corn 25209
ET.109=ET5.109+ET6.109+ET7.109+ET8.109+ETd.109
ET.109[1]=NA # can't calculate ET the first day
plot(SM.109$Dates, ET.109,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for Corn 25201', xlab='', ylab='ET (mm)')
lines(SM.109$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for Corn 25209
Kc.109=ET.109/ETo
plot(SM.109$Dates, Kc.109,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for Corn 25209', xlab='', ylab='Kc')

### sensor 13/202 #######################################################
SM.202=data[c(1:3, 59:66)]
SM.202=na.omit(SM.202)
n=length(SM.202$Dates) # Number of days with data
ETo=SM.202$ETo_mm     # Reference ET (mm)
rd=SM.202$Zr_Corn          # Root depth (mm)
sm1=SM.202$sm1.202
sm2=SM.202$sm2.202
sm3=SM.202$sm3.202
sm4=SM.202$sm4.202
sm5=SM.202$sm5.202
sm6=SM.202$sm6.202
sm7=SM.202$sm7.202
sm8=SM.202$sm8.202

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.202$Dates, -rd, 
     type='h', col='goldenrod', lwd='3', ylim=c(-2000, 0),
     main='Corn root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.202=c() # ET contribution from sensor 1
ET2.202=c() # ET contribution from sensor 2
ET3.202=c() # ET contribution from sensor 3
ET4.202=c() # ET contribution from sensor 4
ETd.202=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.202[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.202[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.202[i]<0) {ET1.202[i]=0}}         
plot(ET1.202, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.202[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.202[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.202[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.202[i]<0) {ET2.202[i]=0}}         
plot(ET2.202, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.202[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.202[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.202[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.202[i]<0) {ET3.202[i]=0} }         
plot(ET3.202, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.202[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.202[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.202[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.202[i]<0) {ET4.202[i]=0} }         
plot(ET4.202, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.202[i]=0  }
  else { # (sd4 < RD[i])
    ETd.202[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.202[i] < 0) {
    ETd.202[i]=0  }}
plot(ETd.202, type='h')

# Total ET for Corn 2523
ET.13=ET1.202+ET2.202+ET3.202+ET4.202+ETd.202
ET.13[1]=NA # can't calculate ET the first day
plot(SM.202$Dates, ET.13,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for Corn 2523', xlab='', ylab='ET (mm)')
lines(SM.202$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for Corn 2523
Kc.13=ET.13/ETo
plot(SM.202$Dates, Kc.13,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for Corn 2523', xlab='', ylab='Kc')

ET5.202=c() # ET contribution from sensor 5
ET6.202=c() # ET contribution from sensor 6
ET7.202=c() # ET contribution from sensor 7
ET8.202=c() # ET contribution from sensor 8
ETd.202=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.202[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.202[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.202[i]<0) {ET5.202[i]=0}}         
plot(ET5.202, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.202[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.202[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.202[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.202[i]<0) {ET6.202[i]=0}}         
plot(ET6.202, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.202[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.202[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.202[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.202[i]<0) {ET7.202[i]=0} }         
plot(ET7.202, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.202[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.202[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.202[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.202[i]<0) {ET8.202[i]=0} }         
plot(ET8.202, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.202[i]=0  }
  else { # (sd8 < RD[i])
    ETd.202[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.202[i] < 0) {
    ETd.202[i]=0  }}
plot(ETd.202, type='h')

# Total ET for sensor 202
ET.202=ET5.202+ET6.202+ET7.202+ET8.202+ETd.202
ET.202[1]=NA # can't calculate ET the first day
plot(SM.202$Dates, ET.202,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 202', xlab='', ylab='ET (mm)')
lines(SM.202$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 202
Kc.202=ET.202/ETo
plot(SM.202$Dates, Kc.202,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 202', xlab='', ylab='Kc')

### sensor 16/205 #######################################################
SM.205=data[c(1:3, 68:75)]
SM.205=na.omit(SM.205)
n=length(SM.205$Dates) # Number of days with data
ETo=SM.205$ETo_mm     # Reference ET (mm)
rd=SM.205$Zr_Corn          # Root depth (mm)
sm1=SM.205$sm1.205
sm2=SM.205$sm2.205
sm3=SM.205$sm3.205
sm4=SM.205$sm4.205
sm5=SM.205$sm5.205
sm6=SM.205$sm6.205
sm7=SM.205$sm7.205
sm8=SM.205$sm8.205

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.205$Dates, -rd, 
     type='h', col='goldenrod', lwd='3', ylim=c(-2000, 0),
     main='Corn root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.205=c() # ET contribution from sensor 1
ET2.205=c() # ET contribution from sensor 2
ET3.205=c() # ET contribution from sensor 3
ET4.205=c() # ET contribution from sensor 4
ETd.205=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.205[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.205[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.205[i]<0) {ET1.205[i]=0}}         
plot(ET1.205, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.205[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.205[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.205[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.205[i]<0) {ET2.205[i]=0}}         
plot(ET2.205, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.205[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.205[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.205[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.205[i]<0) {ET3.205[i]=0} }         
plot(ET3.205, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.205[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.205[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.205[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.205[i]<0) {ET4.205[i]=0} }         
plot(ET4.205, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.205[i]=0  }
  else { # (sd4 < RD[i])
    ETd.205[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.205[i] < 0) {
    ETd.205[i]=0  }}
plot(ETd.205, type='h')

# Total ET for Corn 2526
ET.16=ET1.205+ET2.205+ET3.205+ET4.205+ETd.205
ET.16[1]=NA # can't calculate ET the first day
plot(SM.205$Dates, ET.16,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for Corn 2526', xlab='', ylab='ET (mm)')
lines(SM.205$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for Corn 2526
Kc.16=ET.16/ETo
plot(SM.205$Dates, Kc.16,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for Corn 2526', xlab='', ylab='Kc')

ET5.205=c() # ET contribution from sensor 5
ET6.205=c() # ET contribution from sensor 6
ET7.205=c() # ET contribution from sensor 7
ET8.205=c() # ET contribution from sensor 8
ETd.205=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.205[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.205[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.205[i]<0) {ET5.205[i]=0}}         
plot(ET5.205, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.205[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.205[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.205[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.205[i]<0) {ET6.205[i]=0}}         
plot(ET6.205, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.205[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.205[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.205[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.205[i]<0) {ET7.205[i]=0} }         
plot(ET7.205, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.205[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.205[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.205[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.205[i]<0) {ET8.205[i]=0} }         
plot(ET8.205, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.205[i]=0  }
  else { # (sd8 < RD[i])
    ETd.205[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.205[i] < 0) {
    ETd.205[i]=0  }}
plot(ETd.205, type='h')

# Total ET for sensor 205
ET.205=ET5.205+ET6.205+ET7.205+ET8.205+ETd.205
ET.205[1]=NA # can't calculate ET the first day
plot(SM.205$Dates, ET.205,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 205', xlab='', ylab='ET (mm)')
lines(SM.205$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 205
Kc.205=ET.205/ETo
plot(SM.205$Dates, Kc.205,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 205', xlab='', ylab='Kc')

### sensor 17/206 #######################################################
SM.206=data[c(1:3, 77:84)]
SM.206=na.omit(SM.206)
n=length(SM.206$Dates) # Number of days with data
ETo=SM.206$ETo_mm     # Reference ET (mm)
rd=SM.206$Zr_Corn          # Root depth (mm)
sm1=SM.206$sm1.206
sm2=SM.206$sm2.206
sm3=SM.206$sm3.206
sm4=SM.206$sm4.206
sm5=SM.206$sm5.206
sm6=SM.206$sm6.206
sm7=SM.206$sm7.206
sm8=SM.206$sm8.206

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.206$Dates, -rd, 
     type='h', col='goldenrod', lwd='3', ylim=c(-2000, 0),
     main='Corn root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.206=c() # ET contribution from sensor 1
ET2.206=c() # ET contribution from sensor 2
ET3.206=c() # ET contribution from sensor 3
ET4.206=c() # ET contribution from sensor 4
ETd.206=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.206[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.206[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.206[i]<0) {ET1.206[i]=0}}         
plot(ET1.206, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.206[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.206[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.206[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.206[i]<0) {ET2.206[i]=0}}         
plot(ET2.206, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.206[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.206[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.206[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.206[i]<0) {ET3.206[i]=0} }         
plot(ET3.206, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.206[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.206[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.206[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.206[i]<0) {ET4.206[i]=0} }         
plot(ET4.206, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.206[i]=0  }
  else { # (sd4 < RD[i])
    ETd.206[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.206[i] < 0) {
    ETd.206[i]=0  }}
plot(ETd.206, type='h')

# Total ET for Corn 2527
ET.17=ET1.206+ET2.206+ET3.206+ET4.206+ETd.206
ET.17[1]=NA # can't calculate ET the first day
plot(SM.206$Dates, ET.17,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for Corn 2527', xlab='', ylab='ET (mm)')
lines(SM.206$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for Corn 2527
Kc.17=ET.17/ETo
plot(SM.206$Dates, Kc.17,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for Corn 2527', xlab='', ylab='Kc')

ET5.206=c() # ET contribution from sensor 5
ET6.206=c() # ET contribution from sensor 6
ET7.206=c() # ET contribution from sensor 7
ET8.206=c() # ET contribution from sensor 8
ETd.206=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.206[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.206[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.206[i]<0) {ET5.206[i]=0}}         
plot(ET5.206, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.206[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.206[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.206[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.206[i]<0) {ET6.206[i]=0}}         
plot(ET6.206, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.206[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.206[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.206[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.206[i]<0) {ET7.206[i]=0} }         
plot(ET7.206, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.206[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.206[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.206[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.206[i]<0) {ET8.206[i]=0} }         
plot(ET8.206, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.206[i]=0  }
  else { # (sd8 < RD[i])
    ETd.206[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.206[i] < 0) {
    ETd.206[i]=0  }}
plot(ETd.206, type='h')

# Total ET for sensor 206
ET.206=ET5.206+ET6.206+ET7.206+ET8.206+ETd.206
ET.206[1]=NA # can't calculate ET the first day
plot(SM.206$Dates, ET.206,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 206', xlab='', ylab='ET (mm)')
lines(SM.206$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 206
Kc.206=ET.206/ETo
plot(SM.206$Dates, Kc.206,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 206', xlab='', ylab='Kc')


# Oats ####
### sensor 27/305 #######################################################
SM.305=data[c(1,2,86,88:95)]
SM.305=na.omit(SM.305)
n=length(SM.305$Dates) # Number of days with data
ETo=SM.305$ETo_mm     # Reference ET (mm)
rd=SM.305$Zr_Oats     # Root depth (mm)
sm1=SM.305$sm1.305
sm2=SM.305$sm2.305
sm3=SM.305$sm3.305
sm4=SM.305$sm4.305
sm5=SM.305$sm5.305
sm6=SM.305$sm6.305
sm7=SM.305$sm7.305
sm8=SM.305$sm8.305

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.305$Dates, -rd, 
     type='h', col='darkolivegreen', lwd='3', ylim=c(-2000, 0),
     main='Oats root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.305=c() # ET contribution from sensor 1
ET2.305=c() # ET contribution from sensor 2
ET3.305=c() # ET contribution from sensor 3
ET4.305=c() # ET contribution from sensor 4
ETd.305=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.305[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.305[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.305[i]<0) {ET1.305[i]=0}}         
plot(ET1.305, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.305[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.305[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.305[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.305[i]<0) {ET2.305[i]=0}}         
plot(ET2.305, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.305[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.305[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.305[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.305[i]<0) {ET3.305[i]=0} }         
plot(ET3.305, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.305[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.305[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.305[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.305[i]<0) {ET4.305[i]=0} }         
plot(ET4.305, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.305[i]=0  }
  else { # (sd4 < RD[i])
    ETd.305[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.305[i] < 0) {
    ETd.305[i]=0  }}
plot(ETd.305, type='h')

# Total ET for sensor 27
ET.27=ET1.305+ET2.305+ET3.305+ET4.305+ETd.305
ET.27[1]=NA # can't calculate ET the first day
plot(SM.305$Dates, ET.27,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 27', xlab='', ylab='ET (mm)')
lines(SM.305$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 27
Kc.27=ET.27/ETo
plot(SM.305$Dates, Kc.27,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 27', xlab='', ylab='Kc')

ET5.305=c() # ET contribution from sensor 5
ET6.305=c() # ET contribution from sensor 6
ET7.305=c() # ET contribution from sensor 7
ET8.305=c() # ET contribution from sensor 8
ETd.305=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.305[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.305[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.305[i]<0) {ET5.305[i]=0}}         
plot(ET5.305, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.305[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.305[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.305[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.305[i]<0) {ET6.305[i]=0}}         
plot(ET6.305, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.305[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.305[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.305[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.305[i]<0) {ET7.305[i]=0} }         
plot(ET7.305, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.305[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.305[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.305[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.305[i]<0) {ET8.305[i]=0} }         
plot(ET8.305, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.305[i]=0  }
  else { # (sd8 < RD[i])
    ETd.305[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.305[i] < 0) {
    ETd.305[i]=0  }}
plot(ETd.305, type='h')

# Total ET for sensor 305
ET.305=ET5.305+ET6.305+ET7.305+ET8.305+ETd.305
ET.305[1]=NA # can't calculate ET the first day
plot(SM.305$Dates, ET.305,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 305', xlab='', ylab='ET (mm)')
lines(SM.305$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 305
Kc.305=ET.305/ETo
plot(SM.305$Dates, Kc.305,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 305', xlab='', ylab='Kc')


### sensor 29/307 #######################################################
SM.307=data[c(1,2,86,97:104)]
SM.307=na.omit(SM.307)
n=length(SM.307$Dates) # Number of days with data
ETo=SM.307$ETo_mm     # Reference ET (mm)
rd=SM.307$Zr_Oats     # Root depth (mm)
sm1=SM.307$sm1.307
sm2=SM.307$sm2.307
sm3=SM.307$sm3.307
sm4=SM.307$sm4.307
sm5=SM.307$sm5.307
sm6=SM.307$sm6.307
sm7=SM.307$sm7.307
sm8=SM.307$sm8.307

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.307$Dates, -rd, 
     type='h', col='darkolivegreen', lwd='3', ylim=c(-2000, 0),
     main='Oats root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.307=c() # ET contribution from sensor 1
ET2.307=c() # ET contribution from sensor 2
ET3.307=c() # ET contribution from sensor 3
ET4.307=c() # ET contribution from sensor 4
ETd.307=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.307[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.307[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.307[i]<0) {ET1.307[i]=0}}         
plot(ET1.307, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.307[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.307[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.307[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.307[i]<0) {ET2.307[i]=0}}         
plot(ET2.307, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.307[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.307[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.307[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.307[i]<0) {ET3.307[i]=0} }         
plot(ET3.307, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.307[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.307[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.307[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.307[i]<0) {ET4.307[i]=0} }         
plot(ET4.307, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.307[i]=0  }
  else { # (sd4 < RD[i])
    ETd.307[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.307[i] < 0) {
    ETd.307[i]=0  }}
plot(ETd.307, type='h')

# Total ET for sensor 29
ET.29=ET1.307+ET2.307+ET3.307+ET4.307+ETd.307
ET.29[1]=NA # can't calculate ET the first day
plot(SM.307$Dates, ET.29,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 29', xlab='', ylab='ET (mm)')
lines(SM.307$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 29
Kc.29=ET.29/ETo
plot(SM.307$Dates, Kc.29,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 29', xlab='', ylab='Kc')

ET5.307=c() # ET contribution from sensor 5
ET6.307=c() # ET contribution from sensor 6
ET7.307=c() # ET contribution from sensor 7
ET8.307=c() # ET contribution from sensor 8
ETd.307=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.307[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.307[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.307[i]<0) {ET5.307[i]=0}}         
plot(ET5.307, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.307[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.307[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.307[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.307[i]<0) {ET6.307[i]=0}}         
plot(ET6.307, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.307[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.307[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.307[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.307[i]<0) {ET7.307[i]=0} }         
plot(ET7.307, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.307[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.307[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.307[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.307[i]<0) {ET8.307[i]=0} }         
plot(ET8.307, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.307[i]=0  }
  else { # (sd8 < RD[i])
    ETd.307[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.307[i] < 0) {
    ETd.307[i]=0  }}
plot(ETd.307, type='h')

# Total ET for sensor 307
ET.307=ET5.307+ET6.307+ET7.307+ET8.307+ETd.307
ET.307[1]=NA # can't calculate ET the first day
plot(SM.307$Dates, ET.307,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 307', xlab='', ylab='ET (mm)')
lines(SM.307$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 307
Kc.307=ET.307/ETo
plot(SM.307$Dates, Kc.307,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 307', xlab='', ylab='Kc')


### sensor 30/308 #######################################################
SM.308=data[c(1,2,86,106:113)]
SM.308=na.omit(SM.308)
n=length(SM.308$Dates) # Number of days with data
ETo=SM.308$ETo_mm     # Reference ET (mm)
rd=SM.308$Zr_Oats     # Root depth (mm)
sm1=SM.308$sm1.308
sm2=SM.308$sm2.308
sm3=SM.308$sm3.308
sm4=SM.308$sm4.308
sm5=SM.308$sm5.308
sm6=SM.308$sm6.308
sm7=SM.308$sm7.308
sm8=SM.308$sm8.308

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.308$Dates, -rd, 
     type='h', col='darkolivegreen', lwd='3', ylim=c(-2000, 0),
     main='Oats root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.308=c() # ET contribution from sensor 1
ET2.308=c() # ET contribution from sensor 2
ET3.308=c() # ET contribution from sensor 3
ET4.308=c() # ET contribution from sensor 4
ETd.308=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.308[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.308[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.308[i]<0) {ET1.308[i]=0}}         
plot(ET1.308, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.308[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.308[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.308[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.308[i]<0) {ET2.308[i]=0}}         
plot(ET2.308, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.308[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.308[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.308[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.308[i]<0) {ET3.308[i]=0} }         
plot(ET3.308, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.308[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.308[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.308[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.308[i]<0) {ET4.308[i]=0} }         
plot(ET4.308, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.308[i]=0  }
  else { # (sd4 < RD[i])
    ETd.308[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.308[i] < 0) {
    ETd.308[i]=0  }}
plot(ETd.308, type='h')

# Total ET for sensor 30
ET.30=ET1.308+ET2.308+ET3.308+ET4.308+ETd.308
ET.30[1]=NA # can't calculate ET the first day
plot(SM.308$Dates, ET.30,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 30', xlab='', ylab='ET (mm)')
lines(SM.308$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 30
Kc.30=ET.30/ETo
plot(SM.308$Dates, Kc.30,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 30', xlab='', ylab='Kc')

ET5.308=c() # ET contribution from sensor 5
ET6.308=c() # ET contribution from sensor 6
ET7.308=c() # ET contribution from sensor 7
ET8.308=c() # ET contribution from sensor 8
ETd.308=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.308[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.308[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.308[i]<0) {ET5.308[i]=0}}         
plot(ET5.308, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.308[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.308[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.308[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.308[i]<0) {ET6.308[i]=0}}         
plot(ET6.308, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.308[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.308[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.308[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.308[i]<0) {ET7.308[i]=0} }         
plot(ET7.308, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.308[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.308[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.308[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.308[i]<0) {ET8.308[i]=0} }         
plot(ET8.308, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.308[i]=0  }
  else { # (sd8 < RD[i])
    ETd.308[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.308[i] < 0) {
    ETd.308[i]=0  }}
plot(ETd.308, type='h')

# Total ET for sensor 308
ET.308=ET5.308+ET6.308+ET7.308+ET8.308+ETd.308
ET.308[1]=NA # can't calculate ET the first day
plot(SM.308$Dates, ET.308,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 308', xlab='', ylab='ET (mm)')
lines(SM.308$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 308
Kc.308=ET.308/ETo
plot(SM.308$Dates, Kc.308,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 308', xlab='', ylab='Kc')


### sensor 31/309 #######################################################
SM.309=data[c(1,2,86,115:122)]
SM.309=na.omit(SM.309)
n=length(SM.309$Dates) # Number of days with data
ETo=SM.309$ETo_mm     # Reference ET (mm)
rd=SM.309$Zr_Oats     # Root depth (mm)
sm1=SM.309$sm1.309
sm2=SM.309$sm2.309
sm3=SM.309$sm3.309
sm4=SM.309$sm4.309
sm5=SM.309$sm5.309
sm6=SM.309$sm6.309
sm7=SM.309$sm7.309
sm8=SM.309$sm8.309

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.309$Dates, -rd, 
     type='h', col='darkolivegreen', lwd='3', ylim=c(-2000, 0),
     main='Oats root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.309=c() # ET contribution from sensor 1
ET2.309=c() # ET contribution from sensor 2
ET3.309=c() # ET contribution from sensor 3
ET4.309=c() # ET contribution from sensor 4
ETd.309=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.309[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.309[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.309[i]<0) {ET1.309[i]=0}}         
plot(ET1.309, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.309[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.309[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.309[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.309[i]<0) {ET2.309[i]=0}}         
plot(ET2.309, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.309[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.309[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.309[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.309[i]<0) {ET3.309[i]=0} }         
plot(ET3.309, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.309[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.309[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.309[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.309[i]<0) {ET4.309[i]=0} }         
plot(ET4.309, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.309[i]=0  }
  else { # (sd4 < RD[i])
    ETd.309[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.309[i] < 0) {
    ETd.309[i]=0  }}
plot(ETd.309, type='h')

# Total ET for sensor 31
ET.31=ET1.309+ET2.309+ET3.309+ET4.309+ETd.309
ET.31[1]=NA # can't calculate ET the first day
plot(SM.309$Dates, ET.31,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 31', xlab='', ylab='ET (mm)')
lines(SM.309$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 31
Kc.31=ET.31/ETo
plot(SM.309$Dates, Kc.31,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 31', xlab='', ylab='Kc')

ET5.309=c() # ET contribution from sensor 5
ET6.309=c() # ET contribution from sensor 6
ET7.309=c() # ET contribution from sensor 7
ET8.309=c() # ET contribution from sensor 8
ETd.309=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.309[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.309[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.309[i]<0) {ET5.309[i]=0}}         
plot(ET5.309, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.309[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.309[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.309[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.309[i]<0) {ET6.309[i]=0}}         
plot(ET6.309, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.309[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.309[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.309[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.309[i]<0) {ET7.309[i]=0} }         
plot(ET7.309, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.309[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.309[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.309[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.309[i]<0) {ET8.309[i]=0} }         
plot(ET8.309, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.309[i]=0  }
  else { # (sd8 < RD[i])
    ETd.309[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.309[i] < 0) {
    ETd.309[i]=0  }}
plot(ETd.309, type='h')

# Total ET for sensor 309
ET.309=ET5.309+ET6.309+ET7.309+ET8.309+ETd.309
ET.309[1]=NA # can't calculate ET the first day
plot(SM.309$Dates, ET.309,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 309', xlab='', ylab='ET (mm)')
lines(SM.309$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 309
Kc.309=ET.309/ETo
plot(SM.309$Dates, Kc.309,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 309', xlab='', ylab='Kc')

### sensor 32/310 #######################################################
SM.310=data[c(1,2,86,124:131)]
SM.310=na.omit(SM.310)
n=length(SM.310$Dates) # Number of days with data
ETo=SM.310$ETo_mm     # Reference ET (mm)
rd=SM.310$Zr_Oats     # Root depth (mm)
sm1=SM.310$sm1.310
sm2=SM.310$sm2.310
sm3=SM.310$sm3.310
sm4=SM.310$sm4.310
sm5=SM.310$sm5.310
sm6=SM.310$sm6.310
sm7=SM.310$sm7.310
sm8=SM.310$sm8.310

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.310$Dates, -rd, 
     type='h', col='darkolivegreen', lwd='3', ylim=c(-2000, 0),
     main='Oats root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.310=c() # ET contribution from sensor 1
ET2.310=c() # ET contribution from sensor 2
ET3.310=c() # ET contribution from sensor 3
ET4.310=c() # ET contribution from sensor 4
ETd.310=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.310[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.310[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.310[i]<0) {ET1.310[i]=0}}         
plot(ET1.310, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.310[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.310[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.310[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.310[i]<0) {ET2.310[i]=0}}         
plot(ET2.310, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.310[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.310[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.310[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.310[i]<0) {ET3.310[i]=0} }         
plot(ET3.310, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.310[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.310[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.310[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.310[i]<0) {ET4.310[i]=0} }         
plot(ET4.310, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.310[i]=0  }
  else { # (sd4 < RD[i])
    ETd.310[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.310[i] < 0) {
    ETd.310[i]=0  }}
plot(ETd.310, type='h')

# Total ET for sensor 32
ET.32=ET1.310+ET2.310+ET3.310+ET4.310+ETd.310
ET.32[1]=NA # can't calculate ET the first day
plot(SM.310$Dates, ET.32,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 32', xlab='', ylab='ET (mm)')
lines(SM.310$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 32
Kc.32=ET.32/ETo
plot(SM.310$Dates, Kc.32,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 32', xlab='', ylab='Kc')

ET5.310=c() # ET contribution from sensor 5
ET6.310=c() # ET contribution from sensor 6
ET7.310=c() # ET contribution from sensor 7
ET8.310=c() # ET contribution from sensor 8
ETd.310=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.310[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.310[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.310[i]<0) {ET5.310[i]=0}}         
plot(ET5.310, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.310[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.310[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.310[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.310[i]<0) {ET6.310[i]=0}}         
plot(ET6.310, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.310[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.310[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.310[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.310[i]<0) {ET7.310[i]=0} }         
plot(ET7.310, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.310[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.310[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.310[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.310[i]<0) {ET8.310[i]=0} }         
plot(ET8.310, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.310[i]=0  }
  else { # (sd8 < RD[i])
    ETd.310[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.310[i] < 0) {
    ETd.310[i]=0  }}
plot(ETd.310, type='h')

# Total ET for sensor 310
ET.310=ET5.310+ET6.310+ET7.310+ET8.310+ETd.310
ET.310[1]=NA # can't calculate ET the first day
plot(SM.310$Dates, ET.310,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 310', xlab='', ylab='ET (mm)')
lines(SM.310$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 310
Kc.310=ET.310/ETo
plot(SM.310$Dates, Kc.310,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 310', xlab='', ylab='Kc')

### sensor 34/401 #######################################################
SM.401=data[c(1,2,86,133:140)]
SM.401=na.omit(SM.401)
n=length(SM.401$Dates) # Number of days with data
ETo=SM.401$ETo_mm     # Reference ET (mm)
rd=SM.401$Zr_Oats     # Root depth (mm)
sm1=SM.401$sm1.401
sm2=SM.401$sm2.401
sm3=SM.401$sm3.401
sm4=SM.401$sm4.401
sm5=SM.401$sm5.401
sm6=SM.401$sm6.401
sm7=SM.401$sm7.401
sm8=SM.401$sm8.401

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.401$Dates, -rd, 
     type='h', col='darkolivegreen', lwd='3', ylim=c(-2000, 0),
     main='Oats root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.401=c() # ET contribution from sensor 1
ET2.401=c() # ET contribution from sensor 2
ET3.401=c() # ET contribution from sensor 3
ET4.401=c() # ET contribution from sensor 4
ETd.401=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.401[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.401[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.401[i]<0) {ET1.401[i]=0}}         
plot(ET1.401, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.401[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.401[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.401[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.401[i]<0) {ET2.401[i]=0}}         
plot(ET2.401, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.401[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.401[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.401[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.401[i]<0) {ET3.401[i]=0} }         
plot(ET3.401, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.401[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.401[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.401[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.401[i]<0) {ET4.401[i]=0} }         
plot(ET4.401, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.401[i]=0  }
  else { # (sd4 < RD[i])
    ETd.401[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.401[i] < 0) {
    ETd.401[i]=0  }}
plot(ETd.401, type='h')

# Total ET for sensor 34
ET.34=ET1.401+ET2.401+ET3.401+ET4.401+ETd.401
ET.34[1]=NA # can't calculate ET the first day
plot(SM.401$Dates, ET.34,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 34', xlab='', ylab='ET (mm)')
lines(SM.401$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 34
Kc.34=ET.34/ETo
plot(SM.401$Dates, Kc.34,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 34', xlab='', ylab='Kc')

ET5.401=c() # ET contribution from sensor 5
ET6.401=c() # ET contribution from sensor 6
ET7.401=c() # ET contribution from sensor 7
ET8.401=c() # ET contribution from sensor 8
ETd.401=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.401[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.401[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.401[i]<0) {ET5.401[i]=0}}         
plot(ET5.401, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.401[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.401[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.401[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.401[i]<0) {ET6.401[i]=0}}         
plot(ET6.401, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.401[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.401[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.401[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.401[i]<0) {ET7.401[i]=0} }         
plot(ET7.401, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.401[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.401[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.401[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.401[i]<0) {ET8.401[i]=0} }         
plot(ET8.401, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.401[i]=0  }
  else { # (sd8 < RD[i])
    ETd.401[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.401[i] < 0) {
    ETd.401[i]=0  }}
plot(ETd.401, type='h')

# Total ET for sensor 401
ET.401=ET5.401+ET6.401+ET7.401+ET8.401+ETd.401
ET.401[1]=NA # can't calculate ET the first day
plot(SM.401$Dates, ET.401,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 401', xlab='', ylab='ET (mm)')
lines(SM.401$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 401
Kc.401=ET.401/ETo
plot(SM.401$Dates, Kc.401,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 401', xlab='', ylab='Kc')

### sensor 36/403 #######################################################
SM.403=data[c(1,2,86,142:149)]
SM.403=na.omit(SM.403)
n=length(SM.403$Dates) # Number of days with data
ETo=SM.403$ETo_mm     # Reference ET (mm)
rd=SM.403$Zr_Oats     # Root depth (mm)
sm1=SM.403$sm1.403
sm2=SM.403$sm2.403
sm3=SM.403$sm3.403
sm4=SM.403$sm4.403
sm5=SM.403$sm5.403
sm6=SM.403$sm6.403
sm7=SM.403$sm7.403
sm8=SM.403$sm8.403

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.403$Dates, -rd, 
     type='h', col='darkolivegreen', lwd='3', ylim=c(-2000, 0),
     main='Oats root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.403=c() # ET contribution from sensor 1
ET2.403=c() # ET contribution from sensor 2
ET3.403=c() # ET contribution from sensor 3
ET4.403=c() # ET contribution from sensor 4
ETd.403=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.403[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.403[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.403[i]<0) {ET1.403[i]=0}}         
plot(ET1.403, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.403[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.403[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.403[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.403[i]<0) {ET2.403[i]=0}}         
plot(ET2.403, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.403[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.403[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.403[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.403[i]<0) {ET3.403[i]=0} }         
plot(ET3.403, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.403[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.403[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.403[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.403[i]<0) {ET4.403[i]=0} }         
plot(ET4.403, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.403[i]=0  }
  else { # (sd4 < RD[i])
    ETd.403[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.403[i] < 0) {
    ETd.403[i]=0  }}
plot(ETd.403, type='h')

# Total ET for sensor 36
ET.36=ET1.403+ET2.403+ET3.403+ET4.403+ETd.403
ET.36[1]=NA # can't calculate ET the first day
plot(SM.403$Dates, ET.36,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 36', xlab='', ylab='ET (mm)')
lines(SM.403$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 36
Kc.36=ET.36/ETo
plot(SM.403$Dates, Kc.36,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 36', xlab='', ylab='Kc')

ET5.403=c() # ET contribution from sensor 5
ET6.403=c() # ET contribution from sensor 6
ET7.403=c() # ET contribution from sensor 7
ET8.403=c() # ET contribution from sensor 8
ETd.403=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.403[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.403[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.403[i]<0) {ET5.403[i]=0}}         
plot(ET5.403, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.403[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.403[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.403[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.403[i]<0) {ET6.403[i]=0}}         
plot(ET6.403, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.403[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.403[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.403[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.403[i]<0) {ET7.403[i]=0} }         
plot(ET7.403, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.403[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.403[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.403[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.403[i]<0) {ET8.403[i]=0} }         
plot(ET8.403, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.403[i]=0  }
  else { # (sd8 < RD[i])
    ETd.403[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.403[i] < 0) {
    ETd.403[i]=0  }}
plot(ETd.403, type='h')

# Total ET for sensor 403
ET.403=ET5.403+ET6.403+ET7.403+ET8.403+ETd.403
ET.403[1]=NA # can't calculate ET the first day
plot(SM.403$Dates, ET.403,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 403', xlab='', ylab='ET (mm)')
lines(SM.403$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 403
Kc.403=ET.403/ETo
plot(SM.403$Dates, Kc.403,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 403', xlab='', ylab='Kc')


### sensor 37/404 #######################################################
SM.404=data[c(1,2,86,151:158)]
SM.404=na.omit(SM.404)
n=length(SM.404$Dates) # Number of days with data
ETo=SM.404$ETo_mm     # Reference ET (mm)
rd=SM.404$Zr_Oats     # Root depth (mm)
sm1=SM.404$sm1.404
sm2=SM.404$sm2.404
sm3=SM.404$sm3.404
sm4=SM.404$sm4.404
sm5=SM.404$sm5.404
sm6=SM.404$sm6.404
sm7=SM.404$sm7.404
sm8=SM.404$sm8.404

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.404$Dates, -rd, 
     type='h', col='darkolivegreen', lwd='3', ylim=c(-2000, 0),
     main='Oats root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.404=c() # ET contribution from sensor 1
ET2.404=c() # ET contribution from sensor 2
ET3.404=c() # ET contribution from sensor 3
ET4.404=c() # ET contribution from sensor 4
ETd.404=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.404[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.404[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.404[i]<0) {ET1.404[i]=0}}         
plot(ET1.404, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.404[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.404[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.404[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.404[i]<0) {ET2.404[i]=0}}         
plot(ET2.404, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.404[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.404[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.404[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.404[i]<0) {ET3.404[i]=0} }         
plot(ET3.404, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.404[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.404[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.404[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.404[i]<0) {ET4.404[i]=0} }         
plot(ET4.404, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.404[i]=0  }
  else { # (sd4 < RD[i])
    ETd.404[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.404[i] < 0) {
    ETd.404[i]=0  }}
plot(ETd.404, type='h')

# Total ET for sensor 37
ET.37=ET1.404+ET2.404+ET3.404+ET4.404+ETd.404
ET.37[1]=NA # can't calculate ET the first day
plot(SM.404$Dates, ET.37,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 37', xlab='', ylab='ET (mm)')
lines(SM.404$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 37
Kc.37=ET.37/ETo
plot(SM.404$Dates, Kc.37,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 37', xlab='', ylab='Kc')

ET5.404=c() # ET contribution from sensor 5
ET6.404=c() # ET contribution from sensor 6
ET7.404=c() # ET contribution from sensor 7
ET8.404=c() # ET contribution from sensor 8
ETd.404=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.404[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.404[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.404[i]<0) {ET5.404[i]=0}}         
plot(ET5.404, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.404[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.404[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.404[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.404[i]<0) {ET6.404[i]=0}}         
plot(ET6.404, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.404[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.404[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.404[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.404[i]<0) {ET7.404[i]=0} }         
plot(ET7.404, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.404[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.404[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.404[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.404[i]<0) {ET8.404[i]=0} }         
plot(ET8.404, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.404[i]=0  }
  else { # (sd8 < RD[i])
    ETd.404[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.404[i] < 0) {
    ETd.404[i]=0  }}
plot(ETd.404, type='h')

# Total ET for sensor 404
ET.404=ET5.404+ET6.404+ET7.404+ET8.404+ETd.404
ET.404[1]=NA # can't calculate ET the first day
plot(SM.404$Dates, ET.404,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 404', xlab='', ylab='ET (mm)')
lines(SM.404$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 404
Kc.404=ET.404/ETo
plot(SM.404$Dates, Kc.404,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 404', xlab='', ylab='Kc')

### sensor 38/405 #######################################################
SM.405=data[c(1,2,86,160:167)]
SM.405=na.omit(SM.405)
n=length(SM.405$Dates) # Number of days with data
ETo=SM.405$ETo_mm     # Reference ET (mm)
rd=SM.405$Zr_Oats     # Root depth (mm)
sm1=SM.405$sm1.405
sm2=SM.405$sm2.405
sm3=SM.405$sm3.405
sm4=SM.405$sm4.405
sm5=SM.405$sm5.405
sm6=SM.405$sm6.405
sm7=SM.405$sm7.405
sm8=SM.405$sm8.405

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.405$Dates, -rd, 
     type='h', col='darkolivegreen', lwd='3', ylim=c(-2000, 0),
     main='Oats root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.405=c() # ET contribution from sensor 1
ET2.405=c() # ET contribution from sensor 2
ET3.405=c() # ET contribution from sensor 3
ET4.405=c() # ET contribution from sensor 4
ETd.405=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.405[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.405[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.405[i]<0) {ET1.405[i]=0}}         
plot(ET1.405, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.405[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.405[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.405[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.405[i]<0) {ET2.405[i]=0}}         
plot(ET2.405, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.405[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.405[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.405[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.405[i]<0) {ET3.405[i]=0} }         
plot(ET3.405, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.405[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.405[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.405[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.405[i]<0) {ET4.405[i]=0} }         
plot(ET4.405, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.405[i]=0  }
  else { # (sd4 < RD[i])
    ETd.405[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.405[i] < 0) {
    ETd.405[i]=0  }}
plot(ETd.405, type='h')

# Total ET for sensor 38
ET.38=ET1.405+ET2.405+ET3.405+ET4.405+ETd.405
ET.38[1]=NA # can't calculate ET the first day
plot(SM.405$Dates, ET.38,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 38', xlab='', ylab='ET (mm)')
lines(SM.405$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 38
Kc.38=ET.38/ETo
plot(SM.405$Dates, Kc.38,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 38', xlab='', ylab='Kc')

ET5.405=c() # ET contribution from sensor 5
ET6.405=c() # ET contribution from sensor 6
ET7.405=c() # ET contribution from sensor 7
ET8.405=c() # ET contribution from sensor 8
ETd.405=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.405[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.405[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.405[i]<0) {ET5.405[i]=0}}         
plot(ET5.405, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.405[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.405[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.405[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.405[i]<0) {ET6.405[i]=0}}         
plot(ET6.405, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.405[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.405[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.405[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.405[i]<0) {ET7.405[i]=0} }         
plot(ET7.405, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.405[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.405[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.405[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.405[i]<0) {ET8.405[i]=0} }         
plot(ET8.405, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.405[i]=0  }
  else { # (sd8 < RD[i])
    ETd.405[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.405[i] < 0) {
    ETd.405[i]=0  }}
plot(ETd.405, type='h')

# Total ET for sensor 405
ET.405=ET5.405+ET6.405+ET7.405+ET8.405+ETd.405
ET.405[1]=NA # can't calculate ET the first day
plot(SM.405$Dates, ET.405,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 405', xlab='', ylab='ET (mm)')
lines(SM.405$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 405
Kc.405=ET.405/ETo
plot(SM.405$Dates, Kc.405,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 405', xlab='', ylab='Kc')

### sensor 39/406 #######################################################
SM.406=data[c(1,2,86,169:176)]
SM.406=na.omit(SM.406)
n=length(SM.406$Dates) # Number of days with data
ETo=SM.406$ETo_mm     # Reference ET (mm)
rd=SM.406$Zr_Oats     # Root depth (mm)
sm1=SM.406$sm1.406
sm2=SM.406$sm2.406
sm3=SM.406$sm3.406
sm4=SM.406$sm4.406
sm5=SM.406$sm5.406
sm6=SM.406$sm6.406
sm7=SM.406$sm7.406
sm8=SM.406$sm8.406

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.406$Dates, -rd, 
     type='h', col='darkolivegreen', lwd='3', ylim=c(-2000, 0),
     main='Oats root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.406=c() # ET contribution from sensor 1
ET2.406=c() # ET contribution from sensor 2
ET3.406=c() # ET contribution from sensor 3
ET4.406=c() # ET contribution from sensor 4
ETd.406=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.406[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.406[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.406[i]<0) {ET1.406[i]=0}}         
plot(ET1.406, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.406[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.406[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.406[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.406[i]<0) {ET2.406[i]=0}}         
plot(ET2.406, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.406[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.406[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.406[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.406[i]<0) {ET3.406[i]=0} }         
plot(ET3.406, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.406[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.406[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.406[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.406[i]<0) {ET4.406[i]=0} }         
plot(ET4.406, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.406[i]=0  }
  else { # (sd4 < RD[i])
    ETd.406[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.406[i] < 0) {
    ETd.406[i]=0  }}
plot(ETd.406, type='h')

# Total ET for sensor 39
ET.39=ET1.406+ET2.406+ET3.406+ET4.406+ETd.406
ET.39[1]=NA # can't calculate ET the first day
plot(SM.406$Dates, ET.39,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 39', xlab='', ylab='ET (mm)')
lines(SM.406$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 39
Kc.39=ET.39/ETo
plot(SM.406$Dates, Kc.39,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 39', xlab='', ylab='Kc')

ET5.406=c() # ET contribution from sensor 5
ET6.406=c() # ET contribution from sensor 6
ET7.406=c() # ET contribution from sensor 7
ET8.406=c() # ET contribution from sensor 8
ETd.406=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.406[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.406[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.406[i]<0) {ET5.406[i]=0}}         
plot(ET5.406, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.406[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.406[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.406[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.406[i]<0) {ET6.406[i]=0}}         
plot(ET6.406, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.406[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.406[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.406[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.406[i]<0) {ET7.406[i]=0} }         
plot(ET7.406, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.406[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.406[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.406[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.406[i]<0) {ET8.406[i]=0} }         
plot(ET8.406, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.406[i]=0  }
  else { # (sd8 < RD[i])
    ETd.406[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.406[i] < 0) {
    ETd.406[i]=0  }}
plot(ETd.406, type='h')

# Total ET for sensor 406
ET.406=ET5.406+ET6.406+ET7.406+ET8.406+ETd.406
ET.406[1]=NA # can't calculate ET the first day
plot(SM.406$Dates, ET.406,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 406', xlab='', ylab='ET (mm)')
lines(SM.406$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 406
Kc.406=ET.406/ETo
plot(SM.406$Dates, Kc.406,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 406', xlab='', ylab='Kc')


### sensor 41/408 #######################################################
SM.408=data[c(1,2,86,187:194)]
SM.408=na.omit(SM.408)
n=length(SM.408$Dates) # Number of days with data
ETo=SM.408$ETo_mm     # Reference ET (mm)
rd=SM.408$Zr_Oats     # Root depth (mm)
sm1=SM.408$sm1.408
sm2=SM.408$sm2.408
sm3=SM.408$sm3.408
sm4=SM.408$sm4.408
sm5=SM.408$sm5.408
sm6=SM.408$sm6.408
sm7=SM.408$sm7.408
sm8=SM.408$sm8.408

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.408$Dates, -rd, 
     type='h', col='darkolivegreen', lwd='3', ylim=c(-2000, 0),
     main='Oats root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.408=c() # ET contribution from sensor 1
ET2.408=c() # ET contribution from sensor 2
ET3.408=c() # ET contribution from sensor 3
ET4.408=c() # ET contribution from sensor 4
ETd.408=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.408[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.408[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.408[i]<0) {ET1.408[i]=0}}         
plot(ET1.408, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.408[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.408[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.408[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.408[i]<0) {ET2.408[i]=0}}         
plot(ET2.408, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.408[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.408[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.408[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.408[i]<0) {ET3.408[i]=0} }         
plot(ET3.408, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.408[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.408[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.408[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.408[i]<0) {ET4.408[i]=0} }         
plot(ET4.408, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.408[i]=0  }
  else { # (sd4 < RD[i])
    ETd.408[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.408[i] < 0) {
    ETd.408[i]=0  }}
plot(ETd.408, type='h')

# Total ET for sensor 41
ET.41=ET1.408+ET2.408+ET3.408+ET4.408+ETd.408
ET.41[1]=NA # can't calculate ET the first day
plot(SM.408$Dates, ET.41,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 41', xlab='', ylab='ET (mm)')
lines(SM.408$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 41
Kc.41=ET.41/ETo
plot(SM.408$Dates, Kc.41,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 41', xlab='', ylab='Kc')

ET5.408=c() # ET contribution from sensor 5
ET6.408=c() # ET contribution from sensor 6
ET7.408=c() # ET contribution from sensor 7
ET8.408=c() # ET contribution from sensor 8
ETd.408=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.408[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.408[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.408[i]<0) {ET5.408[i]=0}}         
plot(ET5.408, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.408[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.408[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.408[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.408[i]<0) {ET6.408[i]=0}}         
plot(ET6.408, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.408[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.408[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.408[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.408[i]<0) {ET7.408[i]=0} }         
plot(ET7.408, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.408[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.408[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.408[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.408[i]<0) {ET8.408[i]=0} }         
plot(ET8.408, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.408[i]=0  }
  else { # (sd8 < RD[i])
    ETd.408[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.408[i] < 0) {
    ETd.408[i]=0  }}
plot(ETd.408, type='h')

# Total ET for sensor 408
ET.408=ET5.408+ET6.408+ET7.408+ET8.408+ETd.408
ET.408[1]=NA # can't calculate ET the first day
plot(SM.408$Dates, ET.408,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 408', xlab='', ylab='ET (mm)')
lines(SM.408$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 408
Kc.408=ET.408/ETo
plot(SM.408$Dates, Kc.408,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 408', xlab='', ylab='Kc')

### sensor 42/409 #######################################################
SM.409=data[c(1,2,86,196:203)]
SM.409=na.omit(SM.409)
n=length(SM.409$Dates) # Number of days with data
ETo=SM.409$ETo_mm     # Reference ET (mm)
rd=SM.409$Zr_Oats     # Root depth (mm)
sm1=SM.409$sm1.409
sm2=SM.409$sm2.409
sm3=SM.409$sm3.409
sm4=SM.409$sm4.409
sm5=SM.409$sm5.409
sm6=SM.409$sm6.409
sm7=SM.409$sm7.409
sm8=SM.409$sm8.409

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.409$Dates, -rd, 
     type='h', col='darkolivegreen', lwd='3', ylim=c(-2000, 0),
     main='Oats root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.409=c() # ET contribution from sensor 1
ET2.409=c() # ET contribution from sensor 2
ET3.409=c() # ET contribution from sensor 3
ET4.409=c() # ET contribution from sensor 4
ETd.409=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.409[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.409[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.409[i]<0) {ET1.409[i]=0}}         
plot(ET1.409, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.409[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.409[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.409[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.409[i]<0) {ET2.409[i]=0}}         
plot(ET2.409, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.409[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.409[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.409[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.409[i]<0) {ET3.409[i]=0} }         
plot(ET3.409, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.409[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.409[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.409[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.409[i]<0) {ET4.409[i]=0} }         
plot(ET4.409, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.409[i]=0  }
  else { # (sd4 < RD[i])
    ETd.409[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.409[i] < 0) {
    ETd.409[i]=0  }}
plot(ETd.409, type='h')

# Total ET for sensor 42
ET.42=ET1.409+ET2.409+ET3.409+ET4.409+ETd.409
ET.42[1]=NA # can't calculate ET the first day
plot(SM.409$Dates, ET.42,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 42', xlab='', ylab='ET (mm)')
lines(SM.409$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 42
Kc.42=ET.42/ETo
plot(SM.409$Dates, Kc.42,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 42', xlab='', ylab='Kc')

ET5.409=c() # ET contribution from sensor 5
ET6.409=c() # ET contribution from sensor 6
ET7.409=c() # ET contribution from sensor 7
ET8.409=c() # ET contribution from sensor 8
ETd.409=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.409[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.409[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.409[i]<0) {ET5.409[i]=0}}         
plot(ET5.409, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.409[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.409[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.409[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.409[i]<0) {ET6.409[i]=0}}         
plot(ET6.409, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.409[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.409[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.409[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.409[i]<0) {ET7.409[i]=0} }         
plot(ET7.409, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.409[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.409[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.409[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.409[i]<0) {ET8.409[i]=0} }         
plot(ET8.409, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.409[i]=0  }
  else { # (sd8 < RD[i])
    ETd.409[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.409[i] < 0) {
    ETd.409[i]=0  }}
plot(ETd.409, type='h')

# Total ET for sensor 409
ET.409=ET5.409+ET6.409+ET7.409+ET8.409+ETd.409
ET.409[1]=NA # can't calculate ET the first day
plot(SM.409$Dates, ET.409,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 409', xlab='', ylab='ET (mm)')
lines(SM.409$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 409
Kc.409=ET.409/ETo
plot(SM.409$Dates, Kc.409,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 409', xlab='', ylab='Kc')

### sensor 43/410 #######################################################
SM.410=data[c(1,2,86,205:212)]
SM.410=na.omit(SM.410)
n=length(SM.410$Dates) # Number of days with data
ETo=SM.410$ETo_mm     # Reference ET (mm)
rd=SM.410$Zr_Oats     # Root depth (mm)
sm1=SM.410$sm1.410
sm2=SM.410$sm2.410
sm3=SM.410$sm3.410
sm4=SM.410$sm4.410
sm5=SM.410$sm5.410
sm6=SM.410$sm6.410
sm7=SM.410$sm7.410
sm8=SM.410$sm8.410

# sensor depth (mm)
sd1=sd5=1.5*305 # sensor 1: from surface to 1.5ft
sd2=sd6=2.5*305 # sensor 2: from 1.5ft to 2.5ft
sd3=sd7=3.5*305 # sensor 3: from 2.5ft to 3.5ft
sd4=sd8=6.5*305 # sensor 4: from 3.5ft to 4.5ft

# plot root depth with sensor depth
plot(SM.410$Dates, -rd, 
     type='h', col='darkolivegreen', lwd='3', ylim=c(-2000, 0),
     main='Oats root depth', ylab='Soil depth (mm)', xlab=NA)
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
legend('bottomleft', inset=0.02, lty=1, lwd=2, legend=c('s1','s2','s3','s4'), col=c('coral4','steelblue4','gold4','olivedrab4'), ncol=2)

ET1.410=c() # ET contribution from sensor 1
ET2.410=c() # ET contribution from sensor 2
ET3.410=c() # ET contribution from sensor 3
ET4.410=c() # ET contribution from sensor 4
ETd.410=c() # ET contribution past sensor 4 (deep)
RD=c()      # Adjusted root depth - average with previous day
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today
# ET from sensor 1
for(i in 2:n) { 
  # rd->[0,sd1]
  if (RD[i] < sd1) {
    ET1.410[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  # rd->[sd1, inf]
  else {
    ET1.410[i]=(sm1[i-1]-sm1[i])/100*sd1}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET1.410[i]<0) {ET1.410[i]=0}}         
plot(ET1.410, type='h')
# ET from sensor 2
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.410[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2) {
    ET2.410[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.410[i]=(sm2[i-1]-sm2[i])/100*(sd2-sd1) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.410[i]<0) {ET2.410[i]=0}}         
plot(ET2.410, type='h')
# ET from sensor 3
for(i in 2:n) {
  if (RD[i] <= sd2) {
    ET3.410[i]=0}
  else if (sd2 < RD[i] & RD[i] <= sd3) {
    ET3.410[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2) }
  else {
    ET3.410[i]=(sm3[i-1]-sm3[i])/100*(sd3-sd2) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.410[i]<0) {ET3.410[i]=0} }         
plot(ET3.410, type='h')
# ET from sensor 4
for(i in 2:n) {
  if (RD[i] <= sd3) {
    ET4.410[i]=0}
  else if (sd3 < RD[i] & RD[i] <= sd4) {
    ET4.410[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3) }
  else {
    ET4.410[i]=(sm4[i-1]-sm4[i])/100*(sd4-sd3) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.410[i]<0) {ET4.410[i]=0} }         
plot(ET4.410, type='h')
# ET past sensor 4 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd4) {
    ETd.410[i]=0  }
  else { # (sd4 < RD[i])
    ETd.410[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd4)  }}
for (i in 2:n) {
  if (ETd.410[i] < 0) {
    ETd.410[i]=0  }}
plot(ETd.410, type='h')

# Total ET for sensor 43
ET.43=ET1.410+ET2.410+ET3.410+ET4.410+ETd.410
ET.43[1]=NA # can't calculate ET the first day
plot(SM.410$Dates, ET.43,
     ylim=c(0, 15),
     type='h', col='darkslategray4', lwd=2,
     main='Daily ET for sensor 43', xlab='', ylab='ET (mm)')
lines(SM.410$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray4'), lwd=2, inset=0.02)
# Crop coefficient for sensor 43
Kc.43=ET.43/ETo
plot(SM.410$Dates, Kc.43,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid4',
     main='Daily crop coefficient for sensor 43', xlab='', ylab='Kc')

ET5.410=c() # ET contribution from sensor 5
ET6.410=c() # ET contribution from sensor 6
ET7.410=c() # ET contribution from sensor 7
ET8.410=c() # ET contribution from sensor 8
ETd.410=c() # ET contribution past sensor 8 (deep)

# ET from sensor 5
for(i in 2:n) {
  # rd->[0,sd5]
  if (RD[i] < sd5) {
    ET5.410[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  # rd->[sd5, inf]
  else {
    ET5.410[i]=(sm5[i-1]-sm5[i])/100*sd5}}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.410[i]<0) {ET5.410[i]=0}}         
plot(ET5.410, type='h')
# ET from sensor 6
for(i in 2:n) {
  if (RD[i] <= sd5) {
    ET6.410[i]=0}
  else if (sd5 < RD[i] & RD[i] <= sd6) {
    ET6.410[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5) }
  else {
    ET6.410[i]=(sm6[i-1]-sm6[i])/100*(sd6-sd5) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.410[i]<0) {ET6.410[i]=0}}         
plot(ET6.410, type='h')
# ET from sensor 7
for(i in 2:n) {
  if (RD[i] <= sd6) {
    ET7.410[i]=0}
  else if (sd6 < RD[i] & RD[i] <= sd7) {
    ET7.410[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6) }
  else {
    ET7.410[i]=(sm7[i-1]-sm7[i])/100*(sd7-sd6) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.410[i]<0) {ET7.410[i]=0} }         
plot(ET7.410, type='h')
# ET from sensor 8
for(i in 2:n) {
  if (RD[i] <= sd7) {
    ET8.410[i]=0}
  else if (sd7 < RD[i] & RD[i] <= sd8) {
    ET8.410[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd7) }
  else {
    ET8.410[i]=(sm8[i-1]-sm8[i])/100*(sd8-sd7) }}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET8.410[i]<0) {ET8.410[i]=0} }         
plot(ET8.410, type='h')
# ET past sensor 8 (ET deep)
for (i in 2:n) {
  if (RD[i] <= sd8) {
    ETd.410[i]=0  }
  else { # (sd8 < RD[i])
    ETd.410[i]=(sm8[i-1]-sm8[i])/100*(RD[i]-sd8)  }}
for (i in 2:n) {
  if (ETd.410[i] < 0) {
    ETd.410[i]=0  }}
plot(ETd.410, type='h')

# Total ET for sensor 410
ET.410=ET5.410+ET6.410+ET7.410+ET8.410+ETd.410
ET.410[1]=NA # can't calculate ET the first day
plot(SM.410$Dates, ET.410,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Daily ET for sensor 410', xlab='', ylab='ET (mm)')
lines(SM.410$Dates, ETo, type='l', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)
# Crop coefficient for sensor 410
Kc.410=ET.410/ETo
plot(SM.410$Dates, Kc.410,
     ylim=c(0, 2),
     type='h', lwd=2, col='orchid3',
     main='Daily crop coefficient for sensor 410', xlab='', ylab='Kc')
