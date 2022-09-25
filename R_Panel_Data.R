library(plm)
datapanel<- read.csv("Data_olah.csv")
pdata= pdata.frame(datapanel, index = c("Provinsi","Tahun"))
#model
model = Peng_Tbk ~ R_Sek + Ln_UMP + TKK + Ln_PDRB

#Pooled Least Square
pls = plm(model, data=pdata, model="pooling")
summary(pls)

#Fixed Effect Model
fem = plm(model, data=pdata, model="within")
summary(fem)

#Random Effect Model
rem = plm(model, data=pdata, model="random")
summary(rem)

#Poolability Test
pooltest(model, data=pdata, model="within")

#Individual and Time Effect Test
plmtest(model, data=pdata, effect = "twoway", type = "ghm")

#Memilih PLS atau FEM
pFtest (fem, pls)

#Haussman Test (FEM/REM)
phtest(fem, rem)

#Unit Root Test
ram = data.frame(split(datapanel$GDP, datapanel$Country))
purtest(ram, pmax=2, exo="intercept", test="levinlin")
purtest(ram, pmax=2, exo="intercept", test="madwu")
