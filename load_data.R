#-------------------Load data-------------------------------------------------------

df <- read_excel("Copy of SalesDB - JK.xlsx")

colnames(df)<- c("CLIENT.CODE","KAM","DISTRICT","CLIENT.TYPE.CODE","CLIENT.TYPE","CLIENT","SUFFIX","INVOICE.CATEGORY","DISCOUNT...9","PRODUCT.CODE","PRODUCT","QUANTITY","FREE.UNITS"
                 ,"TOTAL QTY","SUPPLIER CODE","SUPPLIER","BRANCH CODE","BRANCH","YEAR","MONTH","FOB TO","REALTO","GTHEOTO","THEOTO","MARGIN","DISCOUNT...26","GLOBDISC","REGCLI" 
                 ,"RESP BRANCH","TELESALE","NEW.CLASSIFICATION" ,"RESP.SUPPLIER" ,"FOCUS.PRODUCT")


df$MONTH <- as.factor(mapvalues(df$MONTH,
                                from=c(1,2,3,4,5,6,7,8,9,10,11,12),
                                to=c("01-01","02-01","03-01","04-01","05-01","06-01","07-01","08-01","09-01","10-01","11-01","12-01")))
df$YEAR <- ifelse(df$YEAR == "20","2020-","2019-")
df$DATE <- paste(df$YEAR,df$MONTH)
df$DATE <- as.Date(df$DATE)