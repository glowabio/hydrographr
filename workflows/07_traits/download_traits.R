library(fwtraits)
fw_be4ustart()
fw_setapikey()

library(httr)
res = httr::GET("https://www.freshwaterecology.info/fweapi2/v1/status")
cat(content(res, 'text', encode = "UTF-8"))

library(httr)
headers = c(
  'Content-Type' = 'application/json'
)
body = '{
        "apikey": "your_apikey"
    }';
res = httr::POST("https://www.freshwaterecology.info/fweapi2/v1/token", body = body, add_headers(headers))
cat(content(res, 'text', encode = "UTF-8"))


migration <- fw_fetchdata(data = 'Abramis brama',
                          organismgroup = 'fi',
                          ecoparams = 'migration',
                          cachefolder = 'cache',
                          warn = TRUE,
                          inform = TRUE,
                          details = TRUE)#the species spelling is checked
