library(readxl)

url <- "https://www.gov.br/economia/pt-br/assuntos/drei/tabelas-de-dispensa-de-alvara/REGIAO_SUL092021.xlsx"
destfile <- "REGIAO_SUL092021.xlsx"
curl::curl_download(url, destfile)
REGIAO_SUL092021 <- read_excel(destfile)


table(REGIAO_SUL092021$MUNICÍPIO == "CURITIBA")
str(REGIAO_SUL092021)

curitiba <- REGIAO_SUL092021[REGIAO_SUL092021$MUNICÍPIO == "CURITIBA",  ]

table(curitiba$CNAE)

curitiba$categoria <- gsub("(\\d\\d).+", "\\1", curitiba$CNAE)

cwb <- curitiba[curitiba$categoria == "62", ]
table(cwb$CNAE)
