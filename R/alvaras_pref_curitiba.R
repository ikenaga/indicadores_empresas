library(stringi)
library(xml2)
library (RCurl)
library(dplyr)
library(ggplot2)
library(magrittr)



load("empresas.rda")

################################################################################
# Alvaras
################################################################################
base  <- read.csv("~/Projects/alvaras/2021-01-01_Alvaras-Base_de_Dados.CSV", sep=";", stringsAsFactors=FALSE)
sort(table(base[stri_detect_fixed(base$ATIVIDADE_PRINCIPAL,"HOLDINGS"), 15]), decreasing = TRUE)
name <- base[stri_detect_fixed(base$ATIVIDADE_PRINCIPAL,"HOLDINGS"), ][1]
invest <- name[stri_detect_fixed(name$NOME_EMPRESARIAL,"INVESTIMENT"), ]
investcnpj <- empresas[empresas$razao_social %in% invest, ]
investcnpj <- investcnpj[investcnpj$uf == "PR", ]
a <- grepl("APART-HOTÉIS", base$ATIVIDADE_PRINCIPAL)
table(toupper(iconv(gsub(" ", "", base$BAIRRO),from="UTF-8",to="ASCII//TRANSLIT")))
table(toupper(base$BAIRRO))



################################################################################
# CNPJ Receita Federal
################################################################################
setwd("~/Projects/cnpjatual/data")
url <- "https://www.gov.br/receitafederal/pt-br/assuntos/orientacao-tributaria/cadastros/consultas/dados-publicos-cnpj"
doc <- url %>% read_html
doc2 <- xml_find_all(doc, xpath = "//a[contains(@href, '.zip')]") %>% 
  xml_attr("href") %>% unlist

for(i in 1:length(doc2)){
  system(paste0("wget ", doc2[i]))
}


doc <- dir()
for(i in 1:length(doc)){
  unzip(doc[i])
  system(paste0("rm ", doc[i]))
}

doc <- dir()
a1 <- grepl(doc,pattern = "EMPRECSV")
a2 <- grepl(doc,pattern = "ESTABELE")
a3 <- grepl(doc,pattern = "SOCIOCSV")



i <- 1

empresa <- data.frame()

empresa <- read.csv(doc[a3][10], header=FALSE, sep=";")



# TABELA DE NATUREZA JURÍDICA 2018
# https://concla.ibge.gov.br/estrutura/natjur-estrutura/natureza-juridica-2018


save(list = ls(), file = "empresas.rda")

hold <- estabe[estabe$V12 == 6462000, ]
emails <- data.frame(table(hold$V28))
idade <- data.frame(table(socio$V11))


barcelona <- grepl("BARCELONA", empre$V2)

barcelona <- empre[barcelona, ]
barcelona2 <- grepl("INVEST", barcelona$V2)
barcelona2 <- barcelona[barcelona2, ]

aa2 <- estabe[estabe$V1 %in% barcelona2$V1, ]

empresasname <- socio[grepl(toupper("Guylherme Rivas Mendes Miranda"), socio$V3),]
empresasname[grepl(toupper(""), empresasname$V3), ]

socio[socio$V1 %in% empresasname$V1, ]
socio[socio$V1 == "37386147", ]

estabe[estabe$V1 %in% empresasname$V1, ]
estabe[estabe$V1 == "37386147", ]

empre[empre$V1 %in% empresasname$V1, ]
empre[empre$V1 == "37386147", ]


mun <- read.csv("~/Projects/cnpjatual/outros/F.K03200$Z.D10510.MUNICCSV", header=FALSE, sep=";", colClasses = c("character", "character"))
mun[mun$V2 == "CURITIBA", ] # 7535

curitiba <- estabe[estabe$V21 == 7535, ]
holdcuritiba <- curitiba[curitiba$V12 == 6462000, ]

dir()
unzip("F.K03200$Z.D10510.CNAECSV.zip")
