library(dplyr)
library(xml2)
# library (RCurl)

setwd("~/Projects/cnpjatual")
# https://www.gov.br/receitafederal/pt-br/assuntos/orientacao-tributaria/cadastros/consultas/dados-publicos-cnpj

url <- paste0("https://www.gov.br/receitafederal/pt-br/assuntos/",
              "orientacao-tributaria/cadastros/consultas/dados-publicos-cnpj")
doc <- url %>% read_html
doc2 <- xml_find_all(doc, xpath = "//a[contains(@href, '.zip')]") %>% 
  xml_attr("href") %>% unlist

# usar o shell no diretorio data -----------------------------------------------
print(paste("wget", doc2))

rm(doc2, url, doc)
#-------------------------------------------------------------------------------
setwd("~/Projects/cnpjatual/data2")
files <- dir(pattern = "*.zip")

for(i in 1:length(files)){
  unzip(zipfile = files[i])
  system(paste0("rm ", files[i]))
}

#-------------------------------------------------------------------------------
files <- dir(pattern = "SOCIOCSV")
socio <- data.frame()
for(i in 1:length(files)){
  temp <- read.csv(files[i], header=FALSE, sep=";")
  socio <- rbind(socio, temp)
}
rm(temp, files, i)
save(socio, file = "socio01.rda")


files <- dir(pattern = "ESTABELE")
estabe <- data.frame()
for(i in 1:length(files)){
  temp <- read.csv(files[i], header=FALSE, sep=";")
  estabe <- rbind(estabe, temp)
}
rm(temp, files, i)
save(estabe, file = "estabe01.rda")


files <- dir(pattern = "EMPRECSV")
empre <- data.frame()
for(i in 1:length(files)){
  temp <- read.csv(files[i], header=FALSE, sep=";")
  empre <- rbind(empre, temp)
}
rm(temp, files, i)
save(empre, file = "empre01.rda")

#-------------------------------------------------------------------------------

cnae <- "6462000"
edf1 <- estabe[estabe$cnae1 %in% cnae, ]
edf2 <- estabe[estabe$cnae2 %in% cnae, ]

mdf1 <- empre[empre$cnpj %in% edf1$cnpj, ]
mdf2 <- empre[empre$cnpj %in% edf2$cnpj, ]

sdf1 <- socio[socio$cnpj %in% edf1$cnpj, ]
sdf2 <- socio[socio$cnpj %in% edf2$cnpj, ]

merg1 <- merge(edf1, mdf1, by = "cnpj")
merg2 <- merge(edf2, mdf2, by = "cnpj")

df1 <- merge(merg1, sdf1, by = "cnpj")
df2 <- merge(merg2, sdf2, by = "cnpj")
df1 <- merge(socio1, merg1, by = "cnpj", all=TRUE)
df2 <- merge(socio2, merg2, by = "cnpj", all=TRUE)

save(sdf1, sdf2, merg1, merg2, file = "hold.rda")

socio[socio$cnpj == "28633044", ]
semestabe <- socio[!socio$cnpj %in% estabe$cnpj, ]
semsocio <- estabe[!estabe$cnpj %in% socio$cnpj, ]

################################################################################
# Baixar daqui - atualizado
################################################################################


head(socio)
names(socio) <- c("cnpj",  "tipo",  "nome",  "cpfcnpj",  "qualif",  "entrada", 
                  "pais",  "represen.cpf",  "represen.nome",  "represen.qualif",  
                  "faixaetaria")

head(empre)
names(empre) <- c("cnpj", "razaosocial", "natureza", "qualif", "capital", 
                  "porte", "uf")

head(estabe)
names(estabe) <- c("cnpj", "cnpj.ordem", "cnpj.dv", "matriz", "fantasia", 
                   "situacao", "situacao.data", "situacao.motivo", 
                   "extrangeiro.cidade", "extrangeiro.pais", "inicio", "cnae1", 
                   "cnae2", "logra.tipo", "logra.nome", "logra.numero", 
                   "logra.comp", "logra.bairro", "logra.cep", "logra.uf", 
                   "logra.mun", "tel.d1", "tel.n1", "tel.d2", "tel.n2", "fax.d", 
                   "fax.n", "email", "situacaoespecial", "situacaoesp.data")

save(empre, estabe, socio, file="empresas.rda")

