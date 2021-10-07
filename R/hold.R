

library(dplyr)

setwd("~/Projects/cnpjatual")
load("empresas.rda")


################################################################################
# Sócios
################################################################################


socios <- function(nome = "nome", cpf = NULL){
    empresas <- socio[socio$nome == toupper(nome), ]
    
    if(!is.null(cpf)){
      empresas <- empresas[empresas$cpfcnpj == cpf, ]
    }
    
    emprenome <- empre[empre$cnpj %in% empresas$cnpj,  ]
    saosocios <- socio[socio$cnpj %in% empresas$cnpj, ]
    
    cnpj <- saosocios$cpfcnpj
    cnpj <- gsub("\\*{3}\\d{6}\\*{2}", "", cnpj)
    cnpj <- names(table(cnpj))
    cnpj <- cnpj[-1]
    
    if(length(cnpj) != 0){
        cnpj <- gsub("\\d{6}$", "", cnpj)
        socioempre <- empre[empre$cnpj %in%  cnpj, ]
    }
  
    for(i in 1:dim(empresas)[1]){
      cat("\n")
      cat(paste0(i, " - ", emprenome[emprenome$cnpj == empresas$cnpj[i], ]$razaosocial))
      cat("\n")
      cat(paste0("- ",saosocios[saosocios$cnpj == empresas$cnpj[i],]$nome, "\n"))
      cat("\n")
    }
    if(length(cnpj) != 0){
      cat("\n")
      cat("\n")
      cat("\n")
      cat("Outras empresas \n")
      cat(paste0("- ", socioempre$razaosocial, "\n"))
    
    }
}

socios("LEONARDO BARROS JIANOTI")
socios("MARIANA THOME FORESTI")
socios("JOAO GUILHERME DEL VALLE")
socios("ALPHONSE GUILHERME VOIGT")
socios("ARIEL PATSCHIKI")
socios("WAGNER ALEXIS RUIZ")
socios("Thiago Alves de Souza")
socios("ANDRE BOAVENTURA CZELUSNIAK")
socios("CASSIO HIDEYUKI MIKODA")
socios("Walter Gomes Moreira Neto")
socios("PATRICK HERNANDEZ RAVAILHE")
socios("michelle bau graczyk")
socios("DALTON LUIZ GRACZYK")
socios("Daniele Bau")
socios("LISANDRA VICENTINI CASTRO")
socios("RODRIGO SCHIAVINI")
socios("Antonio Caito Maia Gomes Pereira")
socios("CAMILA FARANI LIMA PORRECA")
socios("Ana Carolina Paifer")
socios("Jose Carlos Semenzato")
socios("JOAO BOSCHILIA APPOLINARIO")
socios("Marcio Carvalho de Sa")
socios("ELIZANGELA FIGUEIREDO DE SOUZA")
socios("Antonio Augusto Moraes Liberato")
socios("Gabriela Saraiva Accorsi")
socios("Anderson Mendonca Thees")
socios("ROMERO VENANCIO RODRIGUES FILHO")
socios("RODRIGO FERNANDES BAHIA BAER")
socios("Aldair Jose Goncalves da Cunha")
socios("ALDAIR JOSE GONCALVES DA CUNHA")

head(estabe, 2)

aa <- data.frame(table(estabe$email))


cont <- aa[grepl("CONTABILIZEI", aa$Var1), ]
aa <- estabe[grepl("CONTABILIZEI", estabe$email), ]

socio[socio$cnpj == "42427958", ]
empre[empre$cnpj == "42427958", ]

a <- gsub(pattern = "(\\d{4})(\\d{2})(\\d{2})", replacement = "\\1-\\2-\\3", x = aa$inicio)
a <- gsub(pattern = "(\\d{4})(\\d{2})(\\d{2})", replacement = "\\1", x = hold$inicio)
a <- gsub(pattern = "(\\d\\d\\d\\d)(\\d\\d)(\\d\\d)", replacement = "\\1-\\2-\\3", x = aa$situacao.data)

a <- as.Date(a, format =  "%Y-%m-%d")
a <- as.Date(a, format =  "%Y")
min(a)

aa[aa$inicio %in% 19810821, ]
socio[socio$cnpj %in% "27586627", ]
hold <- estabe[estabe$cnae1 %in% "6462000", ]
hold2 <- estabe[estabe$cnae2 %in% "6462000", ]
hold <- rbind(hold, hold2)

plot(table(a))
table(hold$logra.uf)


hold2 <- empre[empre$cnpj %in% hold$cnpj, ]
hold2$capital
hold2$capital <- gsub(pattern = ",00", replacement = "", x = hold2$capital)
hold2$capital <- as.numeric(hold2$capital)

hold2 <- hold2[!is.na(hold2$capital), ]
hold2 <- hold2[!hold2$capital > 50000000, ]
plot(table(hold2$capital))
min(hold2$capital)

a <- data.frame(table(hold$email))


a <- socio[grepl(toupper("BAHIA BAER"), socio$nome ), ]
a <- a[grepl(toupper("rodrigo"), a$nome ), ]
table(a$cpfcnpj)

head(estabe)
empre <- rbind(empre, empre.off)
socio <- rbind(socio, socio.off)
estabe <- rbind(estabe, estabe.off)


aa <- empre[grepl(toupper("Eventures"), empre$razaosocial), ]
ab <- aa[grepl(toupper("tecnologia"), aa$razaosocial), ]

################################################################################
# Sócio - Empresas / Estabelecimento
################################################################################

empresas <- function(nome, cpf = NULL){
    df1 <- socio[socio$nome == toupper(nome) , ]
    if(!is.null(cpf)){
      df1 <- df1[df1$cpfcnpj == cpf, ]
    }
    df2 <- empre[empre$cnpj %in% df1$cnpj,  ]
    df3 <- estabe[estabe$cnpj %in% df1$cnpj, ]
    if(length(table(df1$cpfcnpj)) != 1){
      print("ALERTA: Possui homônimos")
    }
    (merge(df3, df2, by = "cnpj") %>% merge(df1, by ="cnpj"))
}


socio[socio$cpfcnpj == "***444658**", ]

empresas("ARIEL PATSCHIKI")
empresas("ANDRE BOAVENTURA CZELUSNIAK")
empresas("Anderson Roberto Godzikowski")
socios("Leandro Henrique de Souza", "***039449**")
empresas("Leandro Henrique de Souza", "***039449**")
empresas("Thiago Alves de Souza", "***505999**")
empresas("Walter Gomes Moreira Neto")
empresas("LISANDRA VICENTINI CASTRO")
empresas("victor hugo de moraes")
empresas("RODRIGO SCHIAVINI")
empresas("Antonio Caito Maia Gomes Pereira")
empresas("CAMILA FARANI LIMA PORRECA")
empresas("Jose Carlos Semenzato")
empresas("Alexandra Siranding Baldeh Loras")
empresas("JOAO BOSCHILIA APPOLINARIO")
empresas("Marcio Carvalho de Sa")
empresas("Antonio Augusto Moraes Liberato")
empresas("Marina Liberato")
empresas("Carlos Roberto Massa")
empresas("Carlos Roberto Massa Junior")
empresas("Anderson Mendonca Thees")
empresas("ROMERO VENANCIO RODRIGUES FILHO")
empresas("RODRIGO FERNANDES BAHIA BAER")
empresas("FELIPE CEZAR AMADEU ESTEVES")
empresas("ERICO ARAUJO ROCHA")

socio[socio$cnpj == "10332345", ]

#  CSM ACADEMY  SERVICOS DE EDUCACAO E TECNOLOGIA LTDA

################################################################################
# Empresas CNAE
################################################################################

apart <- base[grepl("APART-HOTÉIS", base$ATIVIDADE_PRINCIPAL), ]
apartcnpj <- empresas[empresas$razao_social %in% apart$NOME_EMPRESARIAL, ]
apartcnpj <- apartcnpj[apartcnpj$uf == "PR", ]



