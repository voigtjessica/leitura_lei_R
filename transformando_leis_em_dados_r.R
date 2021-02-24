
# Instalando pacotes necessários:
# install.packages("pdftools")
# install.packages("tidyverse")

# carregar pacotes
library(pdftools)
library(tidyverse)
library(stringr)
library(stringi)

# Setar diretório
setwd("/Users/user/Documents/Scripts e notebooks/R scripts/leitura_lei")

# importa o pdf como vetor de txt
lei_original <- pdf_text("LAI.pdf")
lei <- lei_original # segurança

# transformando todas as páginas em uma só (um vetor de um elemento)
lei <- paste(lei, collapse = ' ')

# Tirando espaços duplos
lei <- gsub("  +"," ", lei)

# Criando um vetor com todos os artigos separados: Aqui o padrão é "\n Art."
lei_art <-unlist(str_split(lei, "\n Art."))
lei_art <- lei_art[-1]

# Rotina para criar dois dfs: um dos caputs e outro dos parágrafos:
caput <- data.frame(conteudo  = "",
                    tipo  = "",
                    artigo_referencia = "")

par <- data.frame(conteudo  = "",
                  tipo  = "",
                  artigo_referencia = "",
                  paragrafo_referencia = "")

# início do loop:
for(u in 1:length(lei_art)){
  i = lei_art[u]
  a <- substr(i, 1, 5) # Tirando o numero do art   
  a <- gsub('[^0-9]+', '', a) 
  if (grepl("\n Parágrafo único", i)){
    cont <- unlist(strsplit(i, "\n Parágrafo único"))
    i <- data.frame(conteudo = cont,
                    paragrafo_referencia = "paragrafo unico",
                    artigo_referencia = a)
    # dataframe contendo os conteúdos dos paragrafos, sendo o primeiro o caput
  } else {
    cont <- unlist(strsplit(i, "\n § "))
    # dataframe contendo os conteúdos dos paragrafos, sendo o primeiro o caput
    i <- data.frame(conteudo = cont)
    i <- i %>%
      mutate(paragrafo_referencia = substr(conteudo, 1, 4),
             paragrafo_referencia = gsub('[^0-9]+', '', paragrafo_referencia),
             artigo_referencia = a)
  }
  
  c <- i[1,]  # Caputs
  c <- c %>%
    mutate(tipo = "caput",
           paragrafo_referencia = NA)
  p <- i[-1,] # Parágrafos
  p <- p %>%
    mutate(tipo = 'paragrafo')
  
  caput <- bind_rows(caput, c)
  par <- bind_rows(par, p)
}

# Retirando o preâmbulo e outras coisas que vêm antes do conteúdo:
caput <- caput[-1,] 

# juntando dfs retirando quebras de linha e whitespaces duplicados e limpando início do conteúdo da lei
lei_df <- bind_rows(par , caput) %>%
  mutate(conteudo = gsub("\n ", " ", conteudo),
         conteudo = str_trim(conteudo, side = c("both")),
         conteudo = gsub("^[^a-zA-Z]+", " ", conteudo)) 

# Verificando o df
View(lei_df)



                       