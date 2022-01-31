library(tidytext)
library(dplyr)
library(readr)
library(tidyverse)
library(tm)
library(tidyr)
library(scales)

# Diretório que contém os livros
input.dir <- "../livros/"

# Meus arquivos é uma lista de todos os arquivos dentro da pasta
# que contem os livros  em.txt
meus.arquivos <- dir(input.dir,"\\.txt$")

# Cria uma função chamada corpus.list
# essa funcao recebe duas variáveis
# 1- Lista com os nomes dos arquivos
# 2- Diretório que contém tais arquivos
corpus.list <- function(my.files, input.dir) {
  
  # Cria uma variável chamada corpus.list com uma lista vazia
  corpus.list <- list() 
  
  # Para cada arquivo na lista passada como argumento 1
  for(i in 1:length(my.files)){
    
    # text.v é uma lista contendo do arquivo 
    text.v <- scan(paste(input.dir, my.files[i], sep="/"),
                   what="character", sep="\n", fileEncoding = "UTF-8"
    ) 
    
    # text.v agora é essa lista juntada numa unica string
    text.v <- paste(text.v, collapse=" ")
    
    # text.v agora é essa string só que em minusculo
    text.lower.v <- tolower(text.v)
    
    # text.words.v é uma lista de palavras de text.v
    text.words.v <- strsplit(text.lower.v, "\\W")
    
    # text.words.v agora é um vetor de palavras
    text.words.v <- unlist(text.words.v)
    
    # elimina palavras vazia (iguais a "") de text.words.v
    text.words.v <- text.words.v[which(text.words.v != "")]
    
    # Assimila a lista de palavras do arquivo que foi lido ao corpus list
    # (ex se o arquivo lido foi "saberes.txt")
    # então corpus.list[saberes.txt] contém
    # a lista de palavras desse livro
    corpus.list[[my.files[i]]] <- text.words.v
  }
  
  # Depois que todos os livros foi lidos e as listas foi colocadas
  # em corpus.list, retorna essa variavel
  return(corpus.list)
}

# Coloca o resultado da função definida acima em meu.corpus.list
# ou seja, meu.corpus.list contem a corpus.list dos arquivos em 
# phr
meu.corpus.list <- corpus.list(meus.arquivos, input.dir)

KIWC.2 <- function(meu_corpus, nome_livro, keyword, qtd_palavras_incluir_a_esquerda, qtd_palavras_incluir_a_direita){
  # Procura as ocorrencias
  ocorrencias <- which(meu_corpus[[nome_livro]] == keyword)
  
  result <- NULL
  
  if(length(ocorrencias) > 0) {
    for (h in 1:length(ocorrencias)) {
      start <- ocorrencias[h] - qtd_palavras_incluir_a_esquerda
      
      if(start < 1){
        start <- 1
      } 
      
      end <- ocorrencias[h]+qtd_palavras_incluir_a_direita 
      
      cat(meu_corpus[[nome_livro]][start:end], "\n\n")
      
      myrow <- cbind(
                    ocorrencias[h],
                    #start
                    paste(meu_corpus[[nome_livro]][start:(ocorrencias[h]-1)], collapse=" "),
                    #hit
                    paste(meu_corpus[[nome_livro]][ocorrencias[h]], collapse=" "),
                    paste(meu_corpus[[nome_livro]][(ocorrencias[h]+1):end],collapse=" "),
                    nome_livro
              ) #end
      
      result <- rbind(result, myrow)
    }
    
    colnames(result) <- c("position", "left", "keyword", "right", "autor")
    return(result)
  } 
  
  else {
    cat("KEYWORD NAO ENCONTRADA \n")
    return(NULL)
  }
}

saberes_voce = KIWC.2(meu.corpus.list, "saberes.txt", "livro", 10, 10)

kwic(toks, pattern = phrase("foi"), window = 2)

saberes_foi = KIWC.2(meu.corpus.list, "saberes.txt", "foi", 10, 10)

write.csv(saberes_foi, "test.csv")

# teste nova alteracao 2
