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
# que contem os livros e terminam com .txt
meus.arquivos <- dir(input.dir,"\\.txt$")

# Cria uma função chamada corpus.list
# essa funcao recebe duas variaveis
# 1- Lista com os nomes dos arquivos
# 2- Diretório que contém tais arquivos
corpus.list <- function(my.files, input.dir) {
  
  # Cria uma variavel chamada corpus.list com uma lista vazia
  corpus.list <- list() 
  
  # Para cada arquivo na lista passada como argumento 1
  for(i in 1:length(my.files)){
    
    # text.v é uma lista contendo do arquivo 
    text.v <- scan(paste(input.dir, my.files[i], sep="/"),
                   what="character", sep="\n"
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
    # (ex se o arquivo lido foi "memorias_postumas_bras_cubas.txt")
    # então corpus.list[memorias_postumas_bras_cubas.txt] contem
    # a lista de palavras desse livro
    corpus.list[[my.files[i]]] <- text.words.v
  }
  
  # Depois que todos os livros foram lidos e as listas deles foi colocada
  # em corpus.list, retorna essa variavel
  return(corpus.list)
}

# Coloca o resultado da função definida acima em meu.corpus.list
# ou seja, meu.corpus.list contem a corpus.list dos arquivos em 
# phr
meu.corpus.list <- corpus.list(meus.arquivos, input.dir)

# Isso aqui não sei o que faz
e.positions.sense <- which(meu.corpus.list[[1]][] == "pedagogia")
head(e.positions.sense, 15)
