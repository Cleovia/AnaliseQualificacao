

#frequencia <- bind_rows(mutate(wordfrequency_jean, livro = "P"),
#                                 mutate(wordfrequency_saberes, livro = "H")) %>%
#  mutate(word = str_extract(word, "[a-z']+")) %>%
#  count(livro, word) %>%
#  group_by(livro) %>%
#  mutate(proportion = (n / sum(n)) * 100) %>%
#  select(-n) %>%
#  spread(livro, proportion)