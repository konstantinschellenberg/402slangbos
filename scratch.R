n = names(covv_all)

n = substr(n, 10, length(n))

sub = gsub(".", "-", n, fixed = T)
sub = paste(sub, collapse = ", ")

write.csv(sub, file = paste0(path_coherence, "comma_lineated_bandnames.txt"))

