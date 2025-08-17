library(matlib)
library(data.table)

setwd(file.path(
  "~", "Dropbox-HarvardUniversity", "Martin Bernstein",
  "Fall 2025", "Other", "LCC"
))


# Load IO data
io <- fread(file.path("data", "constructed data", "IO-matrices", "IO_matrices_long.csv"))


nna <- fread(file.path("data", "codes and crosswalks",
                       "NNA_codes_crosswalks.csv"))
nna <- unique(nna[, .(NNA_code, NNA_industry)])
# todrop <- nna[c(19,48:52), NNA_code] #exclude these sectors

# calculate max eigenval for each year
for(y in sort(unique(io$year))){
  dt <- io[year == y]
  
  #Format
  # dt <- io[year == y & !code_industry %in% todrop & !code_commodity %in% todrop ]
  dt <- dt[code_commodity != 99,
           .(code_industry, code_commodity, share_in_industry_output)]
  dt <- pivot_wider(dt, names_from = "code_industry",
                    values_from = "share_in_industry_output")%>%
    setDT()
  commods <- dt$code_commodity
  dt[, code_commodity := NULL]
  inds <- names(dt)
  mat <- as.matrix(dt)
  if(sum(!commods == inds) > 0){
    print("WARNING: matrix rows and columns don't agree")
  }
  
  
  # Leontief inverse
  L <- solve(diag(nrow(mat)) - mat)
  L <- setDT(as.data.frame(L))
  names(L) <- commods
  L[, code_industry := inds]
  L <- pivot_longer(L, cols = all_of(inds), names_to = "code_commodity",
                    values_to = "value")%>%
    setDT()
  L[, year := y]

  # Eigenvalues, to identify strong linkages
  ev <- eigen(mat)
  vals <- ev$values
  vecs <- ev$vectors
  # Index of dominant eigenvalue (one with largest modulus)
  idx <- which(Mod(vals) == max(Mod(vals)))
  # Eigenvector corresponding to that eigenvalue
  v <- as.numeric(vecs[, idx])
  # Each NNA code's entry in that eigenvector
  eigv <- setDT(data.frame(ev = v, NNA_code = inds))
  # Assign industries their entry in the eigenvector of dominant eigenvalue
  thisnna <- copy(nna)
  thisnna[, eigenvec := eigv[.SD, on = .(NNA_code), x.ev]]
  thisnna[, year := y]
  # This is a measure of the strength of that industry's linkages.
  
  # Assemble data
  if(y == min(sort(unique(io$year)))){
    alld <- thisnna
    allL <- L
  }else{
    alld <- rbind(alld, thisnna)
    allL <- rbind(allL, L)
  }
}


# Plot and save
alld[, NNA_industry := factor(NNA_industry, levels = nna$NNA_industry)]
p <- ggplot(alld[!is.na(eigenvec)], aes(year, abs(eigenvec)))+
  geom_line()+
  facet_wrap(~NNA_industry)+
  theme_bw()+
  labs(x = NULL, y = "Eigenvector")
p
ggsave(p, file = file.path("figures", "exploration", "IO-matrices", "industry_eigenvectors.png"),
       width = 15, height = 15, units = "in")

alld[, abs_eigenvec := abs(eigenvec)]
alld <- alld[!is.na(eigenvec), .(year, NNA_code, NNA_industry, abs_eigenvec)]
setorder(alld, year, NNA_code)
write.csv(alld, file = file.path("data", "constructed data", "IO-matrices",
                                 "industry_eigenvalues.csv"),
          row.names = FALSE)


allL[, industry := nna[.SD, on = .(NNA_code = code_industry), x.NNA_industry]]
allL[, commodity := nna[.SD, on = .(NNA_code = code_commodity), x.NNA_industry]]
allL <- allL[, .(year, code_industry, industry,
                 code_commodity, commodity, value)]
write.csv(allL, file = file.path("data", "constructed data", "IO-matrices",
                                 "leontief_inverse_long.csv"),
          row.names = FALSE)