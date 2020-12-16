cursos <- dados %>% 
  select(Qual.a.sua.principal.graduação.cursada..Caso.tenha.mais.de.uma..indique.a.mais.importante.para.a.sua.carreira.docente.) %>% 
  filter(Qual.a.sua.principal.graduação.cursada..Caso.tenha.mais.de.uma..indique.a.mais.importante.para.a.sua.carreira.docente. != '' & Qual.a.sua.principal.graduação.cursada..Caso.tenha.mais.de.uma..indique.a.mais.importante.para.a.sua.carreira.docente. != 'Outro.') %>% 
  rename('graduacao' = 'Qual.a.sua.principal.graduação.cursada..Caso.tenha.mais.de.uma..indique.a.mais.importante.para.a.sua.carreira.docente.')

df_cursos <- tibble(cursos = cursos$graduacao %>% unique())

write.xlsx(df_cursos, 'cursos.xlsx', row.names = F, col.names = TRUE)
