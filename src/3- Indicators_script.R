### SCRIPT TO EXTRACT AND SAVE 4 FILES, ONE FOR EACH INDICATOR BEFORE THE OVERALL MERGING OF OUR INDICATORS

# Using objct from the previous script (2-Sp_dataframe_to_Indicator.R)
indicateur_tree_cover_iris = data_aggregated_iris
indicateur_tree_cover_rest = data_aggregated_dep_reg

# Indicateur pour Iris
indicateur_tree_cover_iris <- indicateur_tree_cover_iris%>%
  summarize(code_iris, nom_iris, tree_cover_mean)%>%
  mutate(scale = 'iris')

# Indicateur pour communes
indicateur_tree_cover_communes <- indicateur_tree_cover_rest%>%
  group_by(insee_communes)%>%
  summarize(nom_commune, tree_cover_mean = mean(tree_cover_mean))%>%
  mutate(scale = 'commune')

# Indicateur pour departments
indicateur_tree_cover_depart <- indicateur_tree_cover_rest%>%
  group_by(insee_departement)%>%
  summarize(nom_departement, tree_cover_mean = round(mean(tree_cover_mean)))%>%
  mutate(scale = 'departement')
# Remove all the implied duplicated to keep one row per department
indicateur_tree_cover_depart = indicateur_tree_cover_depart[!duplicated(indicateur_tree_cover_depart),]
indicateur_tree_cover_depart = na.omit(indicateur_tree_cover_depart)

# Indicateur pour régions
indicateur_tree_cover_reg = indicateur_tree_cover_rest%>%
  group_by(insee_region)%>%
  summarize(nom_region, tree_cover_mean = round(mean(tree_cover_mean)))%>%
  mutate(scale = 'region')
# Remove all the implied duplicated to keep one row per region
indicateur_tree_cover_reg = indicateur_tree_cover_reg[!duplicated(indicateur_tree_cover_reg),]
indicateur_tree_cover_reg = na.omit(indicateur_tree_cover_reg)

########################################################################################################################################################################################################################

# Harmoniser le nom des colonnes
indicateur_tree_cover_iris <- indicateur_tree_cover_iris%>%
  summarize(code=code_iris, name=nom_iris, value=tree_cover_mean, scale)

indicateur_tree_cover_communes <- indicateur_tree_cover_communes%>%
  summarize(code=insee_communes, name=nom_commune, value=tree_cover_mean, scale)

indicateur_tree_cover_depart <- indicateur_tree_cover_depart%>%
  summarize(code=insee_departement, name=nom_departement, value=tree_cover_mean, scale)%>%
  select(-insee_departement)

indicateur_tree_cover_reg <- indicateur_tree_cover_reg%>%
  summarize(code=insee_region, name=nom_region, value=tree_cover_mean, scale)%>%
  select(-insee_region)

# Joindre les différentes echelles
tree_cover_indic <- rbind(indicateur_tree_cover_iris, indicateur_tree_cover_communes, indicateur_tree_cover_depart, indicateur_tree_cover_reg)%>%
  mutate(indicateur ="Taux de couverture forestière", determinant = 'Déterminant n°5 : Température')

# Enregistrer fichier
saveRDS(tree_cover_indic, here("Indicators/Tree Cover/Indicateurs/treecover_indicateur.rds"))
