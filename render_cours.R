# Cours render à la main pour éviter que la compilation se fasse à chaque fois
source("_fun_render.R")
create_cmd("Cours")

enable_render()
disable_render()

quarto::quarto_render('Cours/0_Accueil.qmd')
file.copy('_site/Cours/0_Accueil.pdf', 'Cours/0_Accueil.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/01_introduction.qmd')
file.copy('_site/Cours/01_introduction.pdf', 'Cours/01_introduction.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/02_R_et_JD.qmd')
file.copy('_site/Cours/02_R_et_JD.pdf', 'Cours/02_R_et_JD.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/03_Exploration_series.qmd')
file.copy('_site/Cours/03_Exploration_series.pdf', 'Cours/03_Exploration_series.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/04_Methode_X13-ARIMA.qmd')
file.copy('_site/Cours/04_Methode_X13-ARIMA.pdf', 'Cours/04_Methode_X13-ARIMA.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/05_CJO.qmd')
file.copy('_site/Cours/05_CJO.pdf', 'Cours/05_CJO.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/06_Le_modele_Reg-ARIMA.qmd')
file.copy('_site/Cours/06_Le_modele_Reg-ARIMA.pdf', 'Cours/06_Le_modele_Reg-ARIMA.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/07_Pb_estimation_regarima.qmd')
file.copy('_site/Cours/07_Pb_estimation_regarima.pdf', 'Cours/07_Pb_estimation_regarima.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/08_Les_Moyennes_Mobiles.qmd')
file.copy('_site/Cours/08_Les_Moyennes_Mobiles.pdf', 'Cours/08_Les_Moyennes_Mobiles.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/09_temps_production.qmd')
file.copy('_site/Cours/09_temps_production.pdf', 'Cours/09_temps_production.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/10_cvs_hf.qmd')
file.copy('_site/Cours/10_cvs_hf.pdf', 'Cours/10_cvs_hf.pdf', overwrite = TRUE)

yml <- remove_handout(readLines("Cours/_metadata.yml"))
writeLines(yml, "Cours/_metadata.yml")
quarto::quarto_render('Cours/0_Accueil.qmd')
file.copy('_site/Cours/0_Accueil.pdf', 'Cours/pres/0_Accueil.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/01_introduction.qmd')
file.copy('_site/Cours/01_introduction.pdf', 'Cours/pres/01_introduction.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/02_R_et_JD.qmd')
file.copy('_site/Cours/02_R_et_JD.pdf', 'Cours/pres/02_R_et_JD.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/03_Exploration_series.qmd')
file.copy('_site/Cours/03_Exploration_series.pdf', 'Cours/pres/03_Exploration_series.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/04_Methode_X13-ARIMA.qmd')
file.copy('_site/Cours/04_Methode_X13-ARIMA.pdf', 'Cours/pres/04_Methode_X13-ARIMA.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/05_CJO.qmd')
file.copy('_site/Cours/05_CJO.pdf', 'Cours/pres/05_CJO.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/06_Le_modele_Reg-ARIMA.qmd')
file.copy('_site/Cours/06_Le_modele_Reg-ARIMA.pdf', 'Cours/pres/06_Le_modele_Reg-ARIMA.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/07_Pb_estimation_regarima.qmd')
file.copy('_site/Cours/07_Pb_estimation_regarima.pdf', 'Cours/pres/07_Pb_estimation_regarima.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/08_Les_Moyennes_Mobiles.qmd')
file.copy('_site/Cours/08_Les_Moyennes_Mobiles.pdf', 'Cours/pres/08_Les_Moyennes_Mobiles.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/09_temps_production.qmd')
file.copy('_site/Cours/09_temps_production.pdf', 'Cours/pres/09_temps_production.pdf', overwrite = TRUE)
quarto::quarto_render('Cours/10_cvs_hf.qmd')
file.copy('_site/Cours/10_cvs_hf.pdf', 'Cours/pres/10_cvs_hf.pdf', overwrite = TRUE)
writeLines(add_handout(yml), "Cours/_metadata.yml")

unlink("_site/Cours", recursive = TRUE)
