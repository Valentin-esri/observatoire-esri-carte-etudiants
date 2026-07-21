# =============================================================================
#  Observatoire ESRI Normandie — Carte interactive des effectifs étudiants
#  ÉTAPE 2/2 : application Shiny
# -----------------------------------------------------------------------------
#  Lit les 3 fichiers produits par 01_preparation_donnees.R (dossier data_app/).
#  Aucune opération SIG au démarrage -> rapide en local, sur Shiny Server, et
#  compatible shinylive (WebAssembly) pour un partage sans serveur.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(sf)
  library(leaflet)
  library(leaflet.extras)
  library(htmltools)
  library(htmlwidgets)
  library(DT)
  library(ggplot2)
  library(scales)
  library(officer)
})

# ---- Thème et palettes des graphiques ---------------------------------------
theme_obs <- theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", colour = "#C7102C", size = 17),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 14),
        legend.position = "top")
col_sexe    <- c("Femmes" = "#C7102C", "Hommes" = "#003F7D")
col_evol    <- c("Total" = "#333333", "Femmes" = "#C7102C", "Hommes" = "#003F7D")

# NB : format() aligne les nombres sur une largeur commune en ajoutant des
# espaces à gauche (200 devient " 200" à côté de 5693). trimws() les supprime.
fmt_eff <- function(x) trimws(format(round(x), big.mark = " ", scientific = FALSE))

# Retour à la ligne des libellés longs. Les noms de composantes reconstruits
# (type + nom propre) sont nettement plus longs que les anciens libellés bruts :
# sans césure, ils dévorent la zone de tracé des graphiques en barres.
# Largeur de repli des étiquettes de l'axe des NIVEAUX, en caractères. Cette
# valeur n'est pas arbitraire : mesurée sur la fiche de l'Université de Rouen
# exportée en 6,6 pouces de large, la colonne des libellés non repliés occupait
# 2,85 pouces et les valeurs en bout de barre étaient coupées ; repliée à 24
# caractères elle tombe à 1,76 pouce. Le repli des noms de COMPOSANTES, lui,
# est calculé au cas par cas (voir reglage_comp) : ils sont bien plus longs et
# leur nombre varie de 1 à 25 selon l'établissement.
LARGEUR_ETIQ_NIVEAU <- 24

# Marge droite à réserver aux étiquettes de valeur, exprimée comme le veut
# expansion() : en fraction de l'étendue des données.
#   - une étiquette de n caractères mesure environ n x 0,067 pouce à la taille
#     employée (vérifié avec grid::stringWidth) ;
#   - si la marge vaut E fois l'étendue, elle occupe E/(1+E) du cadre ;
#   - il faut donc E/(1+E) x largeur_cadre >= largeur_texte, d'où la formule.
# La largeur du cadre est estimée par la largeur de l'image moins la colonne
# des noms. Le résultat est plafonné pour ne pas écraser les barres.
# Constantes calibrées par mesure directe (grid::stringWidth) sur la fiche de
# l'Université de Rouen : 0,067 pouce par caractère pour les valeurs (chiffres,
# espaces et parenthèses, étroits) et 0,085 pour les noms d'axe, qui sont en
# CAPITALES et donc sensiblement plus larges. Le terme constant absorbe les
# graduations, le trait d'axe et l'écart entre la barre et son étiquette.
marge_valeurs <- function(etiquettes, noms_axe, largeur_image, taille_axe = 10.5,
                          po_par_car = 0.067, po_par_car_axe = 0.085) {
  if (!length(etiquettes)) return(0.30)
  lignes  <- unlist(strsplit(as.character(noms_axe), "\n", fixed = TRUE))
  l_axe   <- min(0.52 * largeur_image,
                 max(c(0, nchar(lignes))) * po_par_car_axe * taille_axe / 10.5 + 0.30)
  l_cadre <- max(1.2, largeur_image - l_axe)
  besoin  <- max(nchar(as.character(etiquettes))) * po_par_car + 0.22
  r <- min(0.45, besoin / l_cadre)
  round(r / (1 - r), 3)
}

envelopper <- function(x, largeur = 46)
  vapply(as.character(x),
         function(s) paste(strwrap(s, width = largeur), collapse = "\n"),
         character(1), USE.NAMES = FALSE)

# ---- Échelles des graphiques ------------------------------------------------
# ANNÉES : uniquement des entiers. pretty_breaks() raisonne sur un axe continu
# et produisait des dixièmes d'année (2023.96, 2023.98, 2024.00...) dès que la
# période affichée se réduisait à une seule rentrée.
#   n = nombre maximal d'étiquettes ; la série est ancrée sur la dernière année
#   afin que la rentrée la plus récente soit toujours étiquetée.
breaks_annees <- function(n = 4) {
  function(lim) {
    a <- ceiling(min(lim) - 1e-6)
    b <- floor(max(lim) + 1e-6)
    if (!is.finite(a) || !is.finite(b) || b < a) return(round(mean(lim)))
    ans <- a:b
    if (length(ans) <= n) return(ans)
    rev(seq(b, a, by = -ceiling(length(ans) / n)))
  }
}

# EFFECTIFS : entiers également. Un nombre d'étudiants n'a pas de décimale, et
# une série réduite à une seule valeur affichait cinq fois la même étiquette
# (69, 69, 69...) parce que les graduations tombaient à 68,9 / 69,0 / 69,1.
breaks_effectifs <- function(n = 5) {
  function(lim) {
    b <- unique(round(scales::breaks_pretty(n)(lim)))
    b[is.finite(b) & b >= 0]
  }
}

# Message affiché lorsqu'une combinaison de filtres ne renvoie aucune donnée
# (ex. : secteur « Privé » + type « Universités » : inexistant en Normandie).
MSG_VIDE <- paste("Aucune donnée pour cette combinaison de filtres",
                  "(rentrée, secteur, type d'établissement).",
                  "Élargissez la sélection.")

# Répète l'axe des abscisses sous CHAQUE vignette (facette), pour éviter d'avoir
# à faire défiler jusqu'en bas. Utilise l'option `axes` de ggplot2 >= 3.5 ;
# sinon, repli sur des échelles libres (qui affichent aussi les axes partout).
facettes <- function(...) {
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    facet_wrap(..., scales = "free_y", axes = "all_x")
  } else {
    facet_wrap(..., scales = "free")
  }
}

# Étiquettes de valeurs sur les courbes, SANS chevauchement.
#  - si ggrepel est installé : les étiquettes se repoussent automatiquement ;
#  - sinon : repli sans dépendance — l'étiquette de la série la plus haute de
#    l'année est placée au-dessus du point, les autres en dessous.
#  Le jeu de données doit contenir une colonne `rang_annee` (1 = série la plus
#  haute de l'année), ajoutée par `ajouter_rang()`.
ajouter_rang <- function(d) {
  d %>% group_by(rentree) %>%
    mutate(rang_annee = rank(-eff, ties.method = "first")) %>%
    ungroup()
}

# Graphique de repli : affiche un message au lieu de planter un export lorsque
# les filtres ne renvoient aucune donnée.
graphique_message <- function(txt) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = txt, size = 5.5,
             colour = "#C7102C", hjust = 0.5, vjust = 0.5) +
    theme_void()
}

couche_etiquettes <- function(taille = 4.4) {
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    # Pastille BLANCHE OPAQUE (et non du texte simple) : la courbe ne traverse
    # plus l'étiquette. La répulsion évite en outre les collisions entre elles.
    ggrepel::geom_label_repel(
      aes(label = fmt_eff(eff)), size = taille, fontface = "bold",
      show.legend = FALSE,
      fill = "white", label.size = 0, label.padding = unit(0.14, "lines"),
      seed = 42, max.overlaps = Inf, box.padding = 0.45, point.padding = 0.35,
      min.segment.length = 0.25, segment.colour = "grey60", segment.size = 0.3)
  } else {
    # Repli sans ggrepel : décalage vertical ÉCHELONNÉ selon le rang de la série
    # dans l'année (1 = la plus haute). Sans échelonnement, les séries de rang 2
    # et 3 étaient placées au même niveau et se chevauchaient dès que les
    # courbes étaient proches (cas Femmes / Hommes).
    decalages <- c(-1.0, 1.9, 3.2, 4.5)
    geom_label(aes(label = fmt_eff(eff),
                   vjust = decalages[pmin(rang_annee, length(decalages))]),
               size = taille, fontface = "bold", fill = "white", label.size = 0,
               label.padding = unit(0.14, "lines"), show.legend = FALSE)
  }
}

# ---- Données préparées -------------------------------------------------------
rep_data <- "data_app"
data_all <- readRDS(file.path(rep_data, "donnees_carte.rds"))
geo_cantons <- readRDS(file.path(rep_data, "geo_cantons.rds")) %>%
  rename(INSEE_CAN_CODE = code_can)
geo_arr     <- readRDS(file.path(rep_data, "geo_arr.rds")) %>%
  rename(INSEE_ARR_CODE = code_arr)

# --- Couleurs des secteurs : SOURCE UNIQUE ------------------------------------
# La palette est celle des cercles de la carte (colorFactor "Set1"), avec un
# domaine FIXE. Deux effets :
#  1. les graphiques (barres, courbes) reprennent exactement les couleurs de la
#     carte, donc la lecture est cohérente d'un onglet à l'autre ;
#  2. la couleur d'un secteur ne change plus selon les filtres (auparavant, en
#     filtrant sur « Public » seul, le domaine se réduisait et la couleur
#     basculait).
secteurs_ref <- sort(unique(data_all$secteur))
pal_secteur  <- leaflet::colorFactor(palette = "Set1", domain = secteurs_ref)
col_secteur  <- setNames(pal_secteur(secteurs_ref), secteurs_ref)
col_evol_secteur <- c("Total" = "#333333", col_secteur)

# Détecte automatiquement le logo dans www/ (tolérant au nom / à l'extension exacts)
logo_src <- local({
  if (!dir.exists("www")) return(NA_character_)
  imgs <- list.files("www", pattern = "(?i)\\.(jpe?g|png|svg)$")
  if (length(imgs) == 0) return(NA_character_)
  pref <- imgs[grepl("logo|normandie", imgs, ignore.case = TRUE)]
  if (length(pref)) pref[1] else imgs[1]
})

# Mentions institutionnelles et sources, définies UNE SEULE FOIS : le bandeau
# gris de l'application et l'en-tête des documents Word y puisent tous les deux.
# SOURCES_TXT est la version en texte brut du bloc « Sources » du bandeau, qui
# comporte en plus des liens hypertexte ; les deux doivent rester cohérents.
MENTION_DEESTRI <- paste0(
  "Observatoire ESRI — Service Enseignement Supérieur, Recherche et Innovation",
  " — DEESTRI (Économie, Enseignement Supérieur, Tourisme, Recherche et ",
  "Innovation)")
SOURCES_TXT <- paste0(
  "Sources : Effectifs étudiants — Ministère de l'Enseignement supérieur, de la ",
  "Recherche et de l'Espace (MESRE), « Effectifs d'étudiants inscrits dans les ",
  "établissements et les formations de l'enseignement supérieur — détail par ",
  "établissements » (open data MESRE). Limites administratives — IGN, ",
  "ADMIN-EXPRESS-COG-CARTO. Codes géographiques — INSEE, Code officiel ",
  "géographique (COG).")

# Dimensions d'une image PNG ou JPEG, lues directement dans le fichier.
# Évite de déformer le logo dans les exports sans ajouter de dépendance : aucun
# paquet de lecture d'image n'est installable derrière le proxy de la Région.
# Renvoie NULL si le format n'est pas reconnu — l'appelant retombe alors sur un
# ratio par défaut plutôt que d'échouer.
dimensions_image <- function(chemin) {
  if (!file.exists(chemin)) return(NULL)
  con <- file(chemin, "rb"); on.exit(close(con), add = TRUE)
  sig <- readBin(con, "raw", 2)
  if (length(sig) < 2) return(NULL)

  # --- PNG : le bloc IHDR est toujours le premier, largeur puis hauteur -----
  if (identical(as.integer(sig), c(137L, 80L))) {
    readBin(con, "raw", 14)          # fin de signature (6) + taille/type IHDR (8)
    d <- readBin(con, "integer", 2, size = 4, endian = "big")
    if (length(d) < 2 || any(d <= 0)) return(NULL)
    return(list(l = d[1], h = d[2]))
  }

  # --- JPEG : parcours des marqueurs jusqu'au segment SOF, qui porte la
  #     taille de l'image (hauteur PUIS largeur, contrairement au PNG).
  #     Les segments non pertinents (EXIF, vignette, tables de quantification)
  #     sont sautés d'un bloc grâce à leur longueur déclarée.
  if (!identical(as.integer(sig), c(255L, 216L))) return(NULL)
  repeat {
    b <- readBin(con, "raw", 1)
    if (length(b) == 0) return(NULL)                 # fin de fichier
    if (as.integer(b) != 255L) next                  # resynchronisation
    m <- readBin(con, "raw", 1)
    if (length(m) == 0) return(NULL)
    mi <- as.integer(m)
    if (mi %in% c(0L, 255L)) next                    # bourrage / octet échappé
    if (mi >= 208L && mi <= 217L) next               # marqueurs sans charge utile
    lng <- readBin(con, "integer", 1, size = 2, endian = "big", signed = FALSE)
    if (length(lng) == 0 || lng < 2) return(NULL)
    # SOF0 à SOF15, en excluant DHT (C4), JPG (C8) et DAC (CC)
    if (mi >= 192L && mi <= 207L && !(mi %in% c(196L, 200L, 204L))) {
      readBin(con, "raw", 1)                         # précision en bits
      d <- readBin(con, "integer", 2, size = 2, endian = "big", signed = FALSE)
      if (length(d) < 2 || any(d <= 0)) return(NULL)
      return(list(l = d[2], h = d[1]))
    }
    readBin(con, "raw", lng - 2)                     # segment sans intérêt
  }
}

# NIVEAU D'ÉTUDES NON RENSEIGNÉ.
# La source MESR ne renseigne pas le degré d'études avant la rentrée 2021 :
# la colonne est vide pour 100 % des lignes de 2015 à 2020, soit 58 % des
# effectifs cumulés. Ces effectifs sont EXACTS et restent comptés partout ;
# seule leur ventilation par niveau est inconnue. On leur donne donc un libellé
# explicite plutôt que de laisser remonter des « NA » dans les graphiques, les
# tableaux, les popups et les exports.
NIVEAU_INCONNU <- "Niveau non renseigné (source MESR)"

# Ordre standardisé des niveaux d'études (pour les popups / tableaux)
niveaux_ordre <- c(
  "Inférieur ou égal au baccalauréat",
  "BAC + 1", "BAC + 2", "BAC + 3", "BAC + 4", "BAC + 5", "BAC + 6 et plus",
  NIVEAU_INCONNU
)

# Application du libellé, une fois pour toutes, en amont de toute agrégation :
# aucune vue n'a ainsi à se préoccuper du cas.
data_all <- data_all %>%
  mutate(degre = ifelse(is.na(degre) | !nzchar(trimws(degre)),
                        NIVEAU_INCONNU, degre))

# Cercles colorés par secteur avec la palette "Set1" (identique à la carte v21).

# ---- Agrégation au point (établissement × composante × niveau) ---------------
# CLÉ D'IDENTITÉ : uai_etab / uai_comp, avec les noms stabilisés nom_etab et
# nom_comp construits par 01_preparation_donnees.R (bijection nom <-> UAI
# garantie par ce script). Les anciens libellés bruts lib_etab / lib_comp ne
# sont plus utilisés comme clés : ils étaient partagés par plusieurs UAI (par
# ex. 41 lycées sous « LYCEE GENERAL ET TECHNOLOGIQUE ») et variaient d'une
# rentrée à l'autre pour un même établissement.
etab_aggreg <- data_all %>%
  group_by(rentree, uai_etab, nom_etab, etab_affiche,
           uai_comp, nom_comp, comp_affiche, categorie,
           latitude, longitude, degre, commune, secteur, geo_source, unite_urbaine,
           INSEE_CAN_CODE, INSEE_ARR_CODE, Canton_nom, Arrondissement_nom) %>%
  summarise(effectifs = sum(effectifs, na.rm = TRUE),
            femmes    = sum(femmes,    na.rm = TRUE),
            hommes    = sum(hommes,    na.rm = TRUE),
            .groups = "drop")

# ORDRE d'affichage des catégories (de la plus importante à la plus petite),
# établi sur l'ensemble des données. AUCUNE catégorie n'est masquée : cet ordre
# ne sert qu'à présenter les vignettes de façon stable d'un filtre à l'autre.
ordre_categories <- data_all %>%
  group_by(categorie) %>%
  summarise(t = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(t)) %>%
  pull(categorie)

# Liste des établissements, du plus grand au plus petit. Le libellé affiché
# (« nom — commune ») est en correspondance stricte avec un UAI : la liste
# compte donc un item par établissement réel, sans aucune troncature.
liste_etablissements <- data_all %>%
  group_by(etab_affiche) %>%
  summarise(t = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(t)) %>%
  pull(etab_affiche)

# Liste AFFICHÉE dans les sélecteurs : ordre alphabétique (plus facile à
# parcourir). L'ordre par effectifs ci-dessus reste utilisé pour proposer une
# sélection par défaut pertinente (le plus grand établissement).
liste_etab_alpha <- sort(liste_etablissements)

# Bornes chronologiques disponibles (curseurs de période)
annees_dispo <- sort(unique(data_all$rentree))
an_min <- min(annees_dispo); an_max <- max(annees_dispo)

# --- Fond de carte statique (export Word) : préparé une fois au démarrage -----
# Contours départementaux reconstitués en fusionnant les arrondissements
# (les 2 premiers caractères du code arrondissement donnent le département).
fond_dep <- tryCatch({
  geo_arr %>%
    mutate(dep = substr(as.character(INSEE_ARR_CODE), 1, 2)) %>%
    group_by(dep) %>%
    summarise(.groups = "drop") %>%
    sf::st_make_valid()
}, error = function(e) NULL)

# Cadre fixe : toute la Normandie (avec une marge), quel que soit l'établissement
bb_norm <- sf::st_bbox(geo_arr)
marge_x <- (bb_norm["xmax"] - bb_norm["xmin"]) * 0.03
marge_y <- (bb_norm["ymax"] - bb_norm["ymin"]) * 0.05
cadre_x <- c(bb_norm["xmin"] - marge_x, bb_norm["xmax"] + marge_x)
cadre_y <- c(bb_norm["ymin"] - marge_y, bb_norm["ymax"] + marge_y)

# Palette des niveaux d'études (du plus bas au plus élevé)
col_niveaux <- c(
  "Inférieur ou égal au baccalauréat" = "#9E9E9E",
  "BAC + 1" = "#A6CEE3", "BAC + 2" = "#1F78B4", "BAC + 3" = "#33A02C",
  "BAC + 4" = "#FF7F00", "BAC + 5" = "#C7102C", "BAC + 6 et plus" = "#6A3D9A"
)
# Noir : la catégorie n'est pas un niveau d'études et ne doit se confondre avec
# aucun d'entre eux. Le gris précédent était trop proche du gris clair de
# « Inférieur ou égal au baccalauréat » (#9E9E9E) — les deux courbes étaient
# indiscernables. Le noir s'en distingue par la luminosité, et de tous les
# autres niveaux par la teinte.
col_niveaux[NIVEAU_INCONNU] <- "#1A1A1A"

# =============================================================================
#  Fonctions utilitaires
# =============================================================================

ordonner_niveaux <- function(df) {
  df %>% arrange(factor(degre, levels = niveaux_ordre))
}

# Mention ajoutée au sous-titre des graphiques par niveau lorsqu'ils affichent
# effectivement des effectifs sans niveau renseigné.
mention_niveau <- function(v) {
  if (!any(as.character(v) == NIVEAU_INCONNU, na.rm = TRUE)) return("")
  " — niveau non renseigné par la source avant la rentrée 2021"
}

# Style COMMUN à toutes les légendes déposées sur les cartes (même police,
# même taille, même fond) : évite les écarts de rendu d'une légende à l'autre.
STYLE_LEGENDE <- paste0(
  "background:white;padding:8px 10px;border-radius:5px;",
  "font-family:'Avenir Next','Avenir','Segoe UI',Arial,sans-serif;",
  "font-size:12px;line-height:1.4;color:#222222;")
STYLE_LEGENDE_TITRE <- paste0(
  "display:block;font-weight:700;font-size:13px;margin-bottom:5px;color:#003F7D;")

legende_cercles <- function(effectifs) {
  max_eff <- max(effectifs, na.rm = TRUE)
  if (!is.finite(max_eff) || max_eff <= 0) return("")
  # Valeurs arrondies à la centaine (au dix ou à l'unité si les effectifs sont
  # trop faibles pour que l'arrondi à la centaine garde des paliers distincts).
  pas <- if (max_eff >= 1000) 100 else if (max_eff >= 100) 10 else 1
  tailles <- round(c(0.3, 0.6, 1) * max_eff / pas) * pas
  tailles <- sort(unique(tailles[tailles > 0]))
  if (length(tailles) == 0) return("")
  rayons <- 0.5 * sqrt(tailles)
  # Positions verticales cumulées : les cercles ne se chevauchent plus.
  y <- numeric(length(rayons)); haut <- 6
  for (i in seq_along(rayons)) {
    y[i] <- haut + rayons[i]
    haut <- haut + 2 * rayons[i] + 8
  }
  x_texte <- 2 * max(rayons) + 26
  lignes <- vapply(seq_along(rayons), function(i) {
    paste0("<circle cx='", max(rayons) + 8, "' cy='", y[i], "' r='", rayons[i],
           "' stroke='#333333' fill='none'/>",
           "<text x='", x_texte, "' y='", y[i] + 4,
           "' font-size='12' font-family=\"Avenir Next, Avenir, Segoe UI, Arial\"",
           " fill='#222222'>", fmt_eff(tailles[i]), " étudiants</text>")
  }, character(1))
  largeur <- x_texte + 110
  paste0("<div style='", STYLE_LEGENDE, "width:", largeur + 20, "px;'>",
         "<span style='", STYLE_LEGENDE_TITRE, "'>Effectifs</span>",
         "<svg width='", largeur, "' height='", haut + 4, "'>",
         paste(lignes, collapse = ""), "</svg></div>")
}

legende_heatmap <- function() {
  paste0("<div style='", STYLE_LEGENDE, "width:200px;'>",
         "<span style='", STYLE_LEGENDE_TITRE, "'>Carte de chaleur</span>",
         "Intensité = densité d'étudiants",
         "<div style='height:18px;margin-top:8px;border-radius:3px;",
         "background:linear-gradient(to right,blue,lime,yellow,red);'></div></div>")
}

# Bloc "détail par degré" formaté pour un popup
# Bloc "détail par degré" formaté pour un popup.
# Écrit de façon vectorisée : apply() convertirait le tableau en matrice de
# caractères et alignerait les nombres sur une largeur commune, ce qui insérait
# un espace parasite devant les petits effectifs (" 124 F" au lieu de "124 F").
bloc_details <- function(det) {
  if (is.null(det) || !is.data.frame(det) || nrow(det) == 0) return("")
  det <- ordonner_niveaux(det)
  z <- function(x) ifelse(is.na(x), 0, x)
  items <- paste0(det$degre, " : ", fmt_eff(z(det$effectif)),
                  " (", fmt_eff(z(det$femmes)), " F, ",
                  fmt_eff(z(det$hommes)), " H)")
  paste0("<br/><b>Détail par niveau :</b><br/>", paste(items, collapse = "<br/>"))
}

popup_etab <- function(etab, commune, secteur, categorie, eff, femmes, hommes, det, geo_source = NA) {
  approx <- if (!is.na(geo_source) && grepl("centro|approx", geo_source))
    "<br/><i>(localisation approximative : centre de la commune)</i>" else ""
  paste0("<b>", etab, "</b><br/>Commune : ", commune,
         "<br/>Secteur : ", secteur, "<br/>Catégorie : ", categorie,
         "<br/>Effectif total : ", fmt_eff(ifelse(is.na(eff), 0, eff)),
         "<br/>Femmes : ", fmt_eff(ifelse(is.na(femmes), 0, femmes)),
         "<br/>Hommes : ", fmt_eff(ifelse(is.na(hommes), 0, hommes)),
         approx, bloc_details(det))
}

# Construit l'objet leaflet complet (partagé par l'affichage et l'export HTML)
construire_carte <- function(pts, cantons_data, arr_data, opts, vue = NULL) {
  map <- leaflet()
  if (opts$base || opts$clusters) map <- addTiles(map)
  map <- addScaleBar(map, position = "bottomleft")

  if (opts$arr && !is.null(arr_data)) {
    pal <- colorNumeric("YlOrRd", arr_data$total_effectifs)
    popups <- vapply(seq_len(nrow(arr_data)), function(i) {
      paste0("<b>Arrondissement : </b>", arr_data$Arrondissement_nom[i],
             "<br/>Code : ", arr_data$INSEE_ARR_CODE[i],
             "<br/>Effectifs : ", fmt_eff(arr_data$total_effectifs[i]),
             bloc_details(arr_data$details[[i]]))
    }, character(1))
    map <- map %>%
      addPolygons(data = arr_data, fillColor = ~pal(total_effectifs),
                  color = "grey", weight = 1, fillOpacity = 0.6, popup = popups) %>%
      addLegend("bottomright", pal = pal, values = arr_data$total_effectifs,
                title = "Effectifs par arrondissement", opacity = 0.7)
  }

  if (opts$cantons && !is.null(cantons_data)) {
    pal <- colorNumeric("YlGnBu", cantons_data$total_effectifs)
    popups <- vapply(seq_len(nrow(cantons_data)), function(i) {
      paste0("<b>Canton : </b>", cantons_data$Canton_nom[i],
             "<br/>Code : ", cantons_data$INSEE_CAN_CODE[i],
             "<br/>Effectifs : ", fmt_eff(cantons_data$total_effectifs[i]),
             bloc_details(cantons_data$details[[i]]))
    }, character(1))
    map <- map %>%
      addPolygons(data = cantons_data, fillColor = ~pal(total_effectifs),
                  color = "darkgrey", weight = 1, fillOpacity = 0.7, popup = popups) %>%
      addLegend("bottomleft", pal = pal, values = cantons_data$total_effectifs,
                title = "Effectifs par canton", opacity = 0.7)
  }

  if (opts$circles && nrow(pts) > 0) {
    # Palette FIXE (définie une fois) : la couleur d'un secteur ne dépend plus
    # des données filtrées, et elle est identique à celle des graphiques.
    map <- map %>%
      addCircleMarkers(data = pts, lng = ~longitude, lat = ~latitude,
                       radius = ~0.5 * sqrt(effectifs),
                       color = ~pal_secteur(secteur), fillColor = ~pal_secteur(secteur),
                       stroke = TRUE, weight = 1.2, opacity = 0.9,
                       fillOpacity = 0.55, popup = ~popup_text) %>%
      addLegend("bottomright", pal = pal_secteur, values = secteurs_ref,
                title = "Secteur d'établissement") %>%
      addControl(HTML(legende_cercles(pts$effectifs)), position = "bottomright")
  }

  if (opts$heat && nrow(pts) > 0) {
    map <- map %>%
      addHeatmap(data = pts, lng = ~longitude, lat = ~latitude,
                 intensity = ~effectifs, blur = 20, max = 0.05, radius = 15) %>%
      addControl(HTML(legende_heatmap()), position = "topright")
  }

  if (opts$clusters && nrow(pts) > 0) {
    u <- distinct(pts, etablissement, .keep_all = TRUE)
    map <- addMarkers(map, data = u, lng = ~longitude, lat = ~latitude,
                      popup = ~popup_text, clusterOptions = markerClusterOptions())
  }

  # Restaure le cadrage courant (centre + zoom). Appliqué EN DERNIER pour
  # l'emporter sur l'ajustement automatique de leaflet aux données affichées :
  # changer un filtre (année, secteur…) ne ramène plus la carte à la vue
  # régionale, ce qui permet de suivre l'évolution des cercles d'une ville.
  if (!is.null(vue) && !is.null(vue$lng) && !is.null(vue$lat) && !is.null(vue$zoom))
    map <- setView(map, lng = vue$lng, lat = vue$lat, zoom = vue$zoom)
  map
}

# =============================================================================
#  UI
# =============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML(
    ".deestri-mention { font-family: 'Avenir Next','Avenir','Segoe UI',Arial,sans-serif;
       font-weight: 700; font-size: 12px; color: #003F7D; line-height: 1.35; margin-top: 6px; }
     .source-block { font-family: 'Avenir Next','Avenir','Segoe UI',Arial,sans-serif;
       font-size: 12px; color: #333; line-height: 1.4; }
     .source-block a { color: #003F7D; }
     .nav-tabs > li > a { color: #C7102C !important; font-weight: 700 !important; }
     .nav-tabs > li.active > a,
     .nav-tabs > li.active > a:hover,
     .nav-tabs > li.active > a:focus { color: #C7102C !important; font-weight: 700 !important; }
     .tab-content .nav-tabs > li > a { color: #003F7D !important; font-weight: 700 !important; }
     .tab-content .nav-tabs > li.active > a,
     .tab-content .nav-tabs > li.active > a:hover,
     .tab-content .nav-tabs > li.active > a:focus { color: #003F7D !important; font-weight: 700 !important; }
     /* Titre principal : blanc sur bandeau rouge charte */
     .bandeau-titre { background-color: #C7102C; color: #FFFFFF;
       padding: 12px 18px; margin: 0 0 10px 0; border-radius: 3px;
       font-family: 'Avenir Next','Avenir','Segoe UI',Arial,sans-serif;
       font-size: 26px; font-weight: 700; line-height: 1.25; }
     /* Bandeau gris : boutons pleine largeur, texte qui passe à la ligne
        (sinon les libellés longs débordaient du panneau étroit). */
     .bloc-exports .btn { display: block; width: 100%; white-space: normal;
       text-align: left; margin: 0 0 6px 0; padding: 6px 10px; font-size: 12px; }
     /* Contenu resserré et défilement propre à la colonne, pour que le logo
        reste atteignable sans faire défiler toute la page. */
     .well { max-height: calc(100vh - 120px); overflow-y: auto; padding: 12px; }
     .well .form-group { margin-bottom: 10px; }
     .well .checkbox { margin-top: 3px; margin-bottom: 3px; }
     .well hr { margin: 10px 0; }
     /* Légendes natives de leaflet (secteur, cantons, arrondissements) :
        même police et même taille que les légendes personnalisées. */
     .leaflet-control.info, .info.legend, .leaflet-control .legend {
       font-family: 'Avenir Next','Avenir','Segoe UI',Arial,sans-serif !important;
       font-size: 12px !important; line-height: 1.4 !important; color: #222222; }
     .info.legend strong, .info.legend .legend-title {
       font-size: 13px !important; font-weight: 700; color: #003F7D; }
     /* La carte occupe la hauteur disponible de la fenêtre, avec un plancher */
     #carte, #carte .leaflet-container { min-height: 420px; }
     /* Supprime la marge basse de la page pour éviter tout bandeau blanc */
     body { padding-bottom: 0 !important; margin-bottom: 0 !important; }
     .container-fluid { padding-bottom: 0 !important; }"))),
  titlePanel(tags$div(class = "bandeau-titre",
                      "Carte interactive des effectifs étudiants en Normandie")),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput("rentree", "Rentrée universitaire :",
                  choices = sort(unique(etab_aggreg$rentree), decreasing = TRUE)),
      selectInput("secteur", "Secteur :",
                  choices = c("Tous", sort(unique(etab_aggreg$secteur)))),
      selectInput("categorie", "Type d'établissement :",
                  choices = c("Tous", sort(unique(etab_aggreg$categorie)))),
      checkboxInput("show_circles", "Afficher les cercles", TRUE),
      checkboxInput("show_cantons", "Afficher les cantons", FALSE),
      checkboxInput("show_arr", "Afficher les arrondissements", FALSE),
      checkboxInput("show_heat", "Afficher la carte de chaleur", FALSE),
      checkboxInput("show_clusters", "Afficher les points en cluster", FALSE),
      checkboxInput("show_base_map", "Afficher le fond de carte", TRUE),
      tags$div(class = "bloc-exports",
        downloadButton("export_map", "Exporter la carte (HTML)"),
        downloadButton("export_graph", "Exporter le graphique (PNG)")),

      tags$hr(),
      # Logo et mention placés AVANT les sources : sur un bandeau étroit, ils
      # restent ainsi visibles sans avoir à faire défiler la page.
      if (!is.na(logo_src))
        tags$img(src = logo_src,
                 style = "width:100%; max-width:230px; height:auto; margin-bottom:4px;"),
      tags$div(class = "deestri-mention", MENTION_DEESTRI),
      tags$hr(),
      # NB : construit en UNE SEULE chaîne HTML. Avec tags$div(...), htmltools
      # place chaque élément sur sa propre ligne et le saut de ligne s'affiche
      # comme une espace — d'où un espace parasite avant chaque « . ».
      tags$div(class = "source-block", HTML(paste0(
        "<b>Sources : </b>",
        "Effectifs étudiants — Ministère de l'Enseignement supérieur, de la ",
        "Recherche et de l'Espace (MESRE) — ",
        "<a href=\"https://data.enseignementsup-recherche.gouv.fr/explore/assets/",
        "fr-esr-atlas_regional-effectifs-d-etudiants-inscrits-detail_etablissements/\" ",
        "target=\"_blank\" rel=\"noopener\">Effectifs d'étudiants inscrits dans les ",
        "établissements et les formations de l'enseignement supérieur — détail ",
        "par établissements (open data MESRE)</a>. ",
        "Limites administratives — IGN, ",
        "<a href=\"https://www.data.gouv.fr/datasets/admin-express-admin-express-cog",
        "-admin-express-cog-carto-admin-express-cog-carto-pe-admin-express-cog-carto",
        "-plus-pe/\" target=\"_blank\" rel=\"noopener\">ADMIN-EXPRESS-COG-CARTO</a>. ",
        "Codes géographiques — INSEE, ",
        "<a href=\"https://www.insee.fr/fr/information/2560452\" target=\"_blank\" ",
        "rel=\"noopener\">Code officiel géographique (COG)</a>."
      )))
    ),
    mainPanel(
      width = 10,
      tabsetPanel(
        tabPanel("Carte", leafletOutput("carte", height = "calc(100vh - 165px)")),
        tabPanel("Graphiques",
          tabsetPanel(id = "onglet_graph",
            tabPanel("Niveau d'études",       value = "niveau",       plotOutput("g_niveau",      height = 480)),
            tabPanel("Sexe × niveau",         value = "sexe",         plotOutput("g_sexe",        height = 480)),
            tabPanel("Secteur",               value = "secteur",      plotOutput("g_secteur",     height = 420)),
            tabPanel("Secteur × niveau",      value = "sec_niveau",   plotOutput("g_sec_niveau",  height = 520)),
            tabPanel("Catégorie",             value = "cat",          plotOutput("g_cat",         height = 620)),
            tabPanel("Catégorie × sexe",      value = "cat_sexe",     plotOutput("g_cat_sexe",    height = 760)),
            tabPanel("Catégorie × secteur",   value = "cat_secteur",  plotOutput("g_cat_secteur", height = 760)),
            tabPanel("Unités urbaines",       value = "uu",           plotOutput("g_uu",          height = 1600)),
            tabPanel("Évolution — sexe", value = "evol",
              tags$div(style = "margin-top:10px; max-width:520px;",
                       sliderInput("per_evol", "Période affichée :",
                                   min = an_min, max = an_max,
                                   value = c(an_min, an_max), step = 1, sep = "")),
              plotOutput("g_evol", height = 620)),
            tabPanel("Évolution — secteur", value = "evol_secteur",
              tags$div(style = "margin-top:10px; max-width:520px;",
                       sliderInput("per_evol_secteur", "Période affichée :",
                                   min = an_min, max = an_max,
                                   value = c(an_min, an_max), step = 1, sep = "")),
              plotOutput("g_evol_secteur", height = 680)),
            tabPanel("Évolution — niveau", value = "evol_niveau",
              tags$div(style = "margin-top:10px; max-width:520px;",
                       sliderInput("per_evol_niveau", "Période affichée :",
                                   min = an_min, max = an_max,
                                   value = c(an_min, an_max), step = 1, sep = "")),
              plotOutput("g_evol_niveau", height = 820)),
            tabPanel("Évolution — catégorie", value = "evol_cat",
              tags$div(style = "margin-top:10px; max-width:520px;",
                       sliderInput("per_evol_cat", "Période affichée :",
                                   min = an_min, max = an_max,
                                   value = c(an_min, an_max), step = 1, sep = "")),
              plotOutput("g_evol_cat", height = 1250))
            # NB : le sous-onglet « Composantes d'un établissement » a été
            # rapatrié dans la Fiche établissement, où il partage le sélecteur
            # de la fiche au lieu d'en imposer un second.
          )
        ),
        tabPanel("Fiche établissement",
          tags$div(style = "margin-top:12px;",
            fluidRow(
              column(8, selectInput("fiche_etab", "Établissement :",
                                    choices = liste_etab_alpha,
                                    selected = liste_etablissements[1],
                                    width = "100%")),
              column(4, tags$div(style = "margin-top:26px; font-style:italic; color:#555;",
                                 textOutput("fiche_annee", inline = TRUE)))
            ),
            uiOutput("fiche_kpi"),
            tags$h4("Localisation des composantes",
                    style = "color:#003F7D; font-weight:700; margin-top:18px;"),
            leafletOutput("carte_fiche", height = 420),
            tags$h4("Évolution des effectifs",
                    style = "color:#003F7D; font-weight:700; margin-top:18px;"),
            tags$div(style = "max-width:520px;",
              sliderInput("per_fiche", "Période affichée :",
                          min = an_min, max = an_max,
                          value = c(an_min, an_max), step = 1, sep = "")),
            plotOutput("g_fiche_evol", height = 520),
            tags$h4("Répartition par niveau et par sexe",
                    style = "color:#003F7D; font-weight:700; margin-top:18px;"),
            plotOutput("g_fiche_niveau", height = 520),
            # Pleine largeur et hauteur proportionnelle au nombre de composantes.
            # En demi-largeur, les noms de composantes reconstruits occupaient
            # l'essentiel du cadre et écrasaient les barres.
            tags$h4("Poids des composantes",
                    style = "color:#003F7D; font-weight:700; margin-top:18px;"),
            uiOutput("ui_g_fiche_comp"),
            # Évolution des composantes : graphiques rapatriés de l'onglet
            # « Graphiques ». Ils portent sur l'établissement de la fiche.
            # Découpage et période restent dans la zone du graphique.
            tags$h4("Évolution des composantes",
                    style = "color:#003F7D; font-weight:700; margin-top:18px;"),
            tags$div(style = "margin-bottom:4px;",
              fluidRow(
                column(5, radioButtons("decoupage_comp", "Découpage :",
                                       choices = c("Total" = "total",
                                                   "Par sexe" = "sexe",
                                                   "Par niveau" = "niveau"),
                                       selected = "total", inline = TRUE)),
                column(7, tags$div(style = "max-width:520px;",
                  sliderInput("per_comp", "Période affichée :",
                              min = an_min, max = an_max,
                              value = c(an_min, an_max), step = 1, sep = "")))
              )
            ),
            tags$div(style = "font-size:12px; color:#666; margin-bottom:6px;",
                     "Survolez un point — ou cliquez dessus sur écran tactile — ",
                     "pour afficher l'effectif de la composante à cette rentrée."),
            # Conteneur en position relative : l'infobulle se place en absolu
            # par rapport à lui, aux coordonnées du curseur.
            tags$div(style = "position:relative;",
                     uiOutput("ui_g_comp"),
                     uiOutput("infobulle_comp")),
            tags$h4("Détail par composante",
                    style = "color:#003F7D; font-weight:700; margin-top:18px;"),
            DTOutput("fiche_table"),
            tags$div(style = "height:20px;")
          )
        ),
        tabPanel("Comparateur",
          tags$div(style = "margin-top:12px;",
            fluidRow(
              column(6, selectInput("comp_a", "Établissement A :",
                                    choices = liste_etab_alpha,
                                    selected = liste_etablissements[1], width = "100%")),
              column(6, selectInput("comp_b", "Établissement B :",
                                    choices = liste_etab_alpha,
                                    selected = liste_etablissements[min(2, length(liste_etablissements))],
                                    width = "100%"))
            ),
            tags$div(style = "margin-bottom:10px;",
                     downloadButton("export_comparateur",
                                    "Exporter la comparaison (PDF)",
                                    class = "btn-primary")),
            uiOutput("comp_titre_cles"),
            DTOutput("comp_table"),
            tags$h4("Évolution comparée des effectifs",
                    style = "color:#003F7D; font-weight:700; margin-top:18px;"),
            tags$div(style = "max-width:520px;",
              sliderInput("per_compar", "Période affichée :",
                          min = an_min, max = an_max,
                          value = c(an_min, an_max), step = 1, sep = "")),
            plotOutput("g_comp_evol", height = 480),
            fluidRow(
              column(6,
                tags$h4("Structure par niveau (en %)",
                        style = "color:#003F7D; font-weight:700; margin-top:18px;"),
                plotOutput("g_comp_niveau", height = 420)),
              column(6,
                tags$h4("Répartition par sexe",
                        style = "color:#003F7D; font-weight:700; margin-top:18px;"),
                plotOutput("g_comp_sexe", height = 420))
            ),
            tags$div(style = "height:20px;")
          )
        ),
        tabPanel("Synthèse par commune", DTOutput("tableau_etab")),
        tabPanel("Données", DTOutput("tableau"))
      )
    )
  )
)

# =============================================================================
#  SERVEUR
# =============================================================================
server <- function(input, output, session) {

  data_filtree <- reactive({
    d <- filter(etab_aggreg, rentree == input$rentree)
    if (input$secteur   != "Tous") d <- filter(d, secteur == input$secteur)
    if (input$categorie != "Tous") d <- filter(d, categorie == input$categorie)
    validate(need(nrow(d) > 0, MSG_VIDE))
    d
  })

  # Effectifs par canton (réactif aux filtres) + géométrie
  cantons_filtree <- reactive({
    d <- data_filtree()
    tot <- d %>% group_by(INSEE_CAN_CODE) %>%
      summarise(total_effectifs = sum(effectifs, na.rm = TRUE), .groups = "drop")
    det <- d %>% group_by(INSEE_CAN_CODE, degre) %>%
      summarise(effectif = sum(effectifs, na.rm = TRUE),
                femmes   = sum(femmes,    na.rm = TRUE),
                hommes   = sum(hommes,    na.rm = TRUE), .groups = "drop") %>%
      nest(details = c(degre, effectif, femmes, hommes))
    eff <- left_join(tot, det, by = "INSEE_CAN_CODE")
    geo_cantons %>%
      left_join(eff, by = "INSEE_CAN_CODE") %>%
      mutate(
        total_effectifs = tidyr::replace_na(total_effectifs, 0),
        details = replace_na(details, list(data.frame(
          degre = character(), effectif = numeric(),
          femmes = numeric(), hommes = numeric())))
      )
  })

  arr_filtree <- reactive({
    d <- data_filtree()
    tot <- d %>% group_by(INSEE_ARR_CODE) %>%
      summarise(total_effectifs = sum(effectifs, na.rm = TRUE), .groups = "drop")
    det <- d %>% group_by(INSEE_ARR_CODE, degre) %>%
      summarise(effectif = sum(effectifs, na.rm = TRUE),
                femmes   = sum(femmes,    na.rm = TRUE),
                hommes   = sum(hommes,    na.rm = TRUE), .groups = "drop") %>%
      nest(details = c(degre, effectif, femmes, hommes))
    eff <- left_join(tot, det, by = "INSEE_ARR_CODE")
    geo_arr %>%
      left_join(eff, by = "INSEE_ARR_CODE") %>%
      mutate(
        total_effectifs = tidyr::replace_na(total_effectifs, 0),
        details = replace_na(details, list(data.frame(
          degre = character(), effectif = numeric(),
          femmes = numeric(), hommes = numeric())))
      )
  })

  # Points établissement (agrégés composante) + popups
  points_filtree <- reactive({
    d <- data_filtree()
    cles <- c("uai_etab", "nom_etab", "uai_comp", "nom_comp",
              "categorie", "commune", "secteur",
              "geo_source", "latitude", "longitude", "INSEE_CAN_CODE", "INSEE_ARR_CODE")
    det <- d %>%
      group_by(across(all_of(c(cles, "degre")))) %>%
      summarise(effectif = sum(effectifs, na.rm = TRUE),
                femmes   = sum(femmes,    na.rm = TRUE),
                hommes   = sum(hommes,    na.rm = TRUE), .groups = "drop") %>%
      nest(details = c(degre, effectif, femmes, hommes))
    tot <- d %>%
      group_by(across(all_of(cles))) %>%
      summarise(effectifs = sum(effectifs, na.rm = TRUE),
                femmes    = sum(femmes,    na.rm = TRUE),
                hommes    = sum(hommes,    na.rm = TRUE), .groups = "drop")
    left_join(tot, det, by = cles) %>%
      # 344 établissements sur 365 n'ont qu'une composante, qui porte le même
      # nom qu'eux : on ne répète pas le libellé dans ce cas.
      mutate(etablissement = ifelse(nom_comp == nom_etab, nom_etab,
                                    paste0(nom_etab, " — ", nom_comp))) %>%
      rowwise() %>%
      mutate(popup_text = popup_etab(etablissement, commune, secteur, categorie,
                                     effectifs, femmes, hommes, details, geo_source)) %>%
      ungroup()
  })

  opts_courantes <- reactive(list(
    base = input$show_base_map, circles = input$show_circles,
    cantons = input$show_cantons, arr = input$show_arr,
    heat = input$show_heat, clusters = input$show_clusters
  ))

  # Cadrage courant de la carte, lu en isolate() : la lecture ne déclenche donc
  # pas de nouveau rendu (ce qui provoquerait une boucle sans fin).
  vue_courante <- function() {
    ctr <- isolate(input$carte_center)
    z   <- isolate(input$carte_zoom)
    if (is.null(ctr) || is.null(z)) return(NULL)
    list(lng = ctr$lng, lat = ctr$lat, zoom = z)
  }

  output$carte <- renderLeaflet({
    construire_carte(points_filtree(), cantons_filtree(), arr_filtree(),
                     opts_courantes(), vue_courante())
  })

  # ---- Graphiques (réagissent aux filtres) -----------------------------------
  # Données multi-années (filtrées secteur/catégorie, toutes rentrées) pour l'évolution
  data_multi <- reactive({
    d <- etab_aggreg
    if (input$secteur   != "Tous") d <- filter(d, secteur == input$secteur)
    if (input$categorie != "Tous") d <- filter(d, categorie == input$categorie)
    validate(need(nrow(d) > 0, MSG_VIDE))
    d
  })

  # Données multi-années SANS le filtre secteur (le graphique porte sur le secteur)
  data_multi_secteur <- reactive({
    d <- etab_aggreg
    if (input$categorie != "Tous") d <- filter(d, categorie == input$categorie)
    validate(need(nrow(d) > 0, MSG_VIDE))
    d
  })

  # Année sélectionnée, SANS le filtre secteur (graphiques portant sur le secteur)
  data_annee_tous_secteurs <- reactive({
    d <- filter(etab_aggreg, rentree == input$rentree)
    if (input$categorie != "Tous") d <- filter(d, categorie == input$categorie)
    validate(need(nrow(d) > 0, MSG_VIDE))
    d
  })

  # Année sélectionnée, SANS le filtre catégorie (graphiques portant sur la catégorie)
  data_annee_toutes_cat <- reactive({
    d <- filter(etab_aggreg, rentree == input$rentree)
    if (input$secteur != "Tous") d <- filter(d, secteur == input$secteur)
    validate(need(nrow(d) > 0, MSG_VIDE))
    d
  })

  # Multi-années, SANS le filtre catégorie (évolution par catégorie)
  data_multi_cat <- reactive({
    d <- etab_aggreg
    if (input$secteur != "Tous") d <- filter(d, secteur == input$secteur)
    validate(need(nrow(d) > 0, MSG_VIDE))
    d
  })

  # Restreint une série à la période choisie sur un curseur
  bornes <- function(p) if (is.null(p)) c(an_min, an_max) else p
  filtre_periode <- function(d, p) {
    p <- bornes(p)
    dplyr::filter(d, rentree >= p[1], rentree <= p[2])
  }
  # Mention de période pour les sous-titres
  txt_periode <- function(p) { p <- bornes(p); paste0("Période ", p[1], "-", p[2]) }

  sous_titre <- reactive(paste0("Rentrée ", input$rentree,
    if (input$secteur != "Tous") paste0(" — ", input$secteur) else "",
    if (input$categorie != "Tous") paste0(" — ", input$categorie) else ""))

  # G1 — Répartition par niveau d'études
  p_niveau <- reactive({
    d <- data_filtree() %>%
      group_by(degre) %>% summarise(eff = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      mutate(degre = factor(degre, levels = rev(niveaux_ordre)),
             part = eff / sum(eff))
    ggplot(d, aes(degre, eff)) +
      geom_col(fill = "#003F7D", width = 0.7) +
      geom_text(aes(label = paste0(fmt_eff(eff), " (", percent(part, accuracy = 0.1), ")")),
                hjust = -0.05, size = 4) +
      coord_flip() +
      scale_y_continuous(labels = fmt_eff, expand = expansion(mult = c(0, 0.18))) +
      labs(title = "Répartition par niveau d'études",
           subtitle = paste0(sous_titre(), mention_niveau(d$degre)),
           x = NULL, y = "Étudiants inscrits") +
      theme_obs
  })

  # G2 — Répartition par sexe et niveau
  p_sexe <- reactive({
    src <- data_filtree()
    tf <- sum(src$femmes, na.rm = TRUE); th <- sum(src$hommes, na.rm = TRUE)
    tt <- tf + th
    lab_sexe <- c(
      "Femmes" = paste0("Femmes : ", fmt_eff(tf), " (", percent(tf / tt, accuracy = 0.1), ")"),
      "Hommes" = paste0("Hommes : ", fmt_eff(th), " (", percent(th / tt, accuracy = 0.1), ")")
    )
    d <- src %>%
      group_by(degre) %>%
      summarise(Femmes = sum(femmes, na.rm = TRUE),
                Hommes = sum(hommes, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_longer(c(Femmes, Hommes), names_to = "Sexe", values_to = "eff") %>%
      group_by(degre) %>% mutate(part = eff / sum(eff)) %>% ungroup() %>%
      mutate(degre = factor(degre, levels = rev(niveaux_ordre)))
    ggplot(d, aes(degre, eff, fill = Sexe)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      geom_text(aes(label = paste0(fmt_eff(eff), " (", percent(part, accuracy = 0.1), ")")),
                position = position_dodge(width = 0.8), hjust = -0.05, size = 4) +
      coord_flip() +
      scale_fill_manual(values = col_sexe, labels = lab_sexe) +
      scale_y_continuous(labels = fmt_eff, expand = expansion(mult = c(0, 0.12))) +
      labs(title = "Répartition par sexe et par niveau d'études",
           subtitle = paste0(sous_titre(), mention_niveau(d$degre)),
           x = NULL, y = "Étudiants inscrits", fill = NULL) +
      theme_obs
  })

  # G3 — Répartition par secteur
  p_secteur <- reactive({
    # Ignore le filtre « Secteur » : ce graphique analyse précisément cette dimension
    d <- data_annee_tous_secteurs() %>%
      group_by(secteur) %>% summarise(eff = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      mutate(part = eff / sum(eff))
    ggplot(d, aes(reorder(secteur, eff), eff, fill = secteur)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = paste0(fmt_eff(eff), " (", percent(part, accuracy = 0.1), ")")),
                hjust = -0.05, size = 4.5) +
      coord_flip() +
      scale_fill_manual(values = col_secteur, guide = "none") +
      scale_y_continuous(labels = fmt_eff, expand = expansion(mult = c(0, 0.22))) +
      labs(title = "Répartition par secteur",
           subtitle = paste0("Rentrée ", input$rentree,
                             if (input$categorie != "Tous") paste0(" — ", input$categorie)
                             else " — toutes catégories d'établissement"),
           x = NULL, y = "Étudiants inscrits") +
      theme_obs
  })

  # G4 — Répartition par catégorie d'établissement (top 12)
  p_cat <- reactive({
    # Ignore le filtre « Type d'établissement » : ce graphique analyse cette dimension
    src <- data_annee_toutes_cat()
    tot <- sum(src$effectifs, na.rm = TRUE)
    d <- src %>%
      group_by(categorie) %>% summarise(eff = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      filter(eff > 0) %>% mutate(part = eff / tot)
    ggplot(d, aes(reorder(categorie, eff), eff)) +
      geom_col(fill = "#003F7D", width = 0.7) +
      geom_text(aes(label = paste0(fmt_eff(eff), " (", percent(part, accuracy = 0.1), ")")),
                hjust = -0.05, size = 4.2) +
      coord_flip() +
      scale_y_continuous(labels = fmt_eff, expand = expansion(mult = c(0, 0.22))) +
      labs(title = "Répartition par catégorie d'établissement",
           subtitle = paste0("Rentrée ", input$rentree,
                             if (input$secteur != "Tous") paste0(" — ", input$secteur)
                             else " — tous secteurs"),
           x = NULL, y = "Étudiants inscrits") +
      theme_obs
  })

  # G5 — Répartition par unité urbaine (toutes, comme la note 2023)
  p_uu <- reactive({
    tot <- sum(data_filtree()$effectifs, na.rm = TRUE)
    uu <- data_filtree() %>%
      mutate(unite_urbaine = ifelse(is.na(unite_urbaine) | unite_urbaine == "",
                                    "Non renseignée", unite_urbaine)) %>%
      group_by(unite_urbaine) %>%
      summarise(eff = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      filter(eff > 0) %>% mutate(part = eff / tot) %>% arrange(desc(eff))
    ggplot(uu, aes(reorder(unite_urbaine, eff), eff)) +
      geom_col(fill = "#1F78B4", width = 0.8) +
      geom_text(aes(label = paste0(fmt_eff(eff), " (", percent(part, accuracy = 0.1), ")")),
                hjust = -0.03, size = 3.4) +
      coord_flip() +
      scale_y_continuous(labels = fmt_eff, expand = expansion(mult = c(0, 0.16))) +
      labs(title = "Répartition par unité urbaine", subtitle = sous_titre(),
           x = NULL, y = "Étudiants inscrits") +
      theme_obs +
      theme(axis.text.y = element_text(size = 11.5),
            axis.text.x = element_text(size = 12))
  })

  # G6 — Évolution des effectifs 2015-2024 (Total, Femmes, Hommes)
  p_evol <- reactive({
    d <- data_multi() %>% filtre_periode(input$per_evol) %>%
      group_by(rentree) %>%
      summarise(Total  = sum(effectifs, na.rm = TRUE),
                Femmes = sum(femmes,    na.rm = TRUE),
                Hommes = sum(hommes,    na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_longer(c(Total, Femmes, Hommes),
                          names_to = "Série", values_to = "eff") %>%
      mutate(`Série` = factor(`Série`, levels = c("Total", "Femmes", "Hommes"))) %>%
      ajouter_rang()
    st <- paste0(txt_periode(input$per_evol), " — ",
                 if (input$secteur != "Tous") input$secteur else "tous secteurs",
                 " — ",
                 if (input$categorie != "Tous") input$categorie
                 else "toutes catégories d'établissement")
    ggplot(d, aes(rentree, eff, colour = `Série`)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.8) +
      couche_etiquettes(4.6) +
      scale_colour_manual(values = col_evol) +
      scale_x_continuous(breaks = sort(unique(d$rentree))) +
      scale_y_continuous(labels = fmt_eff, breaks = breaks_effectifs(),
                         expand = expansion(mult = c(0.24, 0.20))) +
      labs(title = "Évolution des effectifs étudiants en Normandie", subtitle = st,
           x = NULL, y = "Étudiants inscrits", colour = NULL) +
      theme_obs
  })

  # G7 — Évolution des effectifs par secteur (Total, Public, Privé)
  p_evol_secteur <- reactive({
    base <- data_multi_secteur() %>% filtre_periode(input$per_evol_secteur)
    par_secteur <- base %>%
      group_by(rentree, `Série` = secteur) %>%
      summarise(eff = sum(effectifs, na.rm = TRUE), .groups = "drop")
    total <- base %>%
      group_by(rentree) %>%
      summarise(`Série` = "Total", eff = sum(effectifs, na.rm = TRUE), .groups = "drop")
    d <- bind_rows(total, par_secteur) %>%
      mutate(`Série` = factor(`Série`, levels = c("Total", "Public", "Privé"))) %>%
      ajouter_rang()
    st <- paste0(txt_periode(input$per_evol_secteur), " — ",
                 if (input$categorie != "Tous") input$categorie
                 else "toutes catégories d'établissement")
    ggplot(d, aes(rentree, eff, colour = `Série`)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.8) +
      couche_etiquettes(4.6) +
      scale_colour_manual(values = col_evol_secteur) +
      scale_x_continuous(breaks = sort(unique(d$rentree))) +
      scale_y_continuous(labels = fmt_eff, breaks = breaks_effectifs(),
                         expand = expansion(mult = c(0.24, 0.20))) +
      labs(title = "Évolution des effectifs par secteur en Normandie", subtitle = st,
           x = NULL, y = "Étudiants inscrits", colour = NULL) +
      theme_obs
  })

  # G7 ter — Évolution des effectifs par niveau d'études
  # Ce graphique analyse le NIVEAU, qui n'est pas un filtre du bandeau gris :
  # les filtres « Secteur » et « Type d'établissement » s'y appliquent donc
  # normalement, comme sur les autres courbes d'évolution.
  # Rappel : la source ne renseigne le niveau qu'à partir de 2021. Avant, tous
  # les effectifs sont portés par la série « Niveau non renseigné », affichée
  # comme les autres plutôt que masquée — aucun étudiant n'est écarté.
  p_evol_niveau <- reactive({
    base <- data_multi() %>% filtre_periode(input$per_evol_niveau)
    validate(need(nrow(base) > 0, MSG_VIDE))
    d <- base %>%
      group_by(rentree, `Série` = degre) %>%
      summarise(eff = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      mutate(`Série` = factor(`Série`, levels = niveaux_ordre)) %>%
      ajouter_rang()
    st <- paste0(txt_periode(input$per_evol_niveau), " — ",
                 if (input$secteur != "Tous") input$secteur else "tous secteurs",
                 " — ",
                 if (input$categorie != "Tous") input$categorie
                 else "toutes catégories d'établissement",
                 mention_niveau(d$`Série`))
    ggplot(d, aes(rentree, eff, colour = `Série`)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.8) +
      couche_etiquettes(4.2) +
      scale_colour_manual(values = col_niveaux) +
      scale_x_continuous(breaks = sort(unique(d$rentree)),
                         expand = expansion(mult = c(0.06, 0.06))) +
      scale_y_continuous(labels = fmt_eff, breaks = breaks_effectifs(),
                         expand = expansion(mult = c(0.16, 0.18))) +
      labs(title = "Évolution des effectifs par niveau d'études en Normandie",
           subtitle = st, x = NULL, y = "Étudiants inscrits", colour = NULL) +
      theme_obs
  })

  # G8 — Répartition par secteur et niveau d'études
  p_sec_niveau <- reactive({
    src <- data_annee_tous_secteurs()
    tot_sec <- src %>% group_by(secteur) %>%
      summarise(t = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      mutate(lab = paste0(secteur, " : ", fmt_eff(t), " (",
                          percent(t / sum(t), accuracy = 0.1), ")"))
    lab_sec <- setNames(tot_sec$lab, tot_sec$secteur)
    d <- src %>%
      group_by(degre, secteur) %>%
      summarise(eff = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      group_by(degre) %>% mutate(part = eff / sum(eff)) %>% ungroup() %>%
      mutate(degre = factor(degre, levels = rev(niveaux_ordre)))
    ggplot(d, aes(degre, eff, fill = secteur)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      geom_text(aes(label = paste0(fmt_eff(eff), " (", percent(part, accuracy = 0.1), ")")),
                position = position_dodge(width = 0.8), hjust = -0.05, size = 4) +
      coord_flip() +
      scale_fill_manual(values = col_secteur, labels = lab_sec) +
      scale_y_continuous(labels = fmt_eff, expand = expansion(mult = c(0, 0.16))) +
      labs(title = "Répartition par secteur et par niveau d'études",
           subtitle = paste0("Rentrée ", input$rentree,
                             if (input$categorie != "Tous") paste0(" — ", input$categorie) else "",
                             mention_niveau(d$degre)),
           x = NULL, y = "Étudiants inscrits", fill = NULL) +
      theme_obs
  })

  # G9 — Répartition par catégorie d'établissement et par sexe
  p_cat_sexe <- reactive({
    src <- data_annee_toutes_cat()
    d <- src %>%
      group_by(categorie) %>%
      summarise(Femmes = sum(femmes, na.rm = TRUE),
                Hommes = sum(hommes, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_longer(c(Femmes, Hommes), names_to = "Sexe", values_to = "eff") %>%
      group_by(categorie) %>% mutate(part = eff / sum(eff),
                                     tot_cat = sum(eff)) %>% ungroup()
    ggplot(d, aes(reorder(categorie, tot_cat), eff, fill = Sexe)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      geom_text(aes(label = paste0(fmt_eff(eff), " (", percent(part, accuracy = 0.1), ")")),
                position = position_dodge(width = 0.8), hjust = -0.05, size = 3.8) +
      coord_flip() +
      scale_fill_manual(values = col_sexe) +
      scale_y_continuous(labels = fmt_eff, expand = expansion(mult = c(0, 0.20))) +
      labs(title = "Répartition par catégorie d'établissement et par sexe",
           subtitle = paste0("Rentrée ", input$rentree,
                             if (input$secteur != "Tous") paste0(" — ", input$secteur) else ""),
           x = NULL, y = "Étudiants inscrits", fill = NULL) +
      theme_obs
  })

  # G10 — Répartition par catégorie d'établissement et par secteur
  p_cat_secteur <- reactive({
    src <- filter(etab_aggreg, rentree == input$rentree)
    d <- src %>%
      group_by(categorie, secteur) %>%
      summarise(eff = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      group_by(categorie) %>% mutate(part = eff / sum(eff),
                                     tot_cat = sum(eff)) %>% ungroup()
    ggplot(d, aes(reorder(categorie, tot_cat), eff, fill = secteur)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      geom_text(aes(label = paste0(fmt_eff(eff), " (", percent(part, accuracy = 0.1), ")")),
                position = position_dodge(width = 0.8), hjust = -0.05, size = 3.8) +
      coord_flip() +
      scale_fill_manual(values = col_secteur) +
      scale_y_continuous(labels = fmt_eff, expand = expansion(mult = c(0, 0.20))) +
      labs(title = "Répartition par catégorie d'établissement et par secteur",
           subtitle = paste0("Rentrée ", input$rentree),
           x = NULL, y = "Étudiants inscrits", fill = NULL) +
      theme_obs
  })

  # G11 — Évolution par catégorie d'établissement, avec le sexe
  #  Le classement des catégories est calculé UNE FOIS sur l'ensemble des données
  #  (tous secteurs, toutes années) : les mêmes catégories, dans le même ordre,
  #  s'affichent quel que soit le filtre. Seules disparaissent celles réellement
  #  absentes du secteur choisi (ex. : les universités en « Privé »).
  p_evol_cat <- reactive({
    d <- data_multi_cat() %>% filtre_periode(input$per_evol_cat) %>%
      group_by(categorie, rentree) %>%
      summarise(Total  = sum(effectifs, na.rm = TRUE),
                Femmes = sum(femmes,    na.rm = TRUE),
                Hommes = sum(hommes,    na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_longer(c(Total, Femmes, Hommes),
                          names_to = "Série", values_to = "eff") %>%
      mutate(`Série` = factor(`Série`, levels = c("Total", "Femmes", "Hommes")),
             categorie = factor(categorie, levels = ordre_categories))
    ggplot(d, aes(rentree, eff, colour = `Série`)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.2) +
      facettes(~ categorie, ncol = 3, drop = TRUE,
               labeller = label_wrap_gen(width = 32)) +
      scale_colour_manual(values = col_evol) +
      scale_x_continuous(breaks = breaks_annees(4)) +
      scale_y_continuous(labels = fmt_eff, breaks = breaks_effectifs()) +
      labs(title = "Évolution par catégorie d'établissement et par sexe",
           subtitle = paste0(txt_periode(input$per_evol_cat), " — ",
                             if (input$secteur != "Tous") input$secteur else "tous secteurs",
                             " — toutes les catégories"),
           x = NULL, y = "Étudiants inscrits", colour = NULL) +
      theme_obs +
      theme(panel.grid.major.y = element_line(colour = "grey92"),
            strip.text = element_text(face = "bold", size = 12))
  })

  # ---- Évolution des composantes d'un établissement --------------------------
  # Affiché dans la Fiche établissement : ce graphique reprend le périmètre de
  # la fiche, donc son sélecteur — il n'y a plus qu'un seul choix
  # d'établissement dans l'application. Il ignore les filtres « Secteur »,
  # « Type d'établissement » et « Rentrée » de la zone grise (l'établissement
  # porte déjà son secteur et sa catégorie, et l'on affiche toute la série
  # temporelle). fiche_all() est défini plus bas dans le fichier : une réactive
  # n'étant évaluée qu'à l'exécution, l'ordre de déclaration est sans incidence.
  data_comp <- reactive(fiche_all())

  # Nombre de composantes RÉELLEMENT affichées (donc après filtre de période) :
  # sans cela, restreindre la période laissait un grand vide sous le graphique.
  n_panneaux <- reactive({
    d <- data_comp() %>% filtre_periode(input$per_comp)
    length(unique(d$panneau[!is.na(d$effectifs) & d$effectifs > 0]))
  })
  hauteur_comp <- reactive(max(400, ceiling(max(1, n_panneaux()) / 3) * 300 + 130))

  # Survol et clic activés : ils alimentent l'infobulle ci-dessous. Le survol
  # est débruité (debounce) pour ne pas saturer le serveur, et remis à NULL dès
  # que le curseur quitte le graphique afin que l'infobulle disparaisse.
  output$ui_g_comp <- renderUI({
    plotOutput("g_comp", height = paste0(hauteur_comp(), "px"),
               hover = hoverOpts(id = "g_comp_hover", delay = 80,
                                 delayType = "debounce", nullOutside = TRUE),
               click = "g_comp_click")
  })

  # Données effectivement tracées, isolées dans leur propre réactive : le
  # graphique ET l'infobulle de survol s'appuient sur la même table, de sorte
  # que la valeur affichée au survol est exactement celle du point dessiné.
  d_comp_trace <- reactive({
    d0 <- data_comp() %>% filtre_periode(input$per_comp)
    validate(need(nrow(d0) > 0, "Aucune donnée pour cet établissement."))

    ordre_panneaux <- d0 %>%
      group_by(panneau) %>%
      summarise(t = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(t)) %>% pull(panneau)

    dec <- input$decoupage_comp
    if (dec == "sexe") {
      d <- d0 %>%
        group_by(panneau, rentree) %>%
        summarise(Total  = sum(effectifs, na.rm = TRUE),
                  Femmes = sum(femmes,    na.rm = TRUE),
                  Hommes = sum(hommes,    na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_longer(c(Total, Femmes, Hommes),
                            names_to = "serie", values_to = "eff") %>%
        mutate(serie = factor(serie, levels = c("Total", "Femmes", "Hommes")))
    } else if (dec == "niveau") {
      d <- d0 %>%
        group_by(panneau, rentree, degre) %>%
        summarise(eff = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
        rename(serie = degre) %>%
        mutate(serie = factor(serie, levels = niveaux_ordre))
    } else {
      d <- d0 %>%
        group_by(panneau, rentree) %>%
        summarise(eff = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
        mutate(serie = factor("Total"))
    }
    d %>% mutate(panneau = factor(panneau, levels = ordre_panneaux))
  })

  p_comp <- reactive({
    dec <- input$decoupage_comp
    d   <- d_comp_trace()
    pal <- switch(dec, sexe = col_evol, niveau = col_niveaux,
                  c("Total" = "#333333"))
    titre_leg <- NULL

    ggplot(d, aes(rentree, eff, colour = serie)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.9) +
      facettes(~ panneau, ncol = 3, labeller = label_wrap_gen(width = 38)) +
      scale_colour_manual(values = pal, drop = FALSE) +
      scale_x_continuous(breaks = breaks_annees(4)) +
      scale_y_continuous(labels = fmt_eff, breaks = breaks_effectifs()) +
      labs(title = paste0("Évolution des composantes — ", input$fiche_etab),
           subtitle = paste0(
             switch(dec, total = "Effectifs totaux",
                    sexe = "Par sexe", niveau = "Par niveau d'études"),
             " — ", n_panneaux(), " composante(s) — ", txt_periode(input$per_comp)),
           x = NULL, y = "Étudiants inscrits", colour = titre_leg) +
      theme_obs +
      theme(panel.grid.major.y = element_line(colour = "grey92"),
            strip.text = element_text(face = "bold", size = 11))
  })

  # ---- Infobulle sur les points de l'évolution des composantes ---------------
  # Réalisée avec les seuls outils natifs de Shiny (hoverOpts + nearPoints) :
  # aucune bibliothèque supplémentaire n'est requise, donc rien à installer
  # derrière le proxy de la Région, et aucun appel réseau au moment de l'usage.
  # Le survol est prioritaire ; à défaut, le dernier clic est utilisé (utile sur
  # écran tactile, où le survol n'existe pas). Un clic à l'écart d'un point
  # referme l'infobulle, puisque nearPoints() ne renvoie alors aucune ligne.
  # Le clic est mémorisé à part, et effacé dès que l'établissement, la période
  # ou le découpage changent : sans cela, l'ancien clic resterait valide et
  # ferait apparaître une infobulle sur des données qui ne sont plus les mêmes.
  clic_comp <- reactiveVal(NULL)
  observeEvent(input$g_comp_click, clic_comp(input$g_comp_click))
  observeEvent(list(input$fiche_etab, input$per_comp, input$decoupage_comp),
               clic_comp(NULL), ignoreInit = TRUE)

  point_survole <- reactive({
    ev <- input$g_comp_hover
    if (is.null(ev)) ev <- clic_comp()
    if (is.null(ev)) return(NULL)
    d <- tryCatch(d_comp_trace(), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    # La variable de facette est transmise par Shiny sous forme de texte : la
    # colonne doit être de même nature pour que le panneau soit bien identifié.
    d <- as.data.frame(d) %>% mutate(panneau = as.character(panneau))
    pts <- nearPoints(d, ev, maxpoints = 1, threshold = 18, addDist = FALSE)
    if (nrow(pts) == 0) return(NULL)
    list(ev = ev, pt = pts[1, , drop = FALSE])
  })

  output$infobulle_comp <- renderUI({
    info <- point_survole()
    if (is.null(info)) return(NULL)
    pt <- info$pt
    xy <- info$ev$coords_css
    if (is.null(xy)) return(NULL)
    serie <- as.character(pt$serie)
    tags$div(
      style = paste0(
        "position:absolute; z-index:120; pointer-events:none; ",
        "left:", round(xy$x) + 14, "px; ",
        "top:",  max(0, round(xy$y) - 10), "px; ",
        "background:rgba(255,255,255,0.97); ",
        "border:1px solid #003F7D; border-left:4px solid #C7102C; ",
        "border-radius:3px; padding:6px 10px; max-width:320px; ",
        "font-family:'Avenir Next','Avenir','Segoe UI',Arial,sans-serif; ",
        "font-size:12px; line-height:1.45; color:#222222; ",
        "box-shadow:0 2px 6px rgba(0,0,0,0.18);"),
      tags$div(style = "font-weight:700; color:#003F7D;", pt$panneau),
      tags$div(paste0("Rentrée ", as.integer(pt$rentree))),
      tags$div(style = "font-weight:700;",
               paste0(if (identical(serie, "Total")) "Effectif" else serie,
                      " : ", fmt_eff(pt$eff), " étudiant(s)"))
    )
  })

  # ===========================================================================
  #  FICHE ÉTABLISSEMENT
  #  Pilotée par son sélecteur d'établissement ; l'année provient du filtre
  #  « Rentrée universitaire » de la zone grise (les graphiques d'évolution,
  #  eux, couvrent toute la période).
  # ===========================================================================

  fiche_all <- reactive({
    req(input$fiche_etab)
    etab_aggreg %>%
      filter(etab_affiche == input$fiche_etab) %>%
      # Étiquette des composantes : le nom seul, court et lisible. La commune
      # n'est ajoutée que pour les rares homonymes coexistant dans le même
      # établissement (ESIX Caen / ESIX Cherbourg, antenne IUT GON Cherbourg /
      # Saint-Lô) : sans elle, ils fusionneraient en une seule série.
      group_by(nom_comp) %>%
      mutate(panneau = ifelse(n_distinct(uai_comp) > 1, comp_affiche, nom_comp)) %>%
      ungroup()
  })
  fiche_annee_data <- reactive(fiche_all() %>% filter(rentree == input$rentree))

  output$fiche_annee <- renderText(paste0("Chiffres clés : rentrée ", input$rentree))

  # --- Bandeau de chiffres clés ---
  output$fiche_kpi <- renderUI({
    d <- fiche_annee_data()
    if (nrow(d) == 0)
      return(tags$div(style = "padding:12px; color:#C7102C;",
                      "Aucun effectif pour cet établissement à la rentrée ", input$rentree, "."))
    tot <- sum(d$effectifs, na.rm = TRUE)
    f   <- sum(d$femmes,    na.rm = TRUE)
    h   <- sum(d$hommes,    na.rm = TRUE)
    # Évolution par rapport à la rentrée précédente
    prec <- fiche_all() %>% filter(rentree == as.integer(input$rentree) - 1)
    tot_prec <- sum(prec$effectifs, na.rm = TRUE)
    evo <- if (tot_prec > 0) {
      v <- (tot - tot_prec) / tot_prec
      paste0(if (v >= 0) "+" else "", percent(v, accuracy = 0.1), " vs ",
             as.integer(input$rentree) - 1)
    } else "—"
    boite <- function(titre, valeur, couleur = "#003F7D") {
      column(2, tags$div(
        style = paste0("border-left:4px solid ", couleur,
                       "; background:#F7F9FC; padding:8px 10px; margin-bottom:6px;"),
        tags$div(style = "font-size:11px; color:#555;", titre),
        tags$div(style = paste0("font-size:19px; font-weight:700; color:", couleur, ";"),
                 valeur)))
    }
    cr <- croissance_10ans()
    # Couleurs : vert si hausse, rouge si baisse, gris si indéterminé.
    # (grepl("^-", ...) échouait sur le tiret cadratin « — », affiché en vert.)
    coul_evo <- if (identical(evo, "—")) "#666666"
                else if (startsWith(evo, "-")) "#C7102C" else "#1a9850"
    coul_cr  <- if (is.na(cr$an1)) "#666666"
                else if (cr$val < 0) "#C7102C" else "#1a9850"
    lib_cr   <- if (is.na(cr$an1)) "Croissance (période)"
                else paste0("Croissance ", cr$an1, "-", cr$an2)
    lib_e1   <- if (is.na(cr$an1)) "Effectifs (début)" else paste0("Effectifs ", cr$an1)
    lib_e2   <- if (is.na(cr$an2)) "Effectifs (fin)"   else paste0("Effectifs ", cr$an2)
    tagList(
      fluidRow(
        boite("Étudiants inscrits", fmt_eff(tot)),
        boite("Femmes", paste0(fmt_eff(f), " (", percent(f / tot, accuracy = 0.1), ")"), "#C7102C"),
        boite("Hommes", paste0(fmt_eff(h), " (", percent(h / tot, accuracy = 0.1), ")"), "#003F7D"),
        boite("Composantes", as.character(n_distinct(d$panneau)), "#333333"),
        boite("Communes", as.character(n_distinct(d$commune)), "#333333"),
        boite("Évolution annuelle", evo, coul_evo)
      ),
      fluidRow(
        boite(lib_cr, cr$txt_total, coul_cr),
        boite("Rythme annuel moyen", cr$txt_tcam, coul_cr),
        boite(lib_e1, fmt_eff(cr$e1), "#333333"),
        boite(lib_e2, fmt_eff(cr$e2), "#333333"),
        column(4, tags$div(style = "margin-top:14px;",
          downloadButton("export_fiche", "Exporter la fiche (Word)",
                         class = "btn-primary")))
      )
    )
  })

  # --- Croissance sur la période disponible (10 ans) ---
  croissance_10ans <- reactive({
    s <- fiche_all() %>%
      group_by(rentree) %>%
      summarise(eff = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      filter(eff > 0) %>% arrange(rentree)
    if (nrow(s) < 2)
      return(list(an1 = NA, an2 = NA, e1 = 0, e2 = 0, val = 0,
                  txt_total = "—", txt_tcam = "—"))
    an1 <- min(s$rentree); an2 <- max(s$rentree)
    e1 <- s$eff[s$rentree == an1]; e2 <- s$eff[s$rentree == an2]
    val <- (e2 - e1) / e1
    n <- an2 - an1
    tcam <- (e2 / e1)^(1 / n) - 1
    list(an1 = an1, an2 = an2, e1 = e1, e2 = e2, val = val,
         txt_total = paste0(if (val >= 0) "+" else "", percent(val, accuracy = 0.1)),
         txt_tcam  = paste0(if (tcam >= 0) "+" else "", percent(tcam, accuracy = 0.1), " / an"))
  })

  # --- Carte des composantes (cercles proportionnels) ---
  output$carte_fiche <- renderLeaflet({
    d <- fiche_annee_data() %>%
      group_by(panneau, nom_comp, commune, secteur, latitude, longitude, geo_source) %>%
      summarise(effectifs = sum(effectifs, na.rm = TRUE),
                femmes    = sum(femmes,    na.rm = TRUE),
                hommes    = sum(hommes,    na.rm = TRUE), .groups = "drop") %>%
      filter(effectifs > 0)
    validate(need(nrow(d) > 0, "Aucune composante localisée pour cette rentrée."))
    det <- fiche_annee_data() %>%
      group_by(panneau, degre) %>%
      summarise(effectif = sum(effectifs, na.rm = TRUE),
                femmes   = sum(femmes,    na.rm = TRUE),
                hommes   = sum(hommes,    na.rm = TRUE), .groups = "drop") %>%
      nest(details = c(degre, effectif, femmes, hommes))
    d <- left_join(d, det, by = "panneau") %>%
      rowwise() %>%
      mutate(popup_text = paste0(
        "<b>", nom_comp, "</b><br/>Commune : ", commune,
        "<br/>Effectif total : ", fmt_eff(effectifs),
        "<br/>Femmes : ", fmt_eff(femmes), " — Hommes : ", fmt_eff(hommes),
        if (!is.na(geo_source) && grepl("centro|approx", geo_source))
          "<br/><i>(localisation approximative : centre de la commune)</i>" else "",
        bloc_details(details))) %>%
      ungroup()
    leaflet(d) %>%
      addTiles() %>%
      addScaleBar(position = "bottomleft") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = ~pmax(4, 0.6 * sqrt(effectifs)),
                       color = ~ifelse(!is.na(geo_source) &
                                         grepl("centro|approx", geo_source),
                                       "#C7102C", "#1F4E79"),
                       fillColor = ~pal_secteur(secteur),
                       stroke = TRUE, weight = 2, opacity = 0.95, fillOpacity = 0.45,
                       popup = ~popup_text) %>%
      addControl(HTML(paste0(
        "<div style='", STYLE_LEGENDE, "'>",
        "<span style='", STYLE_LEGENDE_TITRE, "'>Localisation</span>",
        "<span style='color:#1F4E79;'>&#9679;</span> exacte<br/>",
        "<span style='color:#C7102C;'>&#9679;</span> approximative ",
        "(centre de la commune)</div>")), position = "topright") %>%
      addControl(HTML(legende_cercles(d$effectifs)), position = "bottomright")
  })

  # --- Évolution (total, femmes, hommes) ---
  p_fiche_evol <- reactive({
    d <- fiche_all() %>% filtre_periode(input$per_fiche) %>%
      group_by(rentree) %>%
      summarise(Total  = sum(effectifs, na.rm = TRUE),
                Femmes = sum(femmes,    na.rm = TRUE),
                Hommes = sum(hommes,    na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_longer(c(Total, Femmes, Hommes),
                          names_to = "serie", values_to = "eff") %>%
      mutate(serie = factor(serie, levels = c("Total", "Femmes", "Hommes"))) %>%
      ajouter_rang()
    ggplot(d, aes(rentree, eff, colour = serie)) +
      geom_line(linewidth = 1.2) + geom_point(size = 2.6) +
      couche_etiquettes(4.6) +
      scale_colour_manual(values = col_evol) +
      scale_x_continuous(breaks = sort(unique(d$rentree))) +
      scale_y_continuous(labels = fmt_eff, breaks = breaks_effectifs(),
                         expand = expansion(mult = c(0.26, 0.22))) +
      labs(title = NULL, subtitle = txt_periode(input$per_fiche),
           x = NULL, y = "Étudiants inscrits", colour = NULL) +
      theme_obs
  })
  output$g_fiche_evol <- renderPlot(p_fiche_evol())

  # --- Répartition par niveau et par sexe (année sélectionnée) ---
  p_fiche_niveau <- reactive({
    d <- fiche_annee_data() %>%
      group_by(degre) %>%
      summarise(Femmes = sum(femmes, na.rm = TRUE),
                Hommes = sum(hommes, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_longer(c(Femmes, Hommes), names_to = "Sexe", values_to = "eff") %>%
      group_by(degre) %>% mutate(part = eff / sum(eff)) %>% ungroup() %>%
      filter(!is.na(eff)) %>%
      mutate(degre = factor(degre, levels = rev(niveaux_ordre)))
    validate(need(nrow(d) > 0, "Aucune donnée pour cette rentrée."))
    marge <- marge_valeurs(
      paste0(fmt_eff(d$eff), " (", percent(d$part, accuracy = 0.1), ")"),
      envelopper(levels(d$degre), LARGEUR_ETIQ_NIVEAU), largeur_image = 6.6)
    ggplot(d, aes(degre, eff, fill = Sexe)) +
      # Barres plus fines que le pas du groupe : cela dégage un blanc entre les
      # deux sexes et, surtout, entre les niveaux — le graphique était compact
      # au point d'être pénible à lire dans l'export Word.
      geom_col(position = position_dodge(width = 0.86), width = 0.62) +
      geom_text(aes(label = paste0(fmt_eff(eff), " (", percent(part, accuracy = 0.1), ")")),
                position = position_dodge(width = 0.86), hjust = -0.05, size = 3.8) +
      coord_flip() +
      # Les libellés de niveau sont longs (« Inférieur ou égal au baccalauréat »,
      # « Niveau non renseigné (source MESR) ») : sans repli, ils réduisaient le
      # cadre de tracé au point de couper les valeurs.
      scale_x_discrete(labels = function(l) envelopper(l, LARGEUR_ETIQ_NIVEAU)) +
      scale_fill_manual(values = col_sexe) +
      scale_y_continuous(labels = fmt_eff, expand = expansion(mult = c(0, marge))) +
      labs(title = NULL,
           subtitle = paste0("Rentrée ", input$rentree, mention_niveau(d$degre)),
           x = NULL, y = "Étudiants inscrits", fill = NULL) +
      theme_obs
  })
  output$g_fiche_niveau <- renderPlot(p_fiche_niveau())

  # --- Poids des composantes (année sélectionnée) ---
  p_fiche_comp <- reactive({
    tot_comp <- fiche_annee_data() %>%
      group_by(panneau) %>%
      summarise(eff    = sum(effectifs, na.rm = TRUE),
                Femmes = sum(femmes,    na.rm = TRUE),
                Hommes = sum(hommes,    na.rm = TRUE), .groups = "drop") %>%
      filter(eff > 0) %>%
      mutate(part = eff / sum(eff))
    validate(need(nrow(tot_comp) > 0, "Aucune donnée pour cette rentrée."))

    # Ordre commun aux deux couches : la barre empilée et son étiquette de total
    # doivent partager exactement les mêmes niveaux de facteur.
    ordre <- tot_comp$panneau[order(tot_comp$eff)]
    tot_comp <- tot_comp %>% mutate(panneau = factor(panneau, levels = ordre))
    d <- tot_comp %>%
      tidyr::pivot_longer(c(Femmes, Hommes), names_to = "Sexe", values_to = "n") %>%
      mutate(Sexe = factor(Sexe, levels = c("Femmes", "Hommes")),
             part_sexe = ifelse(eff > 0, n / eff, NA_real_))

    # Le pourcentage n'est inscrit dans un segment que s'il tient dedans (au
    # moins 7 % de la plus grande barre). Aucune donnée n'est écartée : les
    # effectifs exacts par sexe figurent dans le tableau « Détail par
    # composante », et la longueur du segment reste toujours proportionnelle.
    seuil <- 0.07 * max(tot_comp$eff)

    reg   <- reglage_comp()
    etiq  <- paste0(fmt_eff(tot_comp$eff), " (",
                    percent(tot_comp$part, accuracy = 0.1), ")")
    noms  <- envelopper(levels(tot_comp$panneau), reg$largeur)
    marge <- marge_valeurs(etiq, noms, largeur_image = 6.6, taille_axe = reg$taille)

    ggplot(d, aes(panneau, n, fill = Sexe)) +
      geom_col(width = 0.75) +
      geom_text(aes(label = ifelse(n >= seuil & !is.na(part_sexe),
                                   percent(part_sexe, accuracy = 1), "")),
                position = position_stack(vjust = 0.5),
                colour = "white", fontface = "bold", size = 3.1) +
      geom_text(data = tot_comp, inherit.aes = FALSE,
                aes(panneau, eff,
                    label = paste0(fmt_eff(eff), " (",
                                   percent(part, accuracy = 0.1), ")")),
                hjust = -0.05, size = 3.5) +
      coord_flip() +
      scale_x_discrete(labels = function(l) envelopper(l, reg$largeur)) +
      scale_fill_manual(values = col_sexe) +
      scale_y_continuous(labels = fmt_eff, expand = expansion(mult = c(0, marge))) +
      labs(title = NULL, subtitle = paste0("Rentrée ", input$rentree),
           x = NULL, y = "Étudiants inscrits", fill = NULL) +
      theme_obs +
      theme(axis.text.y = element_text(size = reg$taille, lineheight = 0.95))
  })
  output$g_fiche_comp <- renderPlot(p_fiche_comp())

  # Hauteur ajustée au nombre de composantes réellement affichées : un
  # établissement à 34 composantes ne peut pas tenir dans un cadre fixe.
  n_comp_fiche <- reactive({
    d <- fiche_annee_data()
    length(unique(d$panneau[!is.na(d$effectifs) & d$effectifs > 0]))
  })

  # Nombre de lignes de la plus longue étiquette d'axe. Replier les noms sur
  # deux ou trois lignes élargit le cadre de tracé, mais impose de donner plus
  # de hauteur à chaque barre : sans cela les noms se chevaucheraient.
  lignes_etiq <- function(labels, largeur) {
    if (!length(labels)) return(1L)
    max(vapply(strsplit(envelopper(labels, largeur), "\n", fixed = TRUE),
               length, integer(1)))
  }

  # Réglage du repli et de la police des noms de composantes.
  # Il n'existe pas de réglage unique : la hauteur de l'image est plafonnée par
  # le format A4, si bien qu'un établissement à 25 composantes ne peut pas
  # offrir à chaque nom les trois lignes qu'un établissement à 6 composantes
  # accueille sans peine. On choisit donc, parmi des combinaisons classées de la
  # plus lisible à la plus compacte, la PREMIÈRE qui tient dans la hauteur
  # réellement disponible. Le repli en découle : autant de caractères qu'il en
  # faut pour que le nom le plus long tienne dans le nombre de lignes retenu.
  HAUTEUR_IMAGE_MAX <- 9.0   # pouces, contrainte de la page A4
  MARGES_GRAPHIQUE  <- 1.8   # titre, légende, axe des abscisses

  reglage_comp <- reactive({
    d <- fiche_annee_data()
    lab <- unique(as.character(d$panneau[!is.na(d$effectifs) & d$effectifs > 0]))
    if (!length(lab)) return(list(largeur = 30L, taille = 10.5, lignes = 1L))
    n <- length(lab); nmax <- max(nchar(lab))
    dispo <- (HAUTEUR_IMAGE_MAX - MARGES_GRAPHIQUE) / n
    combinaisons <- list(c(3, 10.5), c(2, 10.5), c(3, 9), c(2, 9),
                         c(3, 8), c(2, 8), c(2, 7.5), c(1, 8))
    for (cfg in combinaisons) {
      lignes <- cfg[1]; taille <- cfg[2]
      if (lignes * taille / 72 * 0.95 + 0.03 <= dispo) {
        largeur <- max(18L, as.integer(ceiling(nmax / lignes)) + 1L)
        return(list(largeur = largeur, taille = taille,
                    lignes = lignes_etiq(lab, largeur)))
      }
    }
    largeur <- max(18L, as.integer(ceiling(nmax / 2)) + 1L)
    list(largeur = largeur, taille = 7, lignes = lignes_etiq(lab, largeur))
  })
  n_lignes_comp <- reactive(reglage_comp()$lignes)

  # Hauteur strictement nécessaire à une barre : la place de son étiquette sur
  # n lignes, plus une respiration. Sert à l'écran comme à l'export.
  hauteur_barre_comp <- reactive({
    reglage_comp()$lignes * reglage_comp()$taille / 72 * 0.95 + 0.03
  })
  output$ui_g_fiche_comp <- renderUI({
    plotOutput("g_fiche_comp",
               height = paste0(round(max(400, 96 * (hauteur_barre_comp() *
                                 max(1, n_comp_fiche()) + MARGES_GRAPHIQUE))), "px"))
  })

  # --- Tableau détaillé des composantes ---
  output$fiche_table <- renderDT({
    d <- fiche_annee_data() %>%
      group_by(uai_comp, Composante = nom_comp, Commune = commune,
               `Niveau` = degre) %>%
      summarise(Effectifs = sum(effectifs, na.rm = TRUE),
                Femmes    = sum(femmes,    na.rm = TRUE),
                Hommes    = sum(hommes,    na.rm = TRUE), .groups = "drop") %>%
      select(-uai_comp) %>%
      arrange(Composante, factor(`Niveau`, levels = niveaux_ordre))
    datatable(d, filter = "top", rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE))
  })

  # --- Carte statique (pour l'export Word) : fond souverain, sans tuiles web ---
  #  Cadre FIXE sur toute la Normandie (lisible quel que soit l'établissement,
  #  y compris mono-site). Mêmes coordonnées que la carte interactive
  #  (GPS MESR -> propagation -> annuaire -> centroïde) ; les points issus du
  #  centroïde communal sont signalés par un cerclage rouge.
  p_fiche_carte <- reactive({
    d <- fiche_annee_data() %>%
      group_by(uai_comp, nom_comp, commune, secteur, latitude, longitude, geo_source) %>%
      summarise(effectifs = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      filter(effectifs > 0) %>%
      mutate(Localisation = ifelse(
        !is.na(geo_source) & grepl("centro|approx", geo_source),
        "Approximative (centre de la commune)", "Exacte"))
    validate(need(nrow(d) > 0, "Aucune composante localisée."))

    g <- ggplot() +
      # 1. Cantons : trame fine, en fond
      geom_sf(data = geo_cantons, fill = "#F7F3E9", colour = "#E4DCCB",
              linewidth = 0.15) +
      # 2. Arrondissements : limites intermédiaires
      geom_sf(data = geo_arr, fill = NA, colour = "#CDBFA6", linewidth = 0.35)
    # 3. Départements : limites structurantes
    if (!is.null(fond_dep))
      g <- g + geom_sf(data = fond_dep, fill = NA, colour = "#8C7A5E",
                       linewidth = 0.7)

    g +
      geom_point(data = d,
                 aes(longitude, latitude, size = effectifs,
                     colour = Localisation, fill = secteur),
                 shape = 21, alpha = 0.6, stroke = 1.2) +
      scale_size_area(max_size = 15, labels = fmt_eff, name = "Étudiants") +
      scale_fill_manual(values = col_secteur, name = "Secteur",
                        guide = guide_legend(
                          override.aes = list(size = 5, colour = "grey40"))) +
      scale_colour_manual(
        values = c("Exacte" = "#1F4E79",
                   "Approximative (centre de la commune)" = "#C7102C"),
        name = "Localisation",
        guide = guide_legend(override.aes = list(size = 5))) +
      coord_sf(xlim = cadre_x, ylim = cadre_y, expand = FALSE) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.background = element_rect(fill = "#DCEAF2", colour = NA),  # mer
            panel.border = element_rect(fill = NA, colour = "#B9C6CF",
                                        linewidth = 0.5),
            panel.grid = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank(),
            legend.position = "right", legend.key = element_blank(),
            legend.title = element_text(face = "bold", size = 10),
            legend.text = element_text(size = 9))
  })

  # --- Export de la fiche au format Word (charte Normandie) ---
  output$export_fiche <- downloadHandler(
    filename = function()
      paste0("Fiche_", gsub("[^A-Za-z0-9]+", "_", input$fiche_etab), "_",
             input$rentree, ".docx"),
    content = function(file) {
      d <- fiche_annee_data()
      if (nrow(d) == 0) {
        doc <- read_docx() %>%
          body_add_fpar(fpar(ftext(input$fiche_etab,
            fp_text(font.family = "Arial", font.size = 16, bold = TRUE,
                    color = "#003F7D")))) %>%
          body_add_fpar(fpar(ftext(paste0(
            "Aucun effectif pour cet établissement à la rentrée ",
            input$rentree, ". Choisissez une autre rentrée."),
            fp_text(font.family = "Arial", font.size = 11, color = "#C7102C"))))
        print(doc, target = file)
        return(invisible(NULL))
      }
      cr  <- croissance_10ans()
      tot <- sum(d$effectifs, na.rm = TRUE)
      f   <- sum(d$femmes,    na.rm = TRUE)
      h   <- sum(d$hommes,    na.rm = TRUE)

      # Images des graphiques (fichiers temporaires)
      img <- function(p, l, ht) {
        tmp <- tempfile(fileext = ".png")
        ggsave(tmp, plot = p, width = l, height = ht, dpi = 150, bg = "white")
        tmp
      }
      i_carte  <- img(p_fiche_carte(),  7.2, 4.0)
      i_evol   <- img(p_fiche_evol(),   6.6, 3.4)
      # Hauteurs proportionnelles au nombre de barres, plafonnées pour tenir
      # dans une page A4. Un cadre fixe écrasait les graphiques dès que
      # l'établissement comptait beaucoup de composantes ou de niveaux.
      n_niv    <- n_distinct(d$degre)
      lg_niv   <- lignes_etiq(unique(as.character(d$degre)), LARGEUR_ETIQ_NIVEAU)
      h_niveau <- min(8.0, max(4.0, (0.52 + 0.16 * lg_niv) * max(1, n_niv) + 1.3))
      i_niveau <- img(p_fiche_niveau(), 6.6, h_niveau)
      # Hauteur proportionnelle au nombre de barres ET au nombre de lignes des
      # étiquettes : replier les noms plus tôt élargit le cadre, mais allonge
      # la colonne de gauche.
      h_comp   <- min(HAUTEUR_IMAGE_MAX, max(4.0, hauteur_barre_comp() *
                                    max(1, n_comp_fiche()) + MARGES_GRAPHIQUE))
      i_comp   <- img(p_fiche_comp(),   6.6, h_comp)


      t_cles <- data.frame(
        Indicateur = c("Étudiants inscrits", "dont femmes", "dont hommes",
                       "Composantes", "Communes d'implantation",
                       paste0("Croissance ", cr$an1, "-", cr$an2),
                       "Rythme annuel moyen"),
        Valeur = c(fmt_eff(tot),
                   paste0(fmt_eff(f), " (", percent(f / tot, accuracy = 0.1), ")"),
                   paste0(fmt_eff(h), " (", percent(h / tot, accuracy = 0.1), ")"),
                   as.character(n_distinct(d$panneau)),
                   as.character(n_distinct(d$commune)),
                   cr$txt_total, cr$txt_tcam),
        stringsAsFactors = FALSE, check.names = FALSE
      )

      t_comp <- fiche_annee_data() %>%
        group_by(uai_comp, Composante = nom_comp, Commune = commune) %>%
        summarise(Effectifs = sum(effectifs, na.rm = TRUE),
                  Femmes    = sum(femmes,    na.rm = TRUE),
                  Hommes    = sum(hommes,    na.rm = TRUE), .groups = "drop") %>%
        select(-uai_comp) %>%
        arrange(desc(Effectifs)) %>%
        # Ligne de total, calculée sur les mêmes données que les chiffres clés :
        # les deux tableaux du document ne peuvent donc pas diverger.
        bind_rows(data.frame(Composante = "TOTAL",
                             Commune    = paste0(n_distinct(d$panneau), " composantes"),
                             Effectifs  = tot, Femmes = f, Hommes = h,
                             stringsAsFactors = FALSE)) %>%
        mutate(across(c(Effectifs, Femmes, Hommes), fmt_eff)) %>%
        as.data.frame()

      tp <- function(t, sz = 11, bold = FALSE, col = "black", it = FALSE)
        ftext(t, fp_text(font.family = "Arial", font.size = sz, bold = bold,
                         color = col, italic = it))
      titre <- function(doc, t) body_add_fpar(doc, fpar(
        tp(t, 13, TRUE, "#003F7D"), fp_p = fp_par(padding.top = 12, padding.bottom = 4)))

      doc <- read_docx()

      # ---- En-tête, sur la première page uniquement --------------------------
      # officer ne sait pas composer une image et du texte côte à côte sans
      # flextable, impossible à installer ici : le logo occupe donc sa propre
      # ligne, au-dessus des mentions. Le bloc n'étant écrit qu'une fois en tête
      # de corps, il n'apparaît que sur la première page.
      logo_doc <- if (!is.na(logo_src)) file.path("www", logo_src) else NA_character_
      if (!is.na(logo_doc) && file.exists(logo_doc) &&
          grepl("(?i)\\.(png|jpe?g)$", logo_doc)) {
        px <- dimensions_image(logo_doc)
        l_logo <- 1.9
        # Ratio de repli si le fichier n'a pas pu être lu : le logo est alors
        # inséré dans une proportion plausible plutôt que d'être omis.
        h_logo <- if (is.null(px)) round(l_logo * 0.38, 2)
                  else round(l_logo * px$h / px$l, 2)
        doc <- tryCatch(body_add_img(doc, logo_doc, width = l_logo, height = h_logo),
                        error = function(e) doc)
      }
      doc <- body_add_fpar(doc, fpar(tp(MENTION_DEESTRI, 9, TRUE, "#003F7D")))
      doc <- body_add_fpar(doc, fpar(
        tp(SOURCES_TXT, 7.5, it = TRUE, col = "#555555"),
        fp_p = fp_par(border.bottom = fp_border(color = "#003F7D", width = 1),
                      padding.bottom = 8)))

      doc <- body_add_fpar(doc, fpar(tp(input$fiche_etab, 18, TRUE, "#003F7D"),
                                     fp_p = fp_par(padding.top = 10)))
      doc <- body_add_fpar(doc, fpar(
          tp(paste0("Fiche « effectifs étudiants » — rentrée ", input$rentree),
             11, it = TRUE, col = "#555555"),
          fp_p = fp_par(border.bottom = fp_border(color = "#C7102C", width = 2),
                        padding.bottom = 6)))

      doc <- titre(doc, "Chiffres clés")
      doc <- body_add_table(doc, t_cles, style = "table_template")

      doc <- titre(doc, "Localisation des composantes")
      doc <- body_add_img(doc, i_carte, width = 6.4, height = 3.56)

      doc <- titre(doc, "Évolution des effectifs")
      doc <- body_add_img(doc, i_evol, width = 6.4, height = 3.3)

      doc <- titre(doc, "Répartition par niveau et par sexe")
      doc <- body_add_img(doc, i_niveau, width = 6.4,
                          height = round(6.4 / 6.6 * h_niveau, 2))

      doc <- titre(doc, "Poids des composantes")
      doc <- body_add_img(doc, i_comp, width = 6.4,
                          height = round(6.4 / 6.6 * h_comp, 2))

      doc <- titre(doc, paste0("Détail par composante — rentrée ", input$rentree))
      doc <- body_add_table(doc, t_comp, style = "table_template")

      # Les sources figurent désormais dans l'en-tête : le pied ne porte plus
      # que le rattachement et la date de production du document.
      doc <- body_add_fpar(doc, fpar(tp(paste0(
        "Observatoire ESRI Normandie — Région Normandie. Document produit le ",
        format(Sys.Date(), "%d/%m/%Y"), "."),
        8, it = TRUE, col = "#555555"),
        fp_p = fp_par(padding.top = 10,
                      border.top = fp_border(color = "#CCCCCC", width = 1))))

      print(doc, target = file)
      unlink(c(i_carte, i_evol, i_niveau, i_comp))
    }
  )

  # ===========================================================================
  #  COMPARATEUR DE DEUX ÉTABLISSEMENTS
  # ===========================================================================
  comp_data <- reactive({
    req(input$comp_a, input$comp_b)
    validate(need(input$comp_a != input$comp_b,
                  "Sélectionnez deux établissements différents."))
    etab_aggreg %>%
      filter(etab_affiche %in% c(input$comp_a, input$comp_b)) %>%
      mutate(Etab = factor(etab_affiche, levels = c(input$comp_a, input$comp_b)))
  })
  # Palette propre aux établissements comparés : volontairement distincte du
  # rouge/bleu de la charte, réservé aux sexes et aux secteurs.
  col_comp <- reactive(setNames(c("#00807A", "#E07B00"), c(input$comp_a, input$comp_b)))

  output$comp_titre_cles <- renderUI({
    tagList(
      tags$h4(paste0("Chiffres clés comparés — rentrée ", input$rentree),
              style = "color:#003F7D; font-weight:700; margin-top:6px; margin-bottom:2px;"),
      tags$div(style = "font-size:12px; color:#666; margin-bottom:8px;",
               "Sauf la ligne « Croissance », calculée sur l'ensemble de la période disponible.")
    )
  })

  # Tableau comparatif (sert à l'affichage écran ET à l'export PDF)
  comp_resume <- reactive({
    d <- comp_data()
    resume <- function(nom) {
      x  <- d %>% filter(etab_affiche == nom)
      xa <- x %>% filter(rentree == input$rentree)
      s  <- x %>% group_by(rentree) %>%
        summarise(e = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
        filter(e > 0) %>% arrange(rentree)
      tot <- sum(xa$effectifs, na.rm = TRUE)
      fm  <- sum(xa$femmes,    na.rm = TRUE)
      croiss <- if (nrow(s) >= 2) {
        v <- (s$e[nrow(s)] - s$e[1]) / s$e[1]
        paste0(if (v >= 0) "+" else "", percent(v, accuracy = 0.1),
               " (", min(s$rentree), "-", max(s$rentree), ")")
      } else "—"
      tibble(
        Indicateur = c("Étudiants inscrits", "Part de femmes", "Composantes",
                       "Communes", "Secteur", "Catégorie", "Croissance sur la période"),
        Valeur = c(fmt_eff(tot),
                   if (tot > 0) percent(fm / tot, accuracy = 0.1) else "—",
                   as.character(n_distinct(xa$uai_comp)),
                   as.character(n_distinct(xa$commune)),
                   paste(unique(xa$secteur), collapse = ", "),
                   paste(unique(xa$categorie), collapse = ", "),
                   croiss)
      ) %>% setNames(c("Indicateur", nom))
    }
    full_join(resume(input$comp_a), resume(input$comp_b), by = "Indicateur")
  })

  output$comp_table <- renderDT({
    datatable(comp_resume(), rownames = FALSE,
              options = list(dom = "t", pageLength = 10, ordering = FALSE))
  })

  p_comp_evol <- reactive({
    d <- comp_data() %>% filtre_periode(input$per_compar) %>%
      group_by(Etab, rentree) %>%
      summarise(eff = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      ajouter_rang()
    ggplot(d, aes(rentree, eff, colour = Etab)) +
      geom_line(linewidth = 1.2) + geom_point(size = 2.6) +
      couche_etiquettes(4.6) +
      scale_colour_manual(values = col_comp()) +
      scale_x_continuous(breaks = sort(unique(d$rentree))) +
      scale_y_continuous(labels = fmt_eff, breaks = breaks_effectifs(),
                         expand = expansion(mult = c(0.26, 0.22))) +
      labs(title = NULL, subtitle = txt_periode(input$per_compar),
           x = NULL, y = "Étudiants inscrits", colour = NULL) +
      theme_obs
  })

  p_comp_niveau <- reactive({
    d <- comp_data() %>% filter(rentree == input$rentree) %>%
      group_by(Etab, degre) %>%
      summarise(eff = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      group_by(Etab) %>% mutate(part = eff / sum(eff)) %>% ungroup() %>%
      mutate(degre = factor(degre, levels = rev(niveaux_ordre)))
    validate(need(nrow(d) > 0, "Aucune donnée pour cette rentrée."))
    ggplot(d, aes(degre, part, fill = Etab)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      geom_text(aes(label = percent(part, accuracy = 0.1)),
                position = position_dodge(width = 0.8), hjust = -0.05, size = 4.2) +
      coord_flip() +
      scale_fill_manual(values = col_comp()) +
      scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.18))) +
      labs(title = NULL,
           subtitle = paste0("Rentrée ", input$rentree, mention_niveau(d$degre)),
           x = NULL, y = "Part des inscrits", fill = NULL) +
      theme_obs
  })

  p_comp_sexe <- reactive({
    d <- comp_data() %>% filter(rentree == input$rentree) %>%
      group_by(Etab) %>%
      summarise(Femmes = sum(femmes, na.rm = TRUE),
                Hommes = sum(hommes, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_longer(c(Femmes, Hommes), names_to = "Sexe", values_to = "eff") %>%
      group_by(Etab) %>% mutate(part = eff / sum(eff)) %>% ungroup()
    validate(need(nrow(d) > 0, "Aucune donnée pour cette rentrée."))
    ggplot(d, aes(Etab, eff, fill = Sexe)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(aes(label = paste0(fmt_eff(eff), "\n(", percent(part, accuracy = 0.1), ")")),
                position = position_dodge(width = 0.8), vjust = -0.3, size = 4.4) +
      scale_fill_manual(values = col_sexe) +
      scale_y_continuous(labels = fmt_eff, expand = expansion(mult = c(0, 0.20))) +
      labs(title = NULL, subtitle = paste0("Rentrée ", input$rentree),
           x = NULL, y = "Étudiants inscrits", fill = NULL) +
      theme_obs +
      theme(axis.text.x = element_text(size = 11))
  })

  output$g_comp_evol   <- renderPlot(p_comp_evol())
  output$g_comp_niveau <- renderPlot(p_comp_niveau())
  output$g_comp_sexe   <- renderPlot(p_comp_sexe())

  # Le tableau comparatif redessiné en graphique, pour pouvoir figurer dans le PDF
  p_comp_tableau <- reactive({
    t <- comp_resume()
    cols <- names(t)
    d <- tibble(
      ligne = rep(seq_len(nrow(t)), 3),
      col   = rep(1:3, each = nrow(t)),
      texte = c(t[[1]], t[[2]], t[[3]])
    )
    ent <- tibble(col = 1:3, texte = c("Indicateur", cols[2], cols[3]))
    ggplot() +
      geom_rect(data = ent, aes(xmin = col - 0.5, xmax = col + 0.5,
                                ymin = 0.5, ymax = 1.4),
                fill = "#003F7D") +
      geom_text(data = ent, aes(col, 0.95, label = texte),
                colour = "white", fontface = "bold", size = 4, lineheight = 0.9) +
      geom_rect(data = subset(d, ligne %% 2 == 0),
                aes(xmin = col - 0.5, xmax = col + 0.5,
                    ymin = ligne + 0.9, ymax = ligne + 1.9),
                fill = "#F2F5F9") +
      geom_text(data = d, aes(col, ligne + 1.4, label = texte,
                              fontface = ifelse(col == 1, "bold", "plain")),
                size = 4, colour = "#222222") +
      scale_y_reverse() +
      scale_x_continuous(limits = c(0.5, 3.5)) +
      labs(title = "Chiffres clés comparés",
           subtitle = paste0("Rentrée ", input$rentree,
                             " (sauf la croissance, calculée sur toute la période)")) +
      theme_void(base_size = 13) +
      theme(plot.title = element_text(face = "bold", colour = "#C7102C", size = 16),
            plot.subtitle = element_text(size = 11, colour = "#555555"),
            plot.margin = margin(10, 10, 10, 10))
  })

  # ---- Export du comparateur en PDF -----------------------------------------
  output$export_comparateur <- downloadHandler(
    filename = function() {
      net <- function(x) gsub("[^A-Za-z0-9]+", "_", x)
      paste0("Comparateur_", net(input$comp_a), "_vs_", net(input$comp_b),
             "_", input$rentree, ".pdf")
    },
    content = function(file) {
      # cairo_pdf gère correctement les accents ; repli sur pdf() si indisponible
      ouvrir <- function(f) tryCatch(
        grDevices::cairo_pdf(f, width = 11.69, height = 8.27, onefile = TRUE),
        error = function(e) grDevices::pdf(f, width = 11.69, height = 8.27,
                                           encoding = "ISOLatin1"))
      ouvrir(file)
      on.exit(grDevices::dev.off(), add = TRUE)

      tryCatch({
        print(p_comp_tableau())
        print(p_comp_evol()   + labs(title = "Évolution comparée des effectifs"))
        print(p_comp_niveau() + labs(title = "Structure par niveau (en %)"))
        print(p_comp_sexe()   + labs(title = "Répartition par sexe"))
      }, error = function(e) {
        print(graphique_message(paste(
          "Comparaison impossible.",
          "Sélectionnez deux établissements différents,",
          "présents à la rentrée choisie.", sep = "\n")))
      })
    }
  )

  # ---- Rendu des graphiques -------------------------------------------------
  output$g_niveau <- renderPlot(p_niveau())
  output$g_sexe <- renderPlot(p_sexe())
  output$g_secteur <- renderPlot(p_secteur())
  output$g_sec_niveau <- renderPlot(p_sec_niveau())
  output$g_cat <- renderPlot(p_cat())
  output$g_cat_sexe <- renderPlot(p_cat_sexe())
  output$g_cat_secteur <- renderPlot(p_cat_secteur())
  output$g_uu <- renderPlot(p_uu())
  output$g_evol <- renderPlot(p_evol())
  output$g_evol_secteur <- renderPlot(p_evol_secteur())
  output$g_evol_niveau  <- renderPlot(p_evol_niveau())
  output$g_evol_cat <- renderPlot(p_evol_cat())
  output$g_comp <- renderPlot(p_comp())

  # ---- Export du graphique affiché (PNG) ------------------------------------
  # Chaque entrée : l'objet ggplot, le nom de fichier, et les dimensions
  # d'export (en pouces) adaptées au contenu du graphique.
  graph_actif <- reactive({
    onglet <- input$onglet_graph
    if (is.null(onglet)) onglet <- "niveau"
    switch(onglet,
      "niveau"       = list(p = p_niveau(),       f = "niveau_etudes",        l = 12, h = 6),
      "sexe"         = list(p = p_sexe(),         f = "sexe_x_niveau",        l = 13, h = 6),
      "secteur"      = list(p = p_secteur(),      f = "secteur",              l = 11, h = 5),
      "sec_niveau"   = list(p = p_sec_niveau(),   f = "secteur_x_niveau",     l = 13, h = 6.5),
      "cat"          = list(p = p_cat(),          f = "categorie",            l = 13, h = 7),
      "cat_sexe"     = list(p = p_cat_sexe(),     f = "categorie_x_sexe",     l = 14, h = 9),
      "cat_secteur"  = list(p = p_cat_secteur(),  f = "categorie_x_secteur",  l = 14, h = 9),
      "uu"           = list(p = p_uu(),           f = "unites_urbaines",      l = 12, h = 18),
      "evol"         = list(p = p_evol(),         f = "evolution_sexe",       l = 13, h = 7),
      "evol_secteur" = list(p = p_evol_secteur(), f = "evolution_secteur",    l = 13, h = 7),
      "evol_niveau"  = list(p = p_evol_niveau(),  f = "evolution_niveau",     l = 14, h = 8.5),
      "evol_cat"     = list(p = p_evol_cat(),     f = "evolution_categorie",  l = 15, h = 14)
    )
  })

  output$export_graph <- downloadHandler(
    filename = function() {
      g <- graph_actif()
      paste0("Graphique_", g$f, "_", input$rentree, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      tryCatch({
        g <- graph_actif()
        ggsave(file, plot = g$p, width = g$l, height = g$h,
               dpi = 150, bg = "white", limitsize = FALSE)
      }, error = function(e) {
        ggsave(file, plot = graphique_message(
          "Aucune donnée à exporter pour la sélection en cours."),
          width = 9, height = 3, dpi = 150, bg = "white")
      })
    }
  )

  output$tableau_etab <- renderDT({
    d <- data_filtree() %>%
      # uai_etab dans le regroupement (puis retiré de l'affichage) : deux
      # établissements homonymes de la même commune resteraient deux lignes.
      group_by(uai_etab, Commune = commune, Établissement = nom_etab,
               Secteur = secteur, Catégorie = categorie) %>%
      summarise(Effectifs = sum(effectifs, na.rm = TRUE), .groups = "drop") %>%
      select(-uai_etab) %>%
      arrange(Commune, desc(Effectifs))
    datatable(
      d,
      filter = "top",
      options = list(pageLength = 25, scrollX = TRUE,
                     order = list(list(0, "asc"), list(4, "desc"))),
      rownames = FALSE
    ) %>%
      formatStyle("Secteur",
                  color = styleEqual(c("Public", "Privé"), c("#003F7D", "#C7102C")),
                  fontWeight = "bold")
  })

  output$tableau <- renderDT({
    datatable(
      data_filtree() %>%
        transmute(rentree, categorie, secteur, uai_etab, nom_etab, uai_comp, nom_comp,
                  commune, Canton_nom, Arrondissement_nom, degre,
                  effectifs, femmes, hommes, Localisation = geo_source) %>%
        arrange(desc(effectifs)),
      options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE
    )
  })

  output$export_map <- downloadHandler(
    filename = function() paste0("Carte_Etudiants_Normandie_", input$rentree, "_",
                                 Sys.Date(), ".html"),
    content = function(file) {
      # selfcontained = TRUE exige pandoc (fourni par RStudio). Repli automatique
      # en fichier non autoportant si pandoc est indisponible ; et message clair
      # si la sélection ne renvoie aucune donnée.
      tryCatch({
        m <- construire_carte(points_filtree(), cantons_filtree(), arr_filtree(),
                              opts_courantes(), vue_courante())
        tryCatch(saveWidget(m, file, selfcontained = TRUE),
                 error = function(e) saveWidget(m, file, selfcontained = FALSE))
      }, error = function(e) {
        writeLines(paste0(
          "<html><meta charset='utf-8'><body style=\"font-family:Arial\">",
          "<p style='color:#C7102C'>Aucune donnée à exporter pour la sélection ",
          "en cours (rentrée, secteur, type d'établissement).</p></body></html>"),
          file)
      })
    }
  )
}

shinyApp(ui, server)
