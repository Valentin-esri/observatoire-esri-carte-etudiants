# Carte interactive des effectifs étudiants en Normandie

Application de consultation des effectifs d'étudiants inscrits dans les
établissements d'enseignement supérieur de Normandie, rentrées 2015 à 2024.

**Version de test** — diffusée pour recueillir des retours avant déploiement.

## Consulter l'application

L'adresse de consultation est indiquée dans l'onglet *Settings → Pages* du dépôt,
ou à la fin du journal du workflow « Publier l'application (Shinylive) ».

Trois choses à savoir avant d'ouvrir :

- **le premier chargement prend une à trois minutes** et télécharge plusieurs
  dizaines de mégaoctets — l'application embarque le moteur R lui-même, qui
  s'exécute dans votre navigateur ; les visites suivantes sont quasi
  instantanées grâce au cache ;
- **navigateur récent obligatoire** (Chrome, Edge ou Firefox à jour), sur poste
  fixe : l'expérience sur téléphone n'est pas exploitable ;
- **l'export PDF du comparateur rend mal les accents** — limitation connue de R
  compilé pour le navigateur, sans incidence sur les autres exports.

Aucune donnée ne transite par un serveur : les fichiers sont téléchargés par
votre navigateur et tous les calculs se font sur votre poste.

## Signaler une anomalie

L'application ne produit aucun journal côté serveur, puisqu'il n'y a pas de
serveur. Pour qu'une anomalie soit exploitable, merci de transmettre :

1. une **capture d'écran** ;
2. l'**établissement, la rentrée et les filtres** en cours ;
3. le contenu de la **console du navigateur** : touche `F12`, onglet *Console*.

## Contenu du dépôt

```
app/
  app.R                  application Shiny
  data_app/              données préparées (3 fichiers .rds)
  www/                   logo
.github/workflows/
  publier_shinylive.yml  publication automatique sur GitHub Pages
```

Les données sont produites en amont par le script `01_preparation_donnees.R`,
qui ne figure pas dans ce dépôt.

## Sources

- Effectifs étudiants — Ministère de l'Enseignement supérieur, de la Recherche
  et de l'Espace (MESRE), « Effectifs d'étudiants inscrits dans les
  établissements et les formations de l'enseignement supérieur — détail par
  établissements » (open data MESRE)
- Limites administratives — IGN, ADMIN-EXPRESS-COG-CARTO
- Codes géographiques — INSEE, Code officiel géographique (COG)

## Rattachement

Observatoire ESRI — Service Enseignement Supérieur, Recherche et Innovation —
DEESTRI (Économie, Enseignement Supérieur, Tourisme, Recherche et Innovation),
Région Normandie.
