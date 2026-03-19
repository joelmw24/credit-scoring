# credit-scoring


Les scripts utiles sont maintenant :

- `python/01_prepare_data.py` : nettoyage des donnees
- `python/02_models.py` : comparaison des modeles avec validation croisee et calibration
- `python/03_score.py` : score sur 1000, seuil retenu et decisions d'acceptation/refus
- `python/scoring_utils.py` : fonctions pour eviter les repetitions

## Modeles utilises

- Arbre de decision
- Random Forest
- XGBoost
- LightGBM

## Resultats produits

- metriques classiques : accuracy, precision, recall, F1, ROC-AUC
- metriques de desequilibre : PR-AUC, balanced accuracy
- metriques metier : taux d'acceptation, taux de refus, taux de defaut accepte, taux de defaut refuse, defaults captes
- validation croisee 
- calibration des probabilites
- construction d'un score client
- proposition de seuil de decision 

## Installation

```bash
pip install -r requirements.txt
```

## Execution

```bash
python3 python/01_prepare_data.py
python3 python/02_models.py
python3 python/03_score.py
```

Tous les fichiers sont sauvegardes dans `python/outputs/`.
