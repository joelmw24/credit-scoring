import joblib
import pandas as pd
from pathlib import Path


def load_clean_data():
    return pd.read_csv("python/outputs/data/credit_scoring_clean.csv")


def evaluate_thresholds(y_true, y_proba):
    rows = []

    for threshold in [value / 100 for value in range(0, 51, 1)]:
        y_pred = (y_proba >= threshold).astype(int)

        tp = ((y_true == 1) & (y_pred == 1)).sum()
        tn = ((y_true == 0) & (y_pred == 0)).sum()
        fp = ((y_true == 0) & (y_pred == 1)).sum()
        fn = ((y_true == 1) & (y_pred == 0)).sum()

        acceptance_rate = (tn + fn) / len(y_true)
        refusal_rate = (tp + fp) / len(y_true)
        bad_rate_accepted = fn / (tn + fn) if (tn + fn) != 0 else 0
        bad_rate_refused = tp / (tp + fp) if (tp + fp) != 0 else 0
        captured_defaults = tp / (tp + fn) if (tp + fn) != 0 else 0

        rows.append(
            {
                "threshold": threshold,
                "score_cutoff": round((1 - threshold) * 1000),
                "acceptance_rate": acceptance_rate,
                "refusal_rate": refusal_rate,
                "bad_rate_accepted": bad_rate_accepted,
                "bad_rate_refused": bad_rate_refused,
                "captured_defaults": captured_defaults,
            }
        )

    return pd.DataFrame(rows)


def main():
    Path("python/outputs/data").mkdir(parents=True, exist_ok=True)
    Path("python/outputs/reports").mkdir(parents=True, exist_ok=True)

    df = load_clean_data()
    bundle = joblib.load("python/outputs/models/best_model.joblib")

    model = bundle["model"]
    model_name = bundle["model_name"]
    best_threshold = bundle["best_threshold"]
    score_cutoff = bundle["score_cutoff"]
    feature_columns = bundle["feature_columns"]

    X = df[feature_columns]
    y = df["default"]

    df["probability_default"] = model.predict_proba(X)[:, 1]
    df["score"] = ((1 - df["probability_default"]) * 1000).round().astype(int)
    df["decision"] = "accept"
    df.loc[df["probability_default"] >= best_threshold, "decision"] = "reject"

    df["risk_band"] = "Medium"
    df.loc[df["score"] >= max(score_cutoff + 100, 800), "risk_band"] = "Low"
    df.loc[df["score"] < score_cutoff, "risk_band"] = "High"

    threshold_df = evaluate_thresholds(y, df["probability_default"])
    selected_row = threshold_df[threshold_df["threshold"] == best_threshold].iloc[0]

    score_file = Path("python/outputs/data/credit_scoring_with_score.csv")
    threshold_file = Path("python/outputs/reports/threshold_business_metrics.csv")
    summary_file = Path("python/outputs/reports/score_summary.txt")

    df.to_csv(score_file, index=False)
    threshold_df.to_csv(threshold_file, index=False)

    with summary_file.open("w", encoding="utf-8") as file:
        file.write("Construction du score et des seuils\n\n")
        file.write(f"Modele retenu : {model_name}\n")
        file.write("Score utilise : score = (1 - probabilite_de_defaut) * 1000\n")
        file.write(f"Seuil conseille sur la probabilite : {best_threshold:.2f}\n")
        file.write(f"Score cutoff conseille : {score_cutoff}\n")
        file.write("\nMetriques au seuil retenu :\n")
        file.write(selected_row.to_string())
        file.write("\n\nRepartition des decisions :\n")
        file.write(df["decision"].value_counts(normalize=True).to_string())
        file.write("\n\nRepartition des bandes de risque :\n")
        file.write(df["risk_band"].value_counts(normalize=True).to_string())

    print(f"Seuils metier enregistres ici: {threshold_file}")
    print(f"Scores enregistres ici: {score_file}")
    print(f"Resume du score ici: {summary_file}")
    print(f"Seuil conseille: {best_threshold:.2f}")


main()
