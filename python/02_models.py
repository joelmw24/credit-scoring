import joblib
import pandas as pd
from pathlib import Path
from sklearn.calibration import CalibratedClassifierCV
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.impute import SimpleImputer
from sklearn.metrics import average_precision_score
from sklearn.metrics import roc_auc_score
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder
from sklearn.tree import DecisionTreeClassifier


def load_clean_data():
    return pd.read_csv("python/outputs/data/credit_scoring_clean.csv")


def build_preprocessor(X):
    numeric_columns = X.select_dtypes(include=["number"]).columns.tolist()
    categorical_columns = X.select_dtypes(exclude=["number"]).columns.tolist()

    numeric_pipeline = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="median")),
        ]
    )

    categorical_pipeline = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="most_frequent")),
            ("encoder", OneHotEncoder(handle_unknown="ignore")),
        ]
    )

    return ColumnTransformer(
        transformers=[
            ("numeric", numeric_pipeline, numeric_columns),
            ("categorical", categorical_pipeline, categorical_columns),
        ]
    )


def build_calibrated_model(preprocessor, model):
    pipeline = Pipeline(
        steps=[
            ("preprocessor", preprocessor),
            ("model", model),
        ]
    )
    return CalibratedClassifierCV(estimator=pipeline, method="sigmoid", cv=2)


def discrimination_metrics(y_true, y_proba):
    return {
        "roc_auc": roc_auc_score(y_true, y_proba),
        "pr_auc": average_precision_score(y_true, y_proba),
    }


def evaluate_thresholds(y_true, y_proba):
    rows = []

    for threshold in [value / 100 for value in range(0, 51, 1)]:
        y_pred = (y_proba >= threshold).astype(int)

        tp = ((y_true == 1) & (y_pred == 1)).sum()
        tn = ((y_true == 0) & (y_pred == 0)).sum()
        fp = ((y_true == 0) & (y_pred == 1)).sum()
        fn = ((y_true == 1) & (y_pred == 0)).sum()

        precision = tp / (tp + fp) if (tp + fp) != 0 else 0
        recall = tp / (tp + fn) if (tp + fn) != 0 else 0
        acceptance_rate = (tn + fn) / len(y_true)
        bad_rate_accepted = fn / (tn + fn) if (tn + fn) != 0 else 0
        captured_defaults = tp / (tp + fn) if (tp + fn) != 0 else 0

        rows.append(
            {
                "threshold": threshold,
                "score_cutoff": round((1 - threshold) * 1000),
                "precision": precision,
                "recall": recall,
                "acceptance_rate": acceptance_rate,
                "bad_rate_accepted": bad_rate_accepted,
                "captured_defaults": captured_defaults,
            }
        )

    return pd.DataFrame(rows)


def choose_best_threshold(threshold_df, default_rate):
    valid_rows = threshold_df[
        (threshold_df["acceptance_rate"] >= 0.60)
        & (threshold_df["bad_rate_accepted"] <= default_rate)
    ]

    if valid_rows.empty:
        valid_rows = threshold_df.copy()

    valid_rows = valid_rows.sort_values(
        by=["captured_defaults", "bad_rate_accepted", "acceptance_rate"],
        ascending=[False, True, False],
    )

    return valid_rows.iloc[0]


def main():
    Path("python/outputs/reports").mkdir(parents=True, exist_ok=True)
    Path("python/outputs/models").mkdir(parents=True, exist_ok=True)

    df = load_clean_data()

    X = df.drop(columns=["default"])
    y = df["default"]

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.3, random_state=123, stratify=y
    )

    preprocessor = build_preprocessor(X_train)

    models = [
        (
            "Decision Tree",
            DecisionTreeClassifier(max_depth=5, random_state=123, class_weight="balanced"),
        ),
        (
            "Random Forest",
            RandomForestClassifier(
                n_estimators=40,
                max_depth=8,
                random_state=123,
                class_weight="balanced",
                n_jobs=-1,
            ),
        ),
        (
            "Gradient Boosting",
            GradientBoostingClassifier(n_estimators=40, learning_rate=0.1, random_state=123),
        ),
    ]

    results = []
    default_rate = y_train.mean()

    for name, model in models:
        print(f"\nTraining {name}...")
        final_model = build_calibrated_model(preprocessor, model)
        final_model.fit(X_train, y_train)
        print(f"{name} trained.")

        y_proba = final_model.predict_proba(X_test)[:, 1]

        metrics = discrimination_metrics(y_test, y_proba)
        threshold_df = evaluate_thresholds(y_test, y_proba)
        best_row = choose_best_threshold(threshold_df, default_rate)

        results.append(
            {
                "model": name,
                "roc_auc": metrics["roc_auc"],
                "pr_auc": metrics["pr_auc"],
                "best_threshold": best_row["threshold"],
                "score_cutoff": best_row["score_cutoff"],
                "recall": best_row["recall"],
                "precision": best_row["precision"],
                "acceptance_rate": best_row["acceptance_rate"],
                "bad_rate_accepted": best_row["bad_rate_accepted"],
                "captured_defaults": best_row["captured_defaults"],
            }
        )

    results_df = pd.DataFrame(results)
    results_df = results_df.sort_values(
        by=["captured_defaults", "bad_rate_accepted", "pr_auc"],
        ascending=[False, True, False],
    )

    results_df.to_csv("python/outputs/reports/results.csv", index=False)

    best_model_name = results_df.iloc[0]["model"]

    for name, model in models:
        if name == best_model_name:
            print(f"\nTraining final best model: {name}...")
            best_model = build_calibrated_model(preprocessor, model)
            best_model.fit(X_train, y_train)

            bundle = {
                "model_name": name,
                "model": best_model,
                "best_threshold": float(results_df.iloc[0]["best_threshold"]),
                "score_cutoff": int(results_df.iloc[0]["score_cutoff"]),
                "feature_columns": X.columns.tolist(),
            }
            joblib.dump(bundle, "python/outputs/models/best_model.joblib")

    print(results_df)
    print("\nBest model:", best_model_name)
    print("Saved in python/outputs/reports/results.csv")
    print("Saved in python/outputs/models/best_model.joblib")


main()
