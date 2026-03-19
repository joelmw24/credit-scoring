import pandas as pd
import os


def main():
    df = pd.read_csv("data/credit_scoring_v1.csv")
    df = df.rename(
        columns={
            "SeriousDlqin2yrs": "default",
            "RevolvingUtilizationOfUnsecuredLines": "revolving_utilization",
            "NumberOfTime30-59DaysPastDueNotWorse": "late_30_59_days",
            "DebtRatio": "debt_ratio",
            "MonthlyIncome": "monthly_income",
            "NumberOfOpenCreditLinesAndLoans": "open_credit_lines",
            "NumberOfTimes90DaysLate": "late_90_days",
            "NumberRealEstateLoansOrLines": "real_estate_loans",
            "NumberOfTime60-89DaysPastDueNotWorse": "late_60_89_days",
            "NumberOfDependents": "dependents",
        }
    )


    os.makedirs("python/outputs/data", exist_ok=True)
    os.makedirs("python/outputs/reports", exist_ok=True)

    # 6. Sauvegarder le fichier nettoye
    df.to_csv("python/outputs/data/credit_scoring_clean.csv", index=False)

    # 8. Afficher un petit resume
    print("Le fichier propre a ete enregistre dans python/outputs/data/")
    print("Taille du tableau :", df.shape)
    print("Valeurs manquantes restantes :")
    print(df.isna().sum())


main()
