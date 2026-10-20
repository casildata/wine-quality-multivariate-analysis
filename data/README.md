# Data

## Source

**Wine Quality** dataset, UCI Machine Learning Repository
<https://archive.ics.uci.edu/dataset/186/wine+quality>. Licensed under
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

> P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. *Modeling wine preferences by
> data mining from physicochemical properties.* Decision Support Systems, 47(4):547–553, 2009.
> <https://doi.org/10.1016/j.dss.2009.05.016>

## Files

| File | Rows | Notes |
|---|---|---|
| `raw/winequality-red.csv` | 1,599 | Red *vinho verde* samples |
| `raw/winequality-white.csv` | 4,898 | White *vinho verde* samples |

Both files are the exact CSVs from the UCI download
(<https://archive.ics.uci.edu/static/public/186/wine+quality.zip>, semicolon-separated).
`data/raw/SHA256SUMS` holds their SHA-256 checksums, and `run_all.R` stops before the
analysis if either file differs (it needs R 4.5+ or the `digest` package to check; otherwise
it warns and continues). To verify by hand:

```bash
cd data/raw && sha256sum -c SHA256SUMS
```

`.gitattributes` keeps Git from converting the files' line endings, so the checksums hold
on Windows, macOS and Linux.

An earlier copy in this repository had been re-saved with commas and with `alcohol` rounded
to 8 decimals in 40 rows (6 red, 34 white; largest change 3.3 × 10⁻⁹). Switching to the
UCI originals left every number in `docs/results.md` and every figure unchanged.

## Columns

| Column | Unit |
|---|---|
| fixed acidity | g(tartaric acid)/dm³ |
| volatile acidity | g(acetic acid)/dm³ |
| citric acid | g/dm³ |
| residual sugar | g/dm³ |
| chlorides | g(sodium chloride)/dm³ |
| free sulfur dioxide | mg/dm³ |
| total sulfur dioxide | mg/dm³ |
| density | g/cm³ |
| pH | — |
| sulphates | g(potassium sulphate)/dm³ |
| alcohol | % vol. |
| quality | 0–10, median of at least three expert sensory ratings |
