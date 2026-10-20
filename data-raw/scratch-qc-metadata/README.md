# Scratch: QC metadata tables for the conditional / aggregate / cross-table checks

Staging area for the metadata that drives the three new QC check families. Edit
here, then copy `table-metadata/` into the `marinegeo-metadata` repo. Nothing in
this directory is read by the package — `data-raw/` is in `.Rbuildignore`, and
the package only ever reads the `marinegeo-metadata` repo on `main`.

The tree mirrors the metadata repo exactly, so the move is a straight copy:

```
table-metadata/
  qc-conditions/qc_conditions.csv            NEW
  aggregate-rules/aggregate_rules.csv        NEW
  cross-table-rules/cross_table_rules.csv    NEW
  numeric-ranges/*.csv                       EXISTING + condition_id column
  categorical-values/*.csv                   EXISTING + condition_id column
```

All identifiers and column names below were taken from the live
`database_structure`, not invented. Every file here has been parsed and
dispatched through `qc_run()` / `qc_run_dataset()` to confirm the rules fire.

---

## Read this before copying anything

### 1. The two existing directories must be updated all-at-once

`readr::read_csv()` row-binds every CSV in a directory and **errors** if they do
not all have the same number of columns:

```
Files must all have 7 columns:
i File 2 has 8 columns.
```

So you cannot drop a single new file carrying `condition_id` into
`numeric-ranges/` or `categorical-values/`. Every CSV in the directory needs the
column at the same time. That is why the existing files are reproduced here in
full (fetched live, read as character so no value was reformatted on write-back)
with the new column already appended — copy all of them together.

### 2. A partial update fails *silently*

`.mg_get_registry_table()` wraps the fetch in `tryCatch(..., error = ~ NULL)`
and falls back to the copy bundled in `R/sysdata.rda`, emitting only a
`message()`. So if the column counts disagree mid-rollout, QC keeps running
against the **stale bundled rules** rather than erroring. It will look like it
worked. After copying, confirm the live tables are really being used:

```r
nrow(utl_mg_get_registry("qc_conditions"))                  # > 0
"condition_id" %in% names(utl_mg_get_registry("numeric_ranges"))   # TRUE
```

If either is wrong, the fetch is failing and you are seeing bundled data. A
`message()` about "using bundled fallback" during a QC run means the same thing.

The three NEW directories are a different case: until they exist on `main`,
`marinegeo_metadata[["qc_conditions"]]` is `NULL`, the rule set resolves empty
and the checks simply do not run. That is safe but quiet, so expect the new
checks to be absent from `checks_run` until the copy lands.

### 3. Rollout order

The checks are already merged and inert. Copy the three new directories and the
two updated ones in one commit on `marinegeo-metadata` `main` (the fetch is
pinned to `main`; there is no staging branch). No `R/sysdata.rda` rebuild is
needed — these tables are all directory-backed and fetched live.

---

## `qc_conditions.csv`

Predicate terms referenced by `condition_id` from the other rule tables. One row
is one term; **rows sharing a `condition_id` are AND-ed**.

| column | notes |
|---|---|
| `condition_id` | Referenced from the rule tables. Unique per `table_id`. |
| `table_id` | The table the predicate is evaluated against. |
| `column_name` | The column tested (var B). |
| `operator` | Closed set: `in`, `not_in`, `==`, `!=`, `>`, `>=`, `<`, `<=`, `is_na`, `not_na`, `matches`, plus the four taxonomy-aware operators below. |
| `value` | Right-hand side. `\|`-delimited for `in`/`not_in` and for the taxonomy operators; empty for `is_na`/`not_na`. |
| `description` | Surfaced in the `detail` column of the QC result. |

Notes on the prefilled rows:

- **`primary_points` is migration-critical, not an example.** It reproduces the
  `cover_type != "canopy taxa"` filter that was hard-coded in
  `qc_check_point_counts()`. Without it, canopy taxa get counted into the
  oyster substrate point total.
- `flowering_zostera` is the two-row AND example. Both rows carry the same
  `description` on purpose: only the **first** row's description is used for the
  result's `detail`, so a "term 1 of 2" style description would read oddly.
- `zostera_leaves` and `flowering_zostera` use `taxon_within Genus:Zostera`
  rather than a hand-written species list. **This is not cosmetic.** The list
  they previously carried named five `Zostera` species; the genus actually
  contains **six** registry taxa, and the one it missed —
  `Heterozostera nigricaulis` — does not begin with "Zostera", so no reviewer
  would have caught it by eye. Deriving the set also means a newly recorded
  `Zostera` species is covered with no edit here.
- `marine_seagrass` shows the functional-group form. Note the **required**
  `vegetation:` tree prefix.
- An operator outside the closed set warns and the rule is **not applied**
  (it never silently widens to all rows). There is deliberately no operator
  that evaluates R code: these CSVs are fetched over the network at runtime, so
  an `eval(parse(...))` operator would make this repo a remote code-execution
  path into every user's R session.
- `NA` in the tested column never matches, including under `not_in` and `!=`.
  Use `is_na` / `not_na` to target missingness.

### Taxonomy-aware operators

Rather than enumerate species, a condition can name a clade or a functional
group and let the registry supply the members.

| operator | `value` | true when |
|---|---|---|
| `taxon_within` / `taxon_not_within` | `Rank:Name` (`Genus:Zostera`, `Family:Zosteraceae`) or a full LSID | the row's taxon is at or below that node of `taxonomic_lookup` |
| `in_functional_group` / `not_in_functional_group` | `tree:Label` (`vegetation:Marine Seagrass`) | the row's taxon is at or below that node of a `functional_group_lookup` tree |

`column_name` is the column holding scientific names — normally
`scientific_name`.

**Check what a reference resolves to before relying on it:**

```r
utl_mg_expand_taxon_set("Genus:Zostera")
#> 6 rows, including Heterozostera nigricaulis

utl_mg_expand_taxon_set("vegetation:Marine Seagrass", hierarchy = "functional_group")
#> 28 rows
```

Things to know:

- **The `tree:` prefix is required** for functional groups. 540 of the 1,579
  labels occur in more than one tree (`Algae` is in both `oyster_composition`
  and `vegetation`), so a bare label warns and the rule is not applied.
- **A node counts as its own ancestor**, so `Species:Zostera marina` matches
  exactly that species.
- **`|` is a union**, and it is the only way to express OR: two rows sharing a
  `condition_id` are combined with AND. So
  `taxon_within  Genus:Zostera|Genus:Halodule` means either genus.
- **Ranks are normalised**, so `Phylum:Tracheophyta` matches rows stored as
  `"Phylum (Division)"`.
- **A row the hierarchy cannot place matches neither the positive nor the
  negative form.** That covers an unregistered name and one mapping to a
  `FUNCTIONAL:`/`UNDEFINED:` id (114 of the 1,696 registry entries have no
  taxonomic chain). A conditional rule should not fire on a row nobody could
  classify; unregistered names are reported separately by the lookup-values
  check.
- **Rank abbreviations resolve only when the stripped name is itself a registry
  entry.** `"Gastropoda sp."` becomes `Gastropoda`, which is in
  `observation_lookup`, so it falls inside `Class:Gastropoda`. `"Halodule sp."`
  becomes `Halodule`, which is **not** in `observation_lookup`, so it is
  unplaceable — and already flagged by the lookup-values check as an
  unrecognised name.
- **The matched set depends on registry state.** Unlike a frozen list, an
  offline run against an older bundled `taxonomic_lookup` can match a narrower
  set than a live one, and the fallback is only a `message()`. Confirm with
  `getOption("marinegeo.utils.live_registry")`.

## `aggregate_rules.csv`

Rules about a *group* of rows. This is where `qc_check_point_counts()` went.

| column | notes |
|---|---|
| `rule_id` | Appears in the result's `rule_id`. |
| `group_columns` | `\|`-delimited grouping key. |
| `value_column` | Column aggregated. |
| `aggregate_function` | `sum`, `mean`, `min`, `max`, `n`, `n_distinct`. |
| `comparison` | `==`, `!=`, `>`, `>=`, `<`, `<=` — read as "the aggregate must be _ the target". |
| `target_column` / `target_value` | **Exactly one.** Both or neither warns and skips the rule. `target_column` must be constant within a group. |
| `tolerance` | Numeric slack; empty means exact. Always *widens* what passes. |
| `condition_id` | Optional row filter applied **before** aggregating. |
| `severity` | `fail` or `warn`. |

Prefilled rows:

| rule | status |
|---|---|
| `oyster_substrate_point_total` | **Required** — preserves existing coverage |
| `seagrass_cover_point_total` | **Required** — preserves existing coverage |
| `fouling_cover_point_total` | New coverage (see below) |
| `seagrass_cover_percent_total` | **Illustrative — review or delete** |

The first two reproduce what the old check covered. Its dispatch gate fired on
any table declaring both `point_count` and `points_in_quadrat`, which was
exactly `oyster-substrate-composition-monitoring-v1` and
`seagrass-cover-monitoring-v1`. **If you drop either row, you lose a check that
is running today.**

`fouling_cover_point_total` is new. `fouling-cover-v1` has the structurally
identical rule under different names (`panel_id` / `points_in_grid`), which the
hard-coded function could not express, so fouling has never had this check.

### One deliberate behaviour change: the grouping key

The old function grouped by `site_name, transect, quadrat`, with **no date
component**. Two visits to the same quadrat on different dates were therefore
summed together into one total. The rules here group by `sample_event_id`
instead, which is part of the row identity (`uuid_identity`) for all three
tables and scopes the total to one sampling event.

This is a fix, but it is a change — if `sample_event_id` does not fully
distinguish repeat visits in your data, add `sample_collection_date` to
`group_columns` (it appears in `uuid_identity` alongside `sample_event_id`, which
suggests it may be needed). Worth confirming against real data before copying.

## `cross_table_rules.csv`

Referential integrity between a **pair** of tables. Keyed on
`left_table_id`/`right_table_id` rather than a single `table_id`, because a
relation belongs to the pair. Only read by `qc_run_dataset()` — `qc_run()` is
handed one table and cannot see these.

| column | notes |
|---|---|
| `relation_type` | `subset` (every left key must exist in right) or `equal_set` (must match both ways). |
| `group_columns` | `\|`-delimited columns shared by both tables, scoping the comparison. |
| `left_columns` / `right_columns` | `\|`-delimited, paired **positionally**, same count. |
| `left_condition_id` / `right_condition_id` | Optional; each resolved against its own table. |
| `severity` | `fail` or `warn`. |

`leaf_quadrat_in_cover` is the shape you described — quadrats recorded in one
table must be present in another for the same sample event. Keys are compared as
text, so a column stored `TINYINT` in one table and `INT` in another still
matches.

`oyster_quadrat_sets_match` is **illustrative — review or delete**; it asserts
that substrate and height quadrat sets are identical, which may not be true of
your protocol.

A rule whose counterpart table is not supplied to `qc_run_dataset()` is skipped,
not failed, and `qc_check_cross_table` is added to the result's
`checks_skipped` so a partial submission does not look fully checked.

## `numeric-ranges/` and `categorical-values/`

The live files, reproduced verbatim with one `condition_id` column appended
(empty on every pre-existing row, so **every existing rule keeps its current
meaning** — empty means "applies to all rows").

One row was **added**, to `seagrass_monitoring_numeric_ranges.csv`:

```csv
seagrass-leaf-monitoring-v1,leaf_length_mm,inclusive,,,1200,20,zostera_leaves
```

A `Zostera`-only band on `leaf_length_mm`, alongside the existing unconditional
`exclusive / min_fail 0` rule — both apply, so a value can breach one, the
other, or both, told apart by `rule_id`. It is deliberately **warn-only** (no
`max_fail`/`min_fail`) so it cannot escalate to a failure before you have
reviewed the bounds, which are a guess. Delete the line or set real bounds.

`categorical-values/` has the column added but **no example rows** — a
conditional vocabulary needs a real restricted-vocabulary case, which I did not
want to invent. To add one, repeat the `condition_id` on every permitted value
of the conditional set (that table is one row per allowed value).

### What will not work here

Do **not** add conditional rules to `database_structure` to make a column
conditionally required. `utl_mg_column_order()` derives canonical column order
from that table's **row order**, so a second row for a column corrupts column
ordering across the package. Conditional required-ness needs its own
`missing-value-rules/` table; that piece is designed but not built.
