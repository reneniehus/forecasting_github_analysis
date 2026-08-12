# The RespiCast weekly schedule — deadline, ensemble, website

What the hubs *say* they do (READMEs and `hub-config/tasks.json`), what they are *automated* to do
(GitHub Actions cron, all UTC), and what the git history shows they *actually* did. Sources:
`european-modelling-hubs/RespiCast-Covid19` and `.../RespiCast-SyndromicIndicators`.

## The week, end to end

| Step | When | Source |
|---|---|---|
| Truth data (ERVISS) published | **Friday** | README ("data is updated generally on Friday") |
| Truth import into the hub | **Friday 18:00 UTC** | `import_truth.yml` (`00 18 * * 5`) |
| Submission window opens | right after the truth data lands | README; `tasks.json` `submissions_due` = `origin_date −6 … 0` (Thu → Wed) |
| **Submission deadline** | **Wednesday 23:59 CET** (= 22:59 UTC in winter) | README, both hubs, identical wording |
| **Ensemble produced** | **Wednesday ~23:30–23:50 UTC** (≈ Thu 00:30–00:50 CET) | `check_intermediate_run.yml` `30 23 * * 3`; `forecast-pipeline.yml` `40 23 * * 3` (COVID) / `50 23 * * 3` (syndromic) |
| Website updated | immediately after, by **webhook** | `upload_baseline_ensemble_pipelined.yml` — step *"Trigger server-side updating and UI deploy"* |
| Published to users | **Thursday** | README ("published on the Hub website every Thursday") |
| Scoring run | **Thursday 00:15 UTC** | `scoring.yml` (`15 00 * * 4`) |

The two hubs are deliberately **staggered by 10 minutes** (COVID 23:40 UTC, syndromic 23:50 UTC).

## The ensemble is produced more than once a week

`check_intermediate_run.yml` also runs `00 10,18 * * 0,1,2,3,6` — **10:00 and 18:00 UTC on Sat, Sun,
Mon, Tue and Wed**. These are *intermediate* ensembles, recomputed as submissions arrive during the
open window; the Wednesday-night run after the deadline produces the final one. So a round's ensemble
file is typically **created days before its own `origin_date`** and then updated repeatedly.

This is visible in the history: of 261 commits to the syndromic hub's ensemble folder, only 92 are
file creations — the rest are updates.

## Observed timings (git history, `committer = github-actions`)

Weekday and hour (UTC) of the **last** commit to each round's ensemble file — i.e. the final ensemble:

| Cluster | Syndromic | COVID | What it is |
|---|---:|---:|---|
| **Wed 23:xx** | 36 rounds | 36 rounds | the post-deadline final run |
| Wed 10:xx | 14 | 20 | last intermediate run before the deadline |
| Wed 18–19:xx | 15 | 19 | last intermediate run before the deadline |
| Thu 00–02:xx | 12 | 5 | pipeline overrunning past midnight UTC |

Exact minutes of the Wednesday-night runs cluster at **23:36–23:49 UTC** in both hubs — the cron time
plus a few minutes of pipeline runtime.

## Reading the clock correctly

- Cron in GitHub Actions is **UTC**; the READMEs and workflow comments quote **CET**.
- Deadline **Wed 23:59 CET** = **22:59 UTC** in winter (CET = UTC+1). The final ensemble run at
  **23:30–23:50 UTC** therefore starts roughly **30–50 minutes after** the deadline, landing in the
  early hours of **Thursday CET** — which is what the README means by "published every Thursday".
- The schedule is not eternal: `forecast-pipeline.yml` (the current unified baseline → ensemble →
  upload → webhook chain) was only **created 2025-08-27**. Earlier seasons ran the same steps as
  separate workflows triggered off `CheckIntermediateRun`.

## Caveat

Commit timestamps record when the automation **committed** the ensemble, which is the best available
proxy for production time but is a few minutes later than job start, and later still than the moment
the ensemble was computed in memory. The webhook fires after the commit step, so the website's own
refresh is later again by an unknown (unlogged) amount.
