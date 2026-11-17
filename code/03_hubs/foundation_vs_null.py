import csv, statistics as st, random, collections, json, sys
FM={"Chronos-Chronos2","pedro-Chronos2","DHauser-FluChronos","QMUL-TabPFNTS"}
EXCL=FM|{"fluxengine-chronos2","fluxengine-triad","respicast-hubEnsemble","respicast-quantileBaseline"}
R=collections.defaultdict(dict); ens={}; hubof={}
for h in ("emh-syndromic","emh-covid"):
    for x in csv.DictReader(open(f"/workspace/{h}/model-evaluation/latest-forecast_scores.csv")):
        if x["metric"]!="WIS" or x["horizon"] not in("1","2"): continue
        k=(h,x["origin_date"],x["target"],x["location"],x["horizon"]); m=x["team_id"]+"-"+x["model_id"]
        if m=="respicast-hubEnsemble": ens[k]=float(x["rank_score"])
        else: R[m][k]=float(x["rank_score"]); hubof.setdefault(m,set()).add(h)
T={m:[k for k in R[m] if k in ens] for m in FM}
obs=[R[m][k]>ens[k] for m in FM for k in T[m]]; OBS=st.mean(obs)
print(f"foundation models: {len(obs)} pairs, win rate {100*OBS:.1f}%")
pool={h:sorted(m for m in R if m not in EXCL and h in hubof[m]) for h in ("emh-syndromic","emh-covid")}
for h,p in pool.items(): print(f"pool {h}: {len(p)} models")
FMhub={m:next(iter(hubof[m])) for m in FM}
cov={(m,c):sum(k in R[c] for k in T[m])/len(T[m]) for m in FM for c in pool[FMhub[m]]}
out={"obs":OBS}
for minov in (0.2,0.5):
    random.seed(1); rates=[]; covs=[]
    cand={m:[c for c in pool[FMhub[m]] if cov[(m,c)]>=minov] for m in FM}
    for _ in range(500):
        w=[]; tot=0
        for m in FM:
            c=random.choice(cand[m])
            w+=[R[c][k]>ens[k] for k in T[m] if k in R[c]]; tot+=len(T[m])
        rates.append(st.mean(w)); covs.append(len(w)/tot)
    rs=sorted(rates); pct=100*sum(r<OBS for r in rates)/500
    sizes=", ".join(m+":"+str(len(cand[m])) for m in sorted(FM))
    print(f"\npool rule: coverage >= {100*minov:.0f}% of replaced model's targets ({sizes})")
    print(f"  median {100*st.median(rs):.1f}%, central 95% {100*rs[12]:.1f}-{100*rs[487]:.1f}%, min {100*rs[0]:.1f}, max {100*rs[-1]:.1f}; mean coverage {100*st.mean(covs):.0f}%")
    print(f"  foundation {100*OBS:.1f}% is above {pct:.0f}% of draws")
    out[str(minov)]=rates
json.dump(out,open(sys.argv[1],"w"))
print("\nnon-foundation models, win rate vs ensemble on their own h1-2 targets (n>=100):")
for c in sorted(set(pool['emh-syndromic'])|set(pool['emh-covid'])):
    w=[R[c][k]>ens[k] for k in R[c] if k in ens]
    if len(w)>=100: print(f"  {c:30s} {100*st.mean(w):5.1f}%  (n={len(w)})")
