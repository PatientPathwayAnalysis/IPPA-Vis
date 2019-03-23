# IPPA-Vis
Visualisation tools for IPPA 

## Load IPPA-Vis

Install using devtools
```r
library(ippaVis)
```

## Sequence frequency diagram

```r
pat.freq <- visualise_pattern_freq(pseudo.tb.js)
print(pat.freq)
```

## Stage distribution diagram

```r
st.di <- visualise_stage_dist(pseudo.tb.js)
print(st.di)
```

## Referral diagram

```r
ref <- visualise_referrals(pseudo.tb.p, bar.width=40)
print(ref)
```

## Capacity and access diagram

```r
ppa <- visualise_accessibility(pseudo.tb.h, pseudo.tb.p)
print(ppa)
```


## License
See [License](LICENSE.)
