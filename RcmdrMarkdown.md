<!-- R Commander Markdown Template -->

Replace with Main Title
=======================

### Your Name

### 2015-12-20




```r
> pred <- prediction(ich$GCS, ich$Outcome)
> perf <- performance(pred, 'tpr', 'fpr')
> plot(perf, colorize=FALSE, add=FALSE)
```

<img src="figure/unnamed-chunk-2-1.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" width="750" />

```r
> remove(perf)
> remove(pred)
```


```r
> pred <- prediction(ich$GCS, ich$Outcome)
> perf <- performance(pred, 'fpr', 'tpr')
> plot(perf, colorize=FALSE, add=FALSE)
```

<img src="figure/unnamed-chunk-3-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" width="750" />

```r
> remove(perf)
> remove(pred)
```


