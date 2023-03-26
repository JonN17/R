


https://stats.oarc.ucla.edu/r/modules/factor-variables/


```
schtyp <- sample(0:1, 20, replace = TRUE)
```
> schtyp

 [1] 0 1 0 0 1 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0
> is.factor(schtyp)
[1] FALSE
> is.numeric(schtyp)
[1] TRUE

```
ses <- c("low", "middle", "low", "low", "low", "low", "middle", "low", "middle",
+          "middle", "middle", "middle", "middle", "high", "high", "low", "middle",
+          "middle", "low", "high")
```

```
is.factor(ses)
```

[1] FALSE

```
is.character(ses)
```
[1] TRUE