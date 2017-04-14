モンテカルロ及び準モンテカルロ法による数値積分
----------------------------------

このパッケージはモンテカルロ法による数値積分関数**mcint**および準モンテカルロ法
による数値積分関数**qmcint**を提供する。

準モンテカルロ法で使用できる三種類の超一様分布列があらかじめ定義されている。
それはWAFOM値の低い Niederreiter-Xing 点集合とSobol 点集合,
および高次元のSobol点集合である。

被積分関数は, ユーザーが定義する必要があるが, 数値のベクトルを引数として
受け取り, 数値を返す必要がある。
被積分関数に渡される点は (0, 1)^s の範囲の数になる。
数値積分の返す値は, 被積分関数の返す値の平均であり, 合計ではない。
合計を得るためには 2^m 倍する必要がある。

まず, 被積分関数の例を挙げる。これは原点を中心とする半径1の単位n次元超球の体積を求める関数である。

```{.r}
library(rmcqmcint)
unit.nsphere <- function(point) {
    if (sqrt(sum(point^2)) <= 1.0) {
        r <- 1.0
    } else {
        r <- 0.0
    }
    return(r)
}
```

例1:

qmc によりデフォルトの超一様分布列を使って4次元超球の体積を求める。

```{.r}
rs <- qmcint(integrand=unit.nsphere, N=100, s=4)
```

この結果は以下のようになる。

```{.r}
rs$mean
## [1] 0.3100977
rs$absError
## [1] 0.006666557
1 / 2 * pi^2 / 16 # to compare true value.
## [1] 0.3084251
```

例2:

qmcによってWAFOM値の小さいSobol点集合を使い, 5次元単位超球の体積を計算する。

```{.r}
rs <- qmcint(integrand=unit.nsphere, N=100, s=5, digitalNetID = 2)
```

この結果は以下のようになる。

```{.r}
rs$mean
## [1] 0.164375
rs$absError
## [1] 0.001514922
8 / 15 * pi^2 / 32 # to compare true value.
## [1] 0.1644934
```

例3:

4次元単位超球の体積をモンテカルロ法によって求める。

```{.r}
rs <- qmcint(integrand=unit.nsphere, N=100, s=4)
```

この結果はおおむね以下のようになるだろう。

```{.r}
rs$mean
## [1] 0.3100977
rs$absError
## [1] 0.006666557
1 / 2 * pi^2 / 16 # to compare true value.
## [1] 0.3084251
```

例4:

5次元単位超球の体積をモンテカルロ法によって求める。

```{.r}
rs <- qmcint(integrand=unit.nsphere, N=100, s=5)
```

この結果はおおむね以下のようになるだろう。

```{.r}
rs$mean
## [1] 0.1638379
rs$absError
## [1] 0.002203611
8 / 15 * pi^2 / 32 # to compare true value.
## [1] 0.1644934
```

別の被積分関数の例を定義する。Genz 氏の超一様分布列テスト関数Oscillatoryであるが,
以下に定義するのはパラメータを与えてOscillator関数のクロージャを作る関数である。

```{.r}
make.oscillatory <- function(a) {
	oscillatory <- function(point) {
		return(cos(sum(a * point)))
	}
	return(oscillatory)
}
```

ベクトルaの合計がπなら, この関数の(0, 1)^s領域での積分値はゼロになるはずである。
以下で指定している ID=3 は21201次元まで指定できる Sobol 点集合を示している。

```{.r}
n <- 100
id <- 3
s <- 1000
m <- 10
p <- 0.99
a <- rep(pi / s, length=s)
osc <- make.oscillatory(a)
rs <- qmcint(osc, N=n, s=s, digitalNetID=id, m=m, probability=p)
```

この結果は以下のようになる。

```{.r}
rs$mean
## [1] 1.167836e-06
rs$absError
## [1] 0.0002671781
```