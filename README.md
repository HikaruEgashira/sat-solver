# SAT Solver

## Prerequirement

- OCaml
- Dune

## Usage

```ml
dune exec sat_solver
```

## 仕様説明

- CNF形式の入力ファイルが与える

```dimacs
p cnf 2 3
1 2 0
-1 2 0
-1 -2
```

- 上記ファイルは以下のような状態を表す

> リテラルの種類 ... 2種類(P1, P2)
> 節の数 ... 3種類
> 論理式 ... `(P1∨P2)∧(￢P1∨P2)∧(￢P1∨￢P2)`

- このデータをOcaml上で扱いやすいように、以下のデータ構造に変換する

```ocaml
// CNF
type cnf = {
    num_vars: int  (* 2 *)
    clauses: clause list (* [[1, 2], [-1, 2], [-1, -2]] *)
}

// 節
type clause = {
    lits: lit list (* [1, 2] *)
}

// リテラル
type lit = {
    var: int (* 1, -2 *)
}
```

* 例外 `PARSE ERROR`, `FILE NOT FOUND`
* 参考 <https://github.com/niklasso/minisat/blob/master/minisat/core/Dimacs.h>

- 同時に出力するデータ構造も定義する
- cnfと

```ocaml
type t = l_True | l_False | l_Undef
type ret = t list (* [l_False, l_True], UNSATの場合は空配列 *)
```

- ファイルに出力すると以下のような形式になる

```
SAT
-1 2 0
```

## 参考

- <https://www.craigfe.io/operator-lookup/>
