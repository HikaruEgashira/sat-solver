# SAT Solver

## Prerequirement

- OCaml
- Dune

## Usage

```ml
dune exec sat_solver -- ./cnf/lesson1.dimacs
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

- このデータをOCaml上で扱いやすいように、以下のデータ構造に変換する

```ocaml
(* CNF *)
type cnf = {
    num_vars: int  (* 2 *)
    clauses: clause list (* [[1; 2], [-1; 2], [-1; -2]] *)
}

(* 節 *)
type clause = {
    lits: lit list (* [1; 2] *)
}

(* リテラル *)
type lit = {
    var: int (* 1, -2 *)
}
```

* 例外 `INVALID_FORMAT`, `FILE_NOT_EXIST`
* 参考 <https://github.com/niklasso/minisat/blob/master/minisat/core/Dimacs.h>

- 同時に出力するデータ構造も定義する
- cnfと

```ocaml
type t = bool option
type ts = t list
type output = ts option (* [Some false; Some true], UNSATの場合はNoneが含まれる *)
```

- ファイルに出力すると以下のような形式になる

```
SAT
-1 2 0
```

## 参考

- <https://www.craigfe.io/operator-lookup/>
- <https://zenn.dev/hoddy3190/articles/4b7347ecd7ba59>
