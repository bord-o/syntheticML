type nat = Z | S of nat
type typ = TUnit | TArr of typ * typ
type expr = Unit | Var of nat | Lam of typ * expr | App of expr * expr
