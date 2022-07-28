# HittingSetModule.hs #

A Haskell `HittingSetModule.hs` module exporting a function that solves a variant of the HittingSet problem: the function takes a moderate-size family 
of moderate-size sets of integers, and it slowly returns the corresponding *lexicographically minimal* blocking set of *minimum cardinality*. 

In the spirit of Theorem 9.12(ii) and Example 9.13 from the monograph [A.O. Matveev, Symmetric Cycles](https://www.jennystanford.com/), Jenny Stanford Publishing, 2023.

Integer-Linear-Programming-free.

## Blocking Sets ##

Let $\mathcal{A} := \{A_1, A_2, ..., A_k\}$ be a finite nonempty family of nonempty subsets of a finite set of integers $E$.
A subset $B$ of the set $E$ is called a *blocking set* (or *hitting set*, *transversal*, *vertex cover* (or *node cover*), 
*system of representatives*) of the family $\mathcal{A}$ if and only if the set $B$ has a nonempty
intersection with each set $A_i$ from the family $\mathcal{A}$.
