{-# OPTIONS --rewriting #-}

module Higher where

open import lib

------------------------------------------------------------------------------
-- Higher order logic
------------------------------------------------------------------------------

-- notation for propositions:  X : Set          (the set of proofs)
--              sets:          M : Set
--              predicates:    P : M → Set
--              relations:     R : M → N → Set

Decidable : Set → Set
Decidable = λ A → A ∨ ¬ A

Nonempty : Set → Set
Nonempty = λ M → ∃ M λ x → ⊤

l1 : (∀ (A : Set) → Decidable A) → (A : Set) → ¬ ¬ A → A
l1 = λ dec a → λ nna → case _ (λ x → x) (λ na → abort _ (nna na))(dec a)

-- check the type of ¬_, _∧_, _∨_ (→ is not a defined operation), _,_,
-- proj₁, proj₂, inj₁, inj₂
A : Set
A = ⊤

l2 : ((A : Set) → Decidable A) → ¬ (X ∧ Y) → (¬ X ∨ ¬ Y)
l2 = λ lem → λ nxny → case _ (λ x → case _ (λ y → abort _ (nxny (x , y))) (λ ny → inj₂ ny) (lem Y)) (λ nx → inj₁ nx) (lem X)

module _ (lem : (A : Set) → Decidable A) where

  Peirce : ((X → Y) → X) → X
  Peirce = λ p → case _ (λ x → x) (λ nx → case _ (λ y → p λ _ → y) (λ ny → p λ x → abort _ (nx x)) (lem Y)) (lem X)

  l3 : (¬ ¬ X ∨ Y) → ¬ Y → X
  l3 = λ p → λ ny → case _ (λ x → x) (λ nx → case _ (l1 lem X) (λ y → abort _ (ny y)) p) (lem X)

  l4 : (M : Set)(P : M → Set) → (¬ ∃ M λ x → ¬ P x) → ∀(x : M) → P x
  l4 = λ M → λ P → λ q → λ x → case _ (λ x → x) (λ npx → abort _ (q (x ,' npx))) (lem (P x))

  -- use l4!
  pub : (M : Set)(P : M → Set) → Nonempty M → ∃ M λ m → P m → ∀(x : M) → P x
  pub = λ M → λ P → λ nonempty → case _ (λ enP → proj₁' enP ,' λ nP → λ x → abort _ (proj₂' enP nP) ) (λ neMnPm → {!!} ,' (λ x → l4 M P neMnPm)) (lem (∃ M λ m → ¬ P m))

  -- use l4!
  l5 : (¬ (∀(x : M) → P x)) → ∃ M λ x → ¬ P x
  l5 = {!!}

-- check the type of l3, pub
B : Set
B = {!l3!}

S : Set₁
S = Set

S₁ : Set₂
S₁ = Set₁

l6 : (n : ℕ) → ¬ (zero ≡ suc n)
l6 = {!!}
