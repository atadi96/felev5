{-# OPTIONS --rewriting #-}

module Submission where

-- propositional logic

infixl 5 _∧_
infixl 4 _,_
infixl 4 _∨_
infixl 3 _↔_

postulate
  X Y Z : Set -- propositional variables
  
  ⊥ : Set
  abort : (C : Set) → ⊥ → C
-- TODO: abort : {C : Set} → ⊥ → C
  
  ⊤ : Set
  tt : ⊤
  
  _∧_ : (A B : Set) → Set
  _,_ : {A B : Set} → A → B → A ∧ B
  proj₁ : {A B : Set} → A ∧ B → A
  proj₂ : {A B : Set} → A ∧ B → B

  _∨_ : (A B : Set) → Set
  inj₁ : {A B : Set} → A → A ∨ B
  inj₂ : {A B : Set} → B → A ∨ B
  case : {A B : Set}(C : Set) → (A → C) → (B → C) → A ∨ B → C
-- TODO: case : {A B : Set}{C : Set} → A ∨ B → (A → C) → (B → C) → C

¬_ : Set → Set
¬ A = A → ⊥

_↔_ : Set → Set → Set
A ↔ B = (A → B) ∧ (B → A)

-- predicate logic

postulate
  M N O : Set
  m     : M
  P Q   : M → Set
  R     : M → N → Set
  ∃     : (A : Set)(B : A → Set) → Set
  _,'_  : {A : Set}{B : A → Set}(a : A) → B a → ∃ A B
  proj₁' : {A : Set}{B : A → Set} → ∃ A B → A
  proj₂' : {A : Set}{B : A → Set}(w : ∃ A B) → B (proj₁' w)

-- Peano arithmetic

postulate
  _≡_   : {A : Set} → A → A → Set
  refl  : {A : Set}(a : A) → a ≡ a
  sym   : {A : Set}{a a' : A} → a ≡ a' → a' ≡ a
  trans : {A : Set}{a a' a'' : A} → a ≡ a' → a' ≡ a'' → a ≡ a''
  cong  : {A B : Set}(f : A → B){a a' : A} → a ≡ a' → f a ≡ f a'
  subst : {A : Set}(P : A → Set){a a' : A} → a ≡ a' → P a → P a'

infix 3 _≡_

{-# BUILTIN REWRITE _≡_ #-}

postulate
  ℕ    : Set
  zero : ℕ
  suc  : ℕ → ℕ
  Rec  : ∀{i}{A : Set i}(az : A)(as : A → A) → ℕ → A
  Ind  : ∀{i}(P : ℕ → Set i)(pz : P zero)(ps : (n : ℕ) → P n → P (suc n))(n : ℕ) → P n
  zeroβ : {A : Set}{az : A}{as : A → A}        → Rec az as zero ≡ az
  sucβ  : {A : Set}{az : A}{as : A → A}{n : ℕ} → Rec az as (suc n) ≡ as (Rec az as n)

{-# REWRITE zeroβ #-}
{-# REWRITE sucβ #-}







Decidable : Set → Set
Decidable = λ A → A ∨ ¬ A

Nonempty : Set → Set
Nonempty = λ M → ∃ M λ x → ⊤

module _ (lem : (A : Set) → Decidable A) where

  l4 : (M : Set)(P : M → Set) → (¬ ∃ M λ x → ¬ P x) → ∀(x : M) → P x
  l4 = λ M P w x → case _ (λ p → p) (λ npx → abort _ (w (x ,' npx))) (lem (P x))

  -- use l4!
  pub : (M : Set)(P : M → Set) → Nonempty M → ∃ M λ m → P m → ∀(x : M) → P x
  pub = λ M → λ P → λ nempty → {!proj₂' nempty!}

  -- use l4!
  l5 : (¬ (∀(x : M) → P x)) → ∃ M λ x → ¬ P x
  l5 = λ notevery → {!_ ,' _!}
