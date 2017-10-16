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
  M N  : Set
  P Q  : M → Set
  R    : M → N → Set
  ∃    : (A : Set)(B : A → Set) → Set
  _,'_ : {A : Set}{B : A → Set}(a : A) → B a → ∃ A B
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
  Rec  : {A : Set}(az : A)(as : A → A) → ℕ → A
  Ind  : (P : ℕ → Set)(pz : P zero)(ps : (n : ℕ) → P n → P (suc n))(n : ℕ) → P n
  zeroβRec : {A : Set}{az : A}{as : A → A}        → Rec az as zero ≡ az
  sucβRec  : {A : Set}{az : A}{as : A → A}{n : ℕ} → Rec az as (suc n) ≡ as (Rec az as n)

{-# REWRITE zeroβRec #-}
{-# REWRITE sucβRec #-}

--  J     : {A : Set}{a : A}(P : (x : A) → a ≡ x → Set) → P a refl → {a' : A}(w : a ≡ a') → P a' w

---------------------------------------------------------

_+_ : ℕ → ℕ → ℕ
_+_ = λ a b → Rec b (λ a'+b → suc a'+b) a

infixl 5 _+_

rightunit : ∀(n : ℕ) → n + zero ≡ n
rightunit = λ n → Ind (λ x → x + zero ≡ x) (refl zero) (λ x → λ p → cong suc p) n

sucright : ∀(a b : ℕ) → a + suc b ≡ suc (a + b)
sucright = λ a → Ind (λ b → a + suc b ≡ suc (a + b)) (trans {!!} {!!}) {!!}

-- ajanlott hasznalni a kovetkezoket: cong, sym, trans, rightunit

comm : ∀(a b : ℕ) → a + b ≡ b + a
comm = {!!}
