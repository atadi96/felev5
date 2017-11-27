{-# OPTIONS --rewriting #-}

module Submission where

open import Agda.Primitive

infix 3 _≡_

------------------------------------------------------------------------------
-- function space
------------------------------------------------------------------------------

-- builtin

-- A : Set      x : A ⊢ B : Set
-- ----------------------------type formation
--      (x : A) → B : Set

--     x : A ⊢ t : B
-- ---------------------introduction
-- λ x → t : (x : A) → B

-- f : (x : A) → B    u : A
-- ------------------------elimination
--      f u : B[x ↦ u]

-- x : A ⊢ t : B    u : A
-- ----------------------computation (β)
-- (λ x → t) u = t[x ↦ u]

-- f : (x : A) → B
-- ---------------uniqueness
--  λ x → f x = f

------------------------------------------------------------------------------
-- equality
------------------------------------------------------------------------------

postulate
  _≡_   : ∀{i}{A : Set i} → A → A → Set i
  refl  : ∀{i}{A : Set i}{a : A} → a ≡ a



  J     : ∀{i}{A : Set i}{a : A}{j}(P : (x : A) → a ≡ x → Set j)(w : P a refl){a' : A}(w : a ≡ a') → P a' w

-- Indℕ  : ∀{i}(P : ℕ → Set i)(pz : P zero)(ps : {n : ℕ} → P n → P (suc n))(n : ℕ) → P n

  reflβ : ∀{i}{A : Set i}{a : A}{j}{P : (x : A) → a ≡ x → Set j}{w : P a refl} → J P w refl ≡ w

{-# BUILTIN REWRITE _≡_ #-}
{-# REWRITE reflβ #-}

------------------------------------------------------------------------------
-- exercise
------------------------------------------------------------------------------

cong : {A B : Set}(f : A → B){a a' : A} → a ≡ a' → f a ≡ f a'
cong {A} {B} f {a} {a'} aa' = J {A = A} {a} (λ a' _  → f a ≡ f a') refl aa' 
