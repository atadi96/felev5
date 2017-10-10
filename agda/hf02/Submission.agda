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

---------------------------------------------------------

l27 : ¬ (X ∨ Y) ↔ (¬ X ∧ ¬ Y)
l27 = (λ nxvy → (λ x → nxvy (inj₁ x)) , λ y → nxvy (inj₂ y)) , λ nxny → λ xvy → case _ (proj₁ nxny) (proj₂ nxny) xvy

l28 : (¬ X ∨ ¬ Y) → ¬ (X ∧ Y)
l28 = λ nxvny → λ xy → case _ (λ nx → nx (proj₁ xy)) (λ ny → ny (proj₂ xy)) nxvny

l2 : ¬ ¬ (∀(x : M) → P x) → ∀(x : M) → ¬ ¬ (P x)
l2 = λ a → λ x → λ npx → a λ nxpx → npx (nxpx x)
