{-# OPTIONS --rewriting #-}

module TT1 where

open import tt

------------------------------------------------------------------------------
-- booleans
------------------------------------------------------------------------------

not : 𝔹 → 𝔹
not = λ b → if[ _ ] b then false else true

and : 𝔹 → 𝔹 → 𝔹
and = λ a → λ b → if[ _ ] a then b else false 

or : 𝔹 → 𝔹 → 𝔹
or = λ a → λ b → if[ _ ] a then true else b

xor : 𝔹 → 𝔹 → 𝔹
xor = λ a → λ b → if[ _ ] a then not b else b

not-false : not false ≡ true
not-false = refl

and-lid : ∀ b → and true b ≡ b
and-lid = λ b → refl

and-rid : ∀ b → and b true ≡ b
and-rid = {!!}

and-comm : ∀ b b' → and b b' ≡ and b' b
and-comm = λ b b' → {!if[ ? ] ? then ? else ?!}

or-comm : ∀ b b' → or b b' ≡ or b' b
or-comm = {!!}

xor-comm : ∀ b b' → xor b b' ≡ xor b' b
xor-comm = {!!}

------------------------------------------------------------------------------
-- equality
------------------------------------------------------------------------------

sym   : {A : Set}{a a' : A} → a ≡ a' → a' ≡ a
sym = {!!}

trans : {A : Set}{a a' a'' : A} → a ≡ a' → a' ≡ a'' → a ≡ a''
trans = {!!}
cong  : {A B : Set}(f : A → B){a a' : A} → a ≡ a' → f a ≡ f a'
cong = {!!}

subst : {A : Set}(P : A → Set){a a' : A} → a ≡ a' → P a → P a'
subst = {!!}

-- true ≠ false

true? : 𝔹 → Set
true? = {!!}

true≢false : (true ≡ false) → ⊥
true≢false = {!!}

-- 0 ≠ 1

zero? : ℕ → Set
zero? = {!!}

0≢1 : (zero ≡ suc zero) → ⊥
0≢1 = {!!}

------------------------------------------------------------------------------
-- decidability of equality
------------------------------------------------------------------------------

Decidable : ∀{i} → Set i → Set i
Decidable = λ A → A + ¬ A

dec≡𝔹 : (b b' : 𝔹) → Decidable (b ≡ b')
dec≡𝔹 = {!!}

dec≡ℕ : (n n' : ℕ) → Decidable (n ≡ n')
dec≡ℕ = {!!}

dec≡+ : ∀{i}{A B : Set i}
        (dec≡A : (a a' : A) → Decidable (a ≡ a'))
        (dec≡B : (b b' : B) → Decidable (b ≡ b'))
      → (w w' : A + B) → Decidable (w ≡ w')
dec≡+ = {!!}
