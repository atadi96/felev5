{-# OPTIONS --rewriting #-}

module Peano where

open import lib

three : ℕ
three = suc (suc (suc zero))

seven : ℕ
seven = suc (suc (suc (suc three )))

-- 0 ↦ 1
-- 1 ↦ 3
-- 2 ↦ 5
-- 3 ↦ 7
-- 4 ↦ 9

2*_+1 : ℕ → ℕ
2*_+1 = λ n → suc (Rec n suc n)
-- 2*_+1 = λ n → Rec (suc zero) (λ x → suc (suc x)) n
test : 2* three +1 ≡ seven
test = refl seven

four five ten twelve seventeen : ℕ
four      = suc three
five      = suc four
ten       = suc (suc (suc seven))
twelve    = suc (suc ten)
seventeen = suc (suc (suc (suc (suc (suc (suc ten))))))

3*_+5 : ℕ → ℕ
3*_+5 = λ n → Rec five (λ x → suc (suc (suc x))) n

test1 : 3* four +5 ≡ seventeen
test1 = refl seventeen

test2 : 3* zero +5 ≡ five
test2 = refl five

_+_ : ℕ → ℕ → ℕ
_+_ = λ x → λ y → Rec x suc y

infixl 5 _+_

test+ : three + four ≡ seven
test+ = refl seven

leftunit : ∀(n : ℕ) → zero + n ≡ n
leftunit = λ n → Ind (λ x → zero + x ≡ x) (refl zero) (λ x → λ y → cong suc y) n

rightunit : ∀(n : ℕ) → n + zero ≡ n
rightunit = λ n → refl n

assoc : ∀(a b c : ℕ) → (a + b) + c ≡ a + (b + c)
assoc = λ a b → Ind (λ c → a + b + c ≡ a + (b + c)) (refl (Rec a suc b)) (λ n → λ m → cong suc m)

_*_ : ℕ → ℕ → ℕ
_*_ = λ x y → Rec zero (x +_) y

infixl 6 _*_

test* : three * four ≡ twelve
test* = refl twelve

sucright : ∀(a b : ℕ) → a + suc b ≡ suc (a + b)
sucright = λ a b → refl (suc (Rec a suc b))

sucleft : ∀(a b : ℕ) → suc a + b ≡ suc (a + b) -- ez fog kelleni a comm bizonyitasahoz
sucleft = {!!}

comm : ∀(a b : ℕ) → a + b ≡ b + a
--comm = λ a → Ind (λ b → Rec a suc b ≡ Rec b suc a) (Ind (λ a → a ≡ Rec zero suc a) (refl zero) (λ _ → λ eq → cong suc eq) a) (Ind (λ n → Rec a suc n ≡ Rec n suc a → suc (Rec a suc n) ≡ Rec (suc n) suc a) (λ eq → {!!}) {!!})
comm = λ a → {!!}
