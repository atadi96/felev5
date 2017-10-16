module Fun where

open import lib

------------------------------------------------------------------------------
-- Function space
------------------------------------------------------------------------------

--  x : M ⊢ t : N              f : M → N    u : M
-- ---------------intro        ------------------elim
-- λ x → t : M → N                   f u : N

id : M → M
id = λ x → x

const : M → N → M
const = λ x _ → x

choose1 : M → M → M
choose1 = λ x _ → x

choose2 : M → M → M
choose2 = λ _ x → x

_∘_ : (N → O) → (M → N) → (M → O)
_∘_ = λ f g → λ x → f (g x)

flip : (M → N → O) → (N → M → O)
flip = λ f → λ n m → f m n
