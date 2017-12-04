{-# OPTIONS --rewriting #-}

module TT1 where

open import tt

------------------------------------------------------------------------------
-- empty
------------------------------------------------------------------------------

emptyExample : (A B : Set) → (A → ⊥) → A → B
emptyExample A B na a = abort {C = B} (na a)

------------------------------------------------------------------------------
-- booleans
------------------------------------------------------------------------------

IF_THEN_ELSE_ : ∀{i} → Bool → {C : Set i} → C → C → C
IF b THEN t ELSE f = if[ _ ] b then t else f

IFtrue : ∀{C}{t f : C} → IF true THEN t ELSE f ≡ t
IFtrue = refl

IFfalse : ∀{C}{t f : C} → IF false THEN t ELSE f ≡ f
IFfalse = refl

not : Bool → Bool
not = λ b → IF b THEN false ELSE true

nottrue : not true ≡ false
nottrue = refl

notfalse : not false ≡ true
notfalse = refl

and : Bool → Bool → Bool
and = λ a b → IF a THEN b ELSE false

and-lid : ∀ b → and true b ≡ b
and-lid = λ _ → refl

and-rid : ∀ b → and b true ≡ b
and-rid = λ b → if[ (λ b → and b true ≡ b)  ] b then refl else refl 

or : Bool → Bool → Bool
or = λ a → λ b → IF a THEN true ELSE b

or-lid : ∀ b → or false b ≡ b
or-lid = λ b → refl

or-rid : ∀ b → or b false ≡ b
or-rid = λ b → if[ (λ b → or b false ≡ b) ] b then refl else refl

xor : Bool → Bool → Bool
xor = λ a → λ b → IF a THEN (not b) ELSE b

and-comm : ∀ b b' → and b b' ≡ and b' b
and-comm = λ b → λ b' → if[ (λ b → (and b b' ≡ and b' b)) ] b then  if[ (λ b' → and true b' ≡ and b' true) ] b' then refl else refl else ((if[(λ b' → and false b' ≡ and b' false)] b' then refl else refl))

or-comm : ∀ b b' → or b b' ≡ or b' b
or-comm = λ b b' → if[(λ b → or b b' ≡ or b' b)] b then (if[(λ b' → or true b' ≡ or b' true)] b' then refl else refl ) else ((if[(λ b' → or false b' ≡ or b' false)] b' then refl else refl))

xor-comm : ∀ b b' → xor b b' ≡ xor b' b
xor-comm = λ b b' → if[(λ b → xor b b' ≡ xor b' b)] b then (if[(λ b' → xor true b' ≡ xor b' true)] b' then refl else refl) else ((if[(λ b' → xor false b' ≡ xor b' false)] b' then refl else refl))

------------------------------------------------------------------------------
-- equality
------------------------------------------------------------------------------

sym   : {A : Set}{a a' : A} → a ≡ a' → a' ≡ a
sym {A}{a}{a'} =  λ aeqa' → J {A = A}{a} (λ a' w → a' ≡ a) refl {a'} aeqa'

trans : {A : Set}{a a' a'' : A} → a ≡ a' → a' ≡ a'' → a ≡ a''
trans {A}{a}{a'}{a''} v  = J {A = A}{a}(λ a' w → a' ≡ a'' → a ≡ a'')  (λ w → w) v 

cong  : ∀{i}{j}{A : Set i}{B : Set j}(f : A → B){a a' : A} → a ≡ a' → f a ≡ f a'
cong {A = A}{B} f {a}{a'} aa' = J {_} {A} {a}  {_} (λ x w → f a ≡ f x ) refl aa'

subst : ∀{i}{A : Set i}{j}(P : A → Set j){a a' : A} → a ≡ a' → P a → P a'
subst {A = A} P {a} {a'} aa' Pa = J {_} {A} {_} (λ x w →  P x ) Pa aa'  

idr : {A : Set}{a a' : A}(p : a ≡ a') → trans p refl ≡ p
idr {A} {a} {a'} p = J (λ a' p → trans p refl ≡ p ) refl p

idl : {A : Set}{a a' : A}(p : a ≡ a') → trans refl p ≡ p
idl {A} {a} {a'} p = refl

-- true ≠ false

true? : Bool → Set
true? = λ b → IF b THEN ⊤  ELSE ⊥

true≢false : (true ≡ false) → ⊥
true≢false = λ tf → subst true? tf tt

-- 0 ≠ 1

zero? : ℕ → Set
zero? = λ n → Indℕ _ ⊤ (λ _ → ⊥) n

0≢1 : {n : ℕ} → (zero ≡ suc n) → ⊥
0≢1 = λ w → subst zero? w tt

------------------------------------------------------------------------------
-- decidability of equality
------------------------------------------------------------------------------

Decidable : ∀{i} → Set i → Set i
Decidable = λ A → A + ¬ A

≡Bool : Bool → Bool → Bool
≡Bool = λ a b → IF a THEN b ELSE not b

test≡Bool₁ : ≡Bool true true ≡ true
test≡Bool₁ = refl

test≡Bool₂ : ≡Bool false false ≡ true
test≡Bool₂ = refl

test≡Bool₃ : ≡Bool true false ≡ false
test≡Bool₃ = refl

test≡Bool₄ : ≡Bool false true ≡ false
test≡Bool₄ = refl

dec≡Bool : (b b' : Bool) → Decidable (b ≡ b')
dec≡Bool =
  λ b b' →
    if[ (λ c → (c ≡ b') + (c ≡ b' → ⊥)) ] b then
      (if[ (λ b' → (true ≡ b') + (true ≡ b' → ⊥)) ] b'  then
        inj₁ refl
      else
        inj₂ (true≢false) )
    else (
      (if[ (λ b' → (false ≡ b') + (false ≡ b' → ⊥)) ] b' then
        inj₂ (λ w → true≢false (sym w))
      else
        inj₁ refl)
      )

dec≡ℕ : (n n' : ℕ) → Decidable (n ≡ n')
dec≡ℕ = λ n n' → Indℕ (λ n' → Decidable (n ≡ n')) ((Indℕ (λ n → (n ≡ zero) + (n ≡ zero → ⊥)) (inj₁ refl) (λ {n} w → {!!}) n)) (λ {n} w → {!case (λ ((x ≡ x) + (x ≡ x → ⊥)) → (x ≡ suc x) + (x ≡ suc x → ⊥) ) ?) ? w!}  ) n'



dec≡+ : ∀{i}{A B : Set i}
        (dec≡A : (a a' : A) → Decidable (a ≡ a'))
        (dec≡B : (b b' : B) → Decidable (b ≡ b'))
      → (w w' : A + B) → Decidable (w ≡ w')
dec≡+{_}{A}{B} decA decB w w'  =
  case (λ w → (w ≡ w') + (w ≡ w' → ⊥))
    ((λ x → case (λ w' → (inj₁ x ≡ w') + (inj₁ x ≡ w' → ⊥))
        (λ x' →  case _ (λ w → inj₁ (cong inj₁ w)) {!!} (decA x x'))
        {!!}
        w'))
    {!!}
    w 

------------------------------------------------------------------------------
-- lists
------------------------------------------------------------------------------

l1 : List Bool
l1 = true ∷l true ∷l true ∷l true ∷l true ∷l []l

l2 : List Bool
l2 = []l


lengthl : {A : Set} → List A → ℕ
lengthl = λ xs → IndList (λ _ → ℕ) zero (λ n _ → suc n) xs 

plengthl : lengthl (zero ∷l zero ∷l zero ∷l []l) ≡ suc (suc (suc zero))
plengthl = refl

mapsl : {A B : Set} → List (A → B) → List A → List B
mapsl = λ f xs → {!IndList _ []l !}

headl : {A : Set} → List A → A
headl = {!!}

taill : {A : Set} → List A → List A
taill = {!!}

headl' : {A : Set}(xs : List A) → ¬ (lengthl xs ≡ zero) → A
headl' {A} = λ xs q → IndList (λ xs → ¬(lengthl xs ≡ zero) → A)
                                           (λ zzn → abort {_}{A}(zzn refl))
                                           (λ _ x _ → x) xs q

------------------------------------------------------------------------------
-- vectors
------------------------------------------------------------------------------

n5 : ℕ
n5 = suc (suc (suc (suc (suc zero))))

v1 : Vec Bool n5
v1 = true ∷ true ∷ true ∷ true ∷ true ∷ []

v2 : Vec Bool zero
v2 = []

maps : {n : ℕ}{A B : Set} → Vec (A → B) n → Vec A n → Vec B n
maps {n}{A}{B} fs as = IndVec (λ {n} xs → Vec B n) [] ((λ {n} {xs} bs x → {!head fs!} ∷ bs )) as

head : {n : ℕ}{A : Set} → Vec A (suc n) → A
head {n}{A} vec = IndVec (λ {m} xs → m ≡ suc n → A) ((λ w → abort (0≢1 w))) (λ _ x _ → x) vec refl

tail : {n : ℕ}{A : Set} → Vec A (suc n) → Vec A n
tail {n} {A} xs = IndVec (λ {m} xs → m ≡ suc n → Vec A n) (λ w → abort (0≢1 w)) (λ {m}{xs} _ _ w → {!subst (Vec A) !}) xs refl
