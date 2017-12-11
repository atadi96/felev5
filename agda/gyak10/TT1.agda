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

IFtrue : ∀{i}{C : Set i}{t f : C} → IF true THEN t ELSE f ≡ t
IFtrue = refl

IFfalse : ∀{i}{C : Set i}{t f : C} → IF false THEN t ELSE f ≡ f
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
and-rid = λ b → {!!}

or : Bool → Bool → Bool
or = {!!}

xor : Bool → Bool → Bool
xor = {!!}

and-comm : ∀ b b' → and b b' ≡ and b' b
and-comm = {!!}

or-comm : ∀ b b' → or b b' ≡ or b' b
or-comm = {!!}

xor-comm : ∀ b b' → xor b b' ≡ xor b' b
xor-comm = {!!}

------------------------------------------------------------------------------
-- equality
------------------------------------------------------------------------------

sym   : ∀{i}{A : Set i}{a a' : A} → a ≡ a' → a' ≡ a
sym = {!!}

trans : ∀{i}{A : Set i}{a a' a'' : A} → a ≡ a' → a' ≡ a'' → a ≡ a''
trans = {!!}

cong  : ∀{i j}{A : Set i}{B : Set j}(f : A → B){a a' : A} → a ≡ a' → f a ≡ f a'
cong = {!!}

subst : ∀{i j}{A : Set i}(P : A → Set j){a a' : A} → a ≡ a' → P a → P a'
subst = {!!}

idr : {A : Set}{a a' : A}(p : a ≡ a') → trans p refl ≡ p
idr = {!!}

idl : {A : Set}{a a' : A}(p : a ≡ a') → trans refl p ≡ p
idl = {!!}

-- true ≠ false

true? : Bool → Set
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

≡Bool : Bool → Bool → Bool
≡Bool = {!!}

test≡Bool₁ : ≡Bool true true ≡ true
test≡Bool₁ = {!!}

test≡Bool₂ : ≡Bool false false ≡ true
test≡Bool₂ = {!!}

test≡Bool₃ : ≡Bool true false ≡ false
test≡Bool₃ = {!!}

test≡Bool₄ : ≡Bool false true ≡ false
test≡Bool₄ = {!!}

dec≡Bool : (b b' : Bool) → Decidable (b ≡ b')
dec≡Bool = {!!}

dec≡ℕ : (n n' : ℕ) → Decidable (n ≡ n')
dec≡ℕ = {!!}

dec≡+ : ∀{i}{A B : Set i}
        (dec≡A : (a a' : A) → Decidable (a ≡ a'))
        (dec≡B : (b b' : B) → Decidable (b ≡ b'))
      → (w w' : A + B) → Decidable (w ≡ w')
dec≡+ = {!!}

------------------------------------------------------------------------------
-- lists
------------------------------------------------------------------------------

l1 : List Bool
l1 = true ∷l true ∷l true ∷l true ∷l true ∷l []l

l2 : List Bool
l2 = []l


lengthl : {A : Set} → List A → ℕ
lengthl = {!!}

mapsl : {A B : Set} → List (A → B) → List A → List B
mapsl = {!!}

headl : {A : Set} → List A → A
headl = {!!}

taill : {A : Set} → List A → List A
taill = {!!}

headl' : {A : Set}(xs : List A) → ¬ (lengthl xs ≡ zero) → A
headl' = {!!}

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
maps = {!!}

head : {n : ℕ}{A : Set} → Vec A (suc n) → A
head = {!!}

tail : {n : ℕ}{A : Set} → Vec A (suc n) → A
tail = {!!}

------------------------------------------------------------------------------
-- isomorphism of Bool and ⊤ + ⊤
------------------------------------------------------------------------------

_≅_ : Set → Set → Set -- \~=
A ≅ B = Σ (A → B) λ f → Σ (B → A) λ g → ((x : A) → g (f x) ≡ x) × ((y : B) → f (g y) ≡ y)

w : Bool ≅ (⊤ + ⊤)
w =  f
  , (g
  , ((λ x → if[ (λ x → g (f x) ≡ x)  ] x then refl else refl)
  ,  λ y → case (λ y → f (g y ) ≡ y) (Ind⊤ _ refl) (Ind⊤ _ refl) y))
  where
    f : Bool → ⊤ + ⊤
    f = λ b → IF b THEN inj₁ tt ELSE inj₂ tt

    g : ⊤ + ⊤ → Bool
    g = λ c → case _ (λ _ → true) (λ _ → false) c

------------------------------------------------------------------------------
-- the two different isomorphisms between Bool and Bool
------------------------------------------------------------------------------

idiso : Bool ≅ Bool
idiso = (λ x → x), ((λ x → x), (((λ _ → refl) , ((λ _ → refl)))))

notiso : Bool ≅ Bool
notiso = not , (not , ((λ x → if[ (λ x → not (not x) ≡ x) ] x then refl else refl) , (λ y → if[ (λ x → not (not x) ≡ x) ] y then refl else refl)))

------------------------------------------------------------------------------
-- Curry - Uncurry
------------------------------------------------------------------------------

curryiso : ∀{A B C : Set} → (A → B → C) ≅ (A × B → C)
curryiso {A}{B}{C}=
         f ,
         (g ,
         ((λ f → refl) ,
         (λ y → funext ((λ x → cong y (sym (h x))))  )))
         where
           f : (A → B → C) → (A × B → C)
           f = λ f' → λ x → f' (proj₁ x) (proj₂ x)
           g : (A × B → C) → (A → B → C)
           g = λ f → λ a → λ b → f (a , b)
           h : (x : A × B) → x ≡ (proj₁ x , proj₂ x)
           h = λ x → IndΣ (λ x → x ≡ (proj₁ x , proj₂ x)) (λ a b → refl) x 

is : ∀{A : Set} → (Bool → A) ≅ (A × A)
is {A} = f ,
               g ,
                 (λ h → funext {f =  (λ b → if[ (λ v → A) ] b then (h true) else (h false))}{g = h}
                   (λ x → if[ (λ x → (if[ (λ v → A) ] x then h true else h false) ≡ h x) ] x then refl else refl)
                 ) ,
               λ y → IndΣ (λ y →  IndΣ (λ _ → A) (λ a _ → a) y , IndΣ (λ w₁ → A) (λ _ b → b) y ≡ y) (λ _ _ → refl) y
              where
                f : (Bool → A) → (A × A)
                f = λ f → (f true , f false)
                g : (A × A) → (Bool → A)
                g = λ x → λ b → IF b THEN proj₁ x ELSE proj₂ x
           

------------------------------------------------------------------------------
-- random exercises
------------------------------------------------------------------------------

congid : ∀{ℓ}{A : Set ℓ}{x y : A}(p : x ≡ y) → cong (λ x → x) p ≡ p
congid = {!!}

≡inv : ∀{ℓ}{A : Set ℓ} {x y : A} (p : x ≡ y) → (trans p (sym p)) ≡ refl
≡inv = {!!}

,=0 : ∀{ℓ ℓ'}{A : Set ℓ}{B : A → Set ℓ'}{a a' : A}{b : B a}{b' : B a'}
   → _≡_ {A = Σ A B} (a , b) (a' , b') → a ≡ a'
,=0 = {!!}

,=1 : ∀{ℓ ℓ'}{A : Set ℓ}{B : A → Set ℓ'}{a a' : A}{b : B a}{b' : B a'}
      (p : (a , b) ≡ (a' , b')) → subst B (,=0 p) b ≡ b'
,=1 = {!!}

,= : ∀{ℓ ℓ'}{A : Set ℓ}{B : A → Set ℓ'}{a a' : A}{b : B a}{b' : B a'}
     (p : a ≡ a') → subst B p b ≡ b' → _≡_ {A = Σ A B} (a , b) (a' , b')
,= = {!!}

-- TODO: set → h1
-- TODO: Hedberg
