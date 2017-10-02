module Prop where

open import lib

------------------------------------------------------------------------------
-- Implication
------------------------------------------------------------------------------

-- A → B -- \->

-- A → B → C = A → (B → C)

--  x : A ⊢ p : B              f : A → B    a : A
-- ---------------intro        ------------------elim
-- λ x → p : A → B                   f a : B

l1 : X → X
l1 = λ x → x

l2 : X → Y → X
l2 = λ x → λ y → x

l3 : (X → Y) → X
l3 = λ x → {!!} --nincs út

l4 : X → Y → Y
l4 = λ x → λ y → y

l5 : X → X → X
l5 = λ x → λ x → x

l5' : X → X → X
l5' = λ x → λ x' → x

l6 : (X → Y → Z) → (Y → X → Z)
l6 = λ f → λ y → λ x → f x y 

------------------------------------------------------------------------------
-- True
------------------------------------------------------------------------------

-- ⊤ -- \top

-- 
-- ------intro
-- tt : ⊤

l7 : ⊤
l7 = tt

l8 : X → ⊤
l8 = λ x → tt

------------------------------------------------------------------------------
-- False
------------------------------------------------------------------------------

-- ⊥ -- \bot

--     p : ⊥
-- -------------elim
-- abort C p : C

l9 : ⊥ → X
l9 = λ p → abort X p

l9' : (X → ⊥) → (X → Y)
l9' = λ f → λ x → abort Y (f x)

-- negation : ¬ A = A → ⊥ -- \neg

l10 : X → ¬ ¬ X
l10 = λ x → λ f → f x

l11 : ¬ ¬ (¬ ¬ X → X)
l11 = λ f → f λ g → abort X (g λ x → f λ h → x)

l12 : ¬ ¬ ¬ X → ¬ X
l12 = λ f → λ x → f λ g → g x

l13 : ¬ X → ¬ ¬ ¬ X
l13 = λ f → λ g → g f

------------------------------------------------------------------------------
-- And
------------------------------------------------------------------------------

-- _∧_ -- \and

-- p : A   q : B              p : A ∧ B              p : A ∧ B      
-- -------------intro        -----------elim1       -----------elim2
-- p , q : A ∧ B             proj₁ p : A            proj₂ p : B

l14 : (X → Y) ∧ (Y → Z) → (X → Z)
l14 = λ w → λ x -> proj₂ w (proj₁ w x)

l15 : ((X ∧ Y) ∧ Z) → (X ∧ (Y ∧ Z))
l15 = λ w → proj₁ (proj₁ w), (proj₂ (proj₁ w) , proj₂ w)

l16 : (X → (Y ∧ Z)) → ((X → Y) ∧ (X → Z))
l16 = λ f → (λ x → proj₁ (f x)) , (λ x → proj₂ (f x))

-- if and only if: A ↔ B = (A → B) ∧ (B → A)\<->

l19 : X ↔ X ∧ ⊤
l19 = (λ x → x , tt) , (λ w → proj₁  w)

l17 : ¬ ¬ ¬ X ↔ ¬ X
l17 = (λ f → λ x → f (λ g → g x)) , (λ f → λ g → g f)

l18 : ¬ (X ↔ ¬ X)
l18 = λ w → proj₁ w (proj₂ w (λ x → proj₁ w x x )) (proj₂ w (λ x → proj₁ w x x ))

------------------------------------------------------------------------------
-- Or
------------------------------------------------------------------------------

-- _∨_ -- \or

--      p : A                    q : B          
-- --------------intro1     --------------intro2
-- inj₁ p : A ∨ B           inj₂ q : A ∨ B      
--
-- p : A → C      q : B → C     r : A ∨ B
-- --------------------------------------elim
--     case C p q r : C

l21 : X ∨ X → X
l21 = λ w → case X (λ x → x) l1 w

l22 : (X ∧ Y) ∨ Z → (X ∨ Z) ∧ (Y ∨ Z)
l22 = λ w → case (X ∨ Z) (λ xy → inj₁ (proj₁ xy)) (λ z → inj₂ z) w , case (Y ∨ Z) (λ xy → inj₁ (proj₂ xy)) (λ z → inj₂ z) w

l20 : X ↔ X ∨ ⊥
l20 = (λ x → inj₁ x) , (λ xb → case X l1 (abort X) xb)

l23 : (Y ∨ X) ↔ (X ∨ Y)
l23 =(λ w → case (X ∨ Y) (λ y → inj₂ y) inj₁ w) , (λ xy → case (Y ∨ X) inj₂ inj₁ xy)

l24 : (X ∨ Y) → Z → (X → Z) ∧ (Y → Z)
l24 = λ xy → λ z → (λ x → z) , (λ y → z)

l25 : ¬ ¬ (X ∨ ¬ X)
l25 = λ f → f (inj₂ (λ x → f (inj₁ x))) 

l26 : (¬ X ∨ Y) → X → Y
l26 = λ nxy → λ x → case _ (λ nx → abort Y (nx x)) (λ y → y) nxy

l27 : ¬ (X ∨ Y) ↔ (¬ X ∧ ¬ Y)
l27 = (λ xny → (λ x → xny (inj₁ x)) , (λ y → xny (inj₂ y))) , (λ nxny → λ xvy → case _ (λ x → proj₁ nxny x) (λ y → proj₂ nxny y) xvy)

l28 : (¬ X ∨ ¬ Y) → ¬ (X ∧ Y)
l28 = λ nxvny → λ xy → case _ (λ nx → nx (proj₁ xy)) (λ ny → ny (proj₂ xy)) nxvny 
