data Variable = Variable String
  deriving (Show, Eq)

data Predicate = Predicate String
  deriving (Show, Eq)

data Expr = Atomic Predicate [Variable]
          | Not Expr
          | Expr :|: Expr
          | Expr :&: Expr
          | Forall Variable Expr
          | Exists Variable Expr
  deriving (Show, Eq)

imp :: Expr -> Expr -> Expr
imp p q = Not p :|: q

iff :: Expr -> Expr -> Expr
iff p q = (p `imp` q) :&: (q `imp` p)

ap :: String -> [String] -> Expr
ap p xs = Atomic (Predicate p) (map Variable xs)

data PrenexBody = PAtomic Predicate [Variable]
                | PNot PrenexBody
                | POr PrenexBody PrenexBody
                | PAnd PrenexBody PrenexBody
  deriving (Show, Eq)

data Quantifier = QForall Variable | QExists Variable
  deriving (Show, Eq)

data Prenex = Prenex [Quantifier] PrenexBody
  deriving (Show, Eq)

flipQuantifier :: Quantifier -> Quantifier
flipQuantifier (QForall x) = QExists x
flipQuantifier (QExists x) = QForall x

prenex :: Expr -> Prenex
prenex (Atomic p xs) = Prenex [] (PAtomic p xs)

prenex (Not p)       = let Prenex quants p' = prenex p
                       in Prenex (map flipQuantifier quants) (PNot p')
                     
prenex (p :|: q)     = let Prenex quantsp' p' = prenex p
                           Prenex quantsq' q' = prenex q
                       in Prenex (quantsp' ++ quantsq') (POr p' q')
                     
prenex (p :&: q)     = let Prenex quantsp' p' = prenex p
                           Prenex quantsq' q' = prenex q
                       in Prenex (quantsp' ++ quantsq') (PAnd p' q')
                       
prenex (Forall x p)  = let Prenex quantsp' p' = prenex p
                       in Prenex (QForall x:quantsp') p'

prenex (Exists x p)  = let Prenex quantsp' p' = prenex p
                       in Prenex (QExists x:quantsp') p'

data SignedPrenexBody = SPAtomic Bool Predicate [Variable]
                      | SPOr SignedPrenexBody SignedPrenexBody
                      | SPAnd SignedPrenexBody SignedPrenexBody
  deriving (Show, Eq)

dual :: SignedPrenexBody -> SignedPrenexBody
dual (SPAtomic b p xs) = SPAtomic (not b) p xs
dual (p `SPOr` q)      = dual p `SPAnd` dual q
dual (p `SPAnd` q)     = dual p `SPOr` dual q

makeSigned :: PrenexBody -> SignedPrenexBody
makeSigned (PAtomic p xs) = SPAtomic True p xs
makeSigned (p `POr` q)    = makeSigned p `SPOr` makeSigned q
makeSigned (p `PAnd` q)   = makeSigned p `SPAnd` makeSigned q
makeSigned (PNot p)       = dual (makeSigned p)

data Signed = Signed Bool Predicate [Variable]
  deriving (Show, Eq)

data Conjunction = Conjunction [Signed]
  deriving (Show, Eq)

data Disjunction = Disjunction [Conjunction]
  deriving (Show, Eq)

conj' :: Conjunction -> Conjunction -> Conjunction
Conjunction ps `conj'` Conjunction qs = Conjunction (ps ++ qs)

disj :: Disjunction -> Disjunction -> Disjunction
Disjunction dps `disj` Disjunction dqs = Disjunction (dps ++ dqs)

conj :: Disjunction -> Disjunction -> Disjunction
Disjunction dps `conj` Disjunction dqs = Disjunction [ps `conj'` qs | ps <- dps, qs <- dqs]

makeDNF :: SignedPrenexBody -> Disjunction
makeDNF (SPAtomic b p xs) = Disjunction [Conjunction [Signed b p xs]]
makeDNF (p `SPOr` q)      = makeDNF p `disj` makeDNF q
makeDNF (p `SPAnd` q)     = makeDNF p `conj` makeDNF q

data PrenexDNF = PrenexDNF [Quantifier] Disjunction
  deriving (Show, Eq)

exprToPrenexDNF :: Expr -> PrenexDNF
exprToPrenexDNF p = let Prenex quants p' = prenex p
                    in PrenexDNF quants (makeDNF (makeSigned p'))

p = Not ((ap "A" [] :|: ap "B" []) :&: Forall (Variable "x") (ap "C" ["x"])) :|: (ap "D" [] :|: ap "E" [])
--  ~((A v B) & (forall x. C x)) v D v E

main = putStr $ show $ exprToPrenexDNF p
-- exists x. (~ A & ~ B) v (~ C x) v D v E