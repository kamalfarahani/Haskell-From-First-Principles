# Exercises

## Intermission: Equivalence Exercises

We’ll give you a lambda expression. Keeping in mind both alpha
equivalence and how multiple heads are nested, choose an answer
that is equivalent to the listed lambda term.

1. $\lambda x y. x z$
    * $\lambda x z. x z$
    * $\lambda m n. m z$ ✅
    * $\lambda z(\lambda x. x z)$
2. $\lambda xy. x x y$
    * $\lambda m n. m n p$
    * $\lambda x.(\lambda y. x y)$
    * $\lambda a.(\lambda b. a a b)$ ✅
3. $\lambda x y z. z x$
    * $\lambda x. (\lambda y. (\lambda z. z))$
    * $\lambda t o s. s t$ ✅
    * $\lambda m n p. m n$


## Chapter 1: Exercises

### Combinatorics
Determine if each of the following are combinators or not.

1. $\lambda x. x x x$ ✅
2. $\lambda x y. z x$ ❌
3. $\lambda x y z. x y (z x)$ ✅
4. $\lambda x y z. x y (z x y)$ ✅
5. $\lambda x y. x y (z x y)$ ❌

## Normal form or divergence?

Determine if each of the following can be reduced to a normal form or if they diverge.

1. $\lambda x. x x x$ ✅ normal form is $\lambda x. x x x$
2. $(\lambda z. z z) (\lambda y. y y)$ ❌ diverges
3. $(\lambda x. x x x) z$ ✅ normal form is $z$

## Beta reduction

Beta reduce Evaluate (that is, beta reduce) each of the following
expressions to normal form. We strongly recommend writing out
the steps on paper with a pencil or pen.

1. $(\lambda a b c. c b a) z z (\lambda w v. w) \to ((\lambda w v. w) z z) \to (z z)$
2. $(\lambda x.\lambda y.xyy)(\lambda a.a)b \to  (\lambda a. a) b b \to b b$
3. $(\lambda y.y)(\lambda x.xx)(\lambda z.zq) \to (\lambda x.xx) (\lambda z.zq) \to (\lambda z.zq) (\lambda z.zq) \to (\lambda z.zq) q \to qq$
4. $(\lambda z.z)(\lambda z.zz) (\lambda z.zy) \to (\lambda z.zz) (\lambda z.zy) \to (\lambda z.zy) (\lambda z.zy) \to (\lambda z.zy) y \to yy$
5. $(\lambda x.\lambda y.xyy) (\lambda y.y) y \to (\lambda y.y) y y \to y y$
6. $(\lambda a.aa) (\lambda b.ba) c \to (\lambda b.b a)(\lambda b. b a) c \to (\lambda b. b a) a  c\to aac$
7. $(\lambda xyz.xz(yz)) (\lambda x.z) (\lambda x.a) \to (\lambda z_1. (\lambda x. z) z ((\lambda x. a) z) \to (\lambda z_1. z a)$