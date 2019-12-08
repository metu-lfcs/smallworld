;;; lexicon file

; proper names and definite desc.
n[bar=2,sg]			: $ 							< john mary bill sue 

; nouns

n[0]					: (lam x ($ x))					< pianist spy prof biologist

; verbs

s\np[sg]            : (lam x ($ x))					< sleeps walks works talks
s\np[pl]			: (lam x ($ x))					< sleep walk work talk
(s\np[sg])/np		: (lam x (lam y (($ x) y))) 	< loves hates chases
(s\np[pl])/np		: (lam x (lam y (($ x) y))) 	< love hate chase

; copula

(s\np[sg])/adj				: (lam x ($ x))								< is was
(s\np[pl])/adj				: (lam x ($ x))								< am are were
(s\np[sg])/n[1]				: (lam x ($ x))								< is was
(s\np[pl])/n[1]				: (lam x ($ x))								< am are were 

; measurable gradable adjectives

adj						    : (lam x ($ x))							< happy strange   
n/n 		 				: (lam x ((mod $) x)) 					< happy strange

; measure phrase
; these are ap specifiers -- an ap can get specified by an MP or an intensifier like 'very' or yet by an adverb
; the evidence for these being specifiers rather than modifiers is that non of them can be cascaded.


; predicative determiners

n[1]/n						: (lam x ($ x))							< a

; quantifiers 

(s/(s\np))/n				: (lam p (lam q (($ p) q)))			< every some no
