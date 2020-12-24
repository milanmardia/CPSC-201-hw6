#lang racket

; ********************************************************************
; Name: Milan Mardia
; Email address: milan.mardia@yale.edu
; ********************************************************************

; CS 201a HW #6  DUE Wednesday, Nov 18 at 11:59 pm
; electronically, using the submit command.  
; Do *not* email your homework -- lateness penalties (5 points per day)
; will accrue until you successfully submit it using your Zoo account.

; Topics: strings, regular expressions, finite-state
; acceptors, and context free grammars

; Unless the problem specifies otherwise:
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.
; * Your procedures need *not* fail gracefully if their
; inputs do not satisfy the assumptions of the problem.
; * You may assume that the input is given as specified,
; you do not need to include tests in your code to verify
; that the input is valid.
; * Please use the specified names (and numbers of arguments)
; for the required procedures in a problem; this is to
; facilitate automatic testing.
; Use "The Racket Language" NOT R5RS.
; DO NOT use mutators. Only use syntax introduced in lectures or hw assignment files.

; ********************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment (including reading):

(define hours 13)

; ********************************************************************
; Representation of finite strings
; ********************************************************************
; Rather than use Racket strings to represent strings, which
; would limit the set of symbols to ASCII characters, we
; define our own representation of Finite Strings as follows.

; A Finite String of symbols is represented by a list of Racket symbols.
; Thus, (a b a b b a) represents a Finite String with six symbols,
; the first, third and sixth being a's and the rest b's.
; We'll also consider (the dog barked at the other dog) 
; as a Finite String of seven symbols.
; The empty Finite String is denoted by the empty list ().
; Note that (append str1 str2) concatenates two
; Finite Strings str1 and str2.
; 
; ********************************************************************
; ** problem 1 ** (11 points)
; Define a procedure (product lst1 lst2)
; that takes two lists lst1 and lst2 of Finite Strings
; and returns a list of all the strings that
; can be obtained by concatenating a string
; from lst1 with a string from lst2.
; The returned list should contain no duplicates.

; Examples
; (product '() '((a) (a b))) => '()
; (product '(()) '((a) (a b))) => '((a) (a b))
; (product '((a) (a b)) '((c d))) => '((a c d) (a b c d))
; (product '((a) (a a)) '((a) (a a))) => '((a a) (a a a) (a a a a))
; (product '((a b) (b c)) '((a b) (d e))) =>
;    '((a b a b) (a b d e) (b c a b) (b c d e))
; (product '(()) '(())) => '(())
; (product '((the)) '((small dog) (large cat))) =>
;     '((the small dog) (the large cat))
; (product '((small) (large)) '((dog) (cat))) =>
;     '((small dog) (small cat) (large dog) (large cat))


(define product-helper
  (lambda (lst1 lst2)
    (cond ((equal? lst1 '()) '())
          (else (append (single (first lst1) lst2) (product-helper (rest lst1) lst2))))))

(define single
  (lambda (lst lst-of-lst)
    (cond ((null? lst-of-lst) '())
          (else (cons (append lst (first lst-of-lst)) (single lst (rest lst-of-lst)))))))

(define product
  (lambda (lst1 lst2)
    (let ((lst (product-helper lst1 lst2)))
      (remove-duplicates lst))))


; ********************************************************************
; Representation of Regular Expressions
; ********************************************************************
; Regular Expressions will be represented as follows.
; 1) A Finite String (defined above) is a regular expression; for these
; purposes, the string may not contain the symbols !, +, or *.
; 2) The concatenation of two or more expressions is represented by
; a list starting with the symbol ! and containing the expressions.
; 3) The union of two or more expressions is represented
; by a list starting with + and containing the expressions.
; 4) The Kleene star of one expression is represented by a list
; starting with * and containing the expression to be starred.

; Some examples of regular expressions in this representation:

(define exp0 '())
(define exp1 '(a a b a b))
(define exp2 '(+ (a b) (b)))
(define exp3 '(! (* (b)) (a) (* (+ (b) (! (a) (* (b)) (a))))))
(define exp4 '(! (alas) (+ (and) (or)) (alack)))
(define exp5 '(! (the sleeping) (+ (dog) (cat) (beast))))
(define exp6 '(* (+ (a) (b))))
(define exp7 '(! (a) (* (+ (very) (quite))) (long) (+ (story) (tale))))

; ********************************************************************
; ** problem 2 ** (11 points)
; Write predicate and selector procedures for regular expressions exp.
; You may assume that the input is Regular Expression defined as above.

; (re-string? exp) should return #t iff exp is a Finite String
; (re-concatenation? exp) should return #t iff exp is a concatenation
; (re-union? exp) should return #t iff exp is a union
; (re-star? exp) should return #t iff exp is a Kleene star expression
; otherwise, #f should be returned by these functions

; If exp is a concatenation then
; (op exp) should return the symbol ! and (args exp) should return the list
; of its arguments
; If exp is a union then
; (op exp) should return the symbol + and (args exp) should return the list
; of its arguments
; If exp is a Kleene star expression, then
; (op exp) should return the symbol * and (arg exp) should return its argument

; After this problem, use these procedures to test the type and
; refer to the parts of regular expressions.

; Examples
; (re-string? exp0) => #t
; (re-string? exp1) => #t
; (re-string? exp2) => #f
; (re-string? exp4) => #f
; (re-concatenation? exp0) => #f
; (re-concatenation? exp1) => #f
; (re-concatenation? exp2) => #f
; (re-concatenation? exp3) => #t
; (re-concatenation? exp6) => #f
; (re-union? exp0) => #f
; (re-union? exp1) => #f
; (re-union? exp2) => #t
; (re-union? exp3) => #f
; (re-star? exp0) => #f
; (re-star? exp1) => #f
; (re-star? exp2) => #f
; (re-star? exp6) => #t
; (op exp2) => '+
; (op exp3) => '!
; (op exp6) => '*
; (args exp2) => '((a b) (b))
; (args exp3) => '((* (b)) (a) (* (+ (b) (! (a) (* (b)) (a)))))
; (arg exp6) => '(+ (a) (b))

(define re-string?
  (lambda (exp)
    (or (null? exp) (not (list? exp)) (and (not (equal? (first exp) '+)) (not (equal? (first exp) '*)) (not (equal? (first exp) '!))))))
(define re-concatenation?
  (lambda (exp)
    (cond ((null? exp) #f)
          (else (equal? (first exp) '!)))))
(define re-star?
  (lambda (exp)
    (cond ((null? exp) #f)
          (else (equal? (first exp) '*)))))
(define re-union?
  (lambda (exp)
    (cond ((null? exp) #f)
          (else (equal? (first exp) '+)))))
(define op
  (lambda (exp)
    (cond ((re-star? exp) '*)
          ((re-union? exp) '+)
          ((re-concatenation? exp) '!)
          (else '()))))
(define args
  (lambda (exp)
    (cond ((re-string? exp) exp)
          ((re-star? exp) (first (rest exp)))
          (else (rest exp)))))

(define arg
  (lambda (exp)
    (cond ((re-star? exp) (first (rest exp)))
          (else #f))))
          
; ********************************************************************
; ** problem 3 ** (11 points)
; Write two procedure (pick lst) and (pick-string exp)
; as follows.

; (pick lst) takes a list lst and
; returns a uniformly chosen random element from lst.
; If lst is the null list, your procedure should return #f.

; Recall Racket procedure
; (random n) returns a randomly chosen
; integer between 0 and n-1 inclusive.

; (pick-string exp) takes a regular expression exp
; and returns a randomly chosen Finite String in the
; language of exp.  Just how you do this is up to you,
; but your procedure should be capable of generating
; any Finite String in the language, and none that aren't.

; You will want to use recursion on the structure of the
; expression for pick-string.

; Examples (Your mileage may vary!):
; (pick '(a b c)) => 'a
; (pick '(a b c)) => 'c
; (pick '(b)) => 'b
; (pick '(a a a b b b c c)) => 'b
; (pick '(5 7 9)) => 5
; (pick '(5 7 9)) => 9
; (pick '()) => #f
; (pick-string exp0) => '()
; (pick-string exp1) => '(a a b a b)
; (pick-string exp2) => '(b)
; (pick-string exp2) => '(a b)
; (pick-string exp3) => '(b a b a a)
; (pick-string exp3) => '(b b b b a b b b a b b b b b a)
; (pick-string exp3) => '(a)
; (pick-string exp4) => '(alas or alack)
; (pick-string exp4) => '(alas and alack)
; (pick-string exp5) => '(the sleeping dog)
; (pick-string exp5) => '(the sleeping cat)
; (pick-string exp6) => '(b a b a a a)
; (pick-string exp6) => '(a a a b)
; (pick-string exp6) => '(b b)
; (pick-string exp7) => '(a quite quite very very very long tale)
; (pick-string exp7) => '(a long story)


(define pick
  (lambda (lst)
    (if (null? lst)
        #f
        (list-ref lst (random (length lst))))))


(define pick-string
  (lambda (lst)
    (cond ((null? lst) '())
          ((re-string? lst) lst)
          ((re-union? lst) (pick-string (pick (args lst))))
          ((re-concatenation? lst) (append* (map pick-string (args lst))))
          (else  (if (= (random 3) 0)
                               '()
                               (append (pick-string (args lst)) (pick-string lst)))))))

; ********************************************************************
; ** problem 4 ** (11 points)
; Write a procedure (reverse-exp exp) 
; to take a regular expression exp
; (in the representation above) and return another
; regular expression exp1 such that L(exp1) contains
; exactly the reverses of all the strings in L(exp).
; (Therefore the reverse of a regular language is regular.)

; Examples (Your mileage may vary!):
; (reverse-exp exp0) => '()
; (reverse-exp exp1) => '(b a b a a)
; (pick-string (reverse-exp exp2)) => '(b a)
; (pick-string (reverse-exp exp3)) => '(a b b b b b b b b b b a a)
; (pick-string (reverse-exp exp3)) => '(a)
; (pick-string (reverse-exp exp4)) => '(alack and alas)
; (pick-string (reverse-exp exp4)) => '(alack or alas)
; (pick-string (reverse-exp exp5)) => '(dog sleeping the)
; (pick-string (reverse-exp exp6)) => '(a a a b b)
; (pick-string (reverse-exp exp6)) => '()
; (pick-string (reverse-exp exp7)) => '(tale long a)
; (pick-string (reverse-exp exp7)) => '(story long quite very quite a)

;(define exp0 '())
;(define exp1 '(a a b a b))
;(define exp2 '(+ (a b) (b)))
;(define exp3 '(! (* (b)) (a) (* (+ (b) (! (a) (* (b)) (a))))))
;(define exp4 '(! (alas) (+ (and) (or)) (alack)))
;(define exp5 '(! (the sleeping) (+ (dog) (cat) (beast))))
;(define exp6 '(* (+ (a) (b))))
;(define exp7 '(! (a) (* (+ (very) (quite))) (long) (+ (story) (tale))))


(define reverse-exp
  (lambda (lst)
    (cond ((null? lst) '())
          ((re-string? lst) (reverse lst))
          ((re-concatenation? lst) (append '(!) (reverse (map reverse-exp (args lst)))))
          ((re-star? lst) (append '(*) (map reverse-exp (list (args lst)))))
          ((re-union? lst) (append '(+) (map reverse-exp (args lst)))))))


; ********************************************************************
; Representation of finite state acceptors
; ********************************************************************
; A Finite State Acceptor is represented by a list of
; three items: the start state, the list of accepting states,
; and a table giving the transition function.  Each entry
; in the table consists of a key (a list consisting
; of a state and a symbol) and a value (a state).

; For example, here is a Finite State Acceptor
; that accepts all strings of a's and b's with
; an odd number of a's:

(define fsa1 
  '(q0 (q1) (((q0 a) q1)  ((q1 a) q0)
	     ((q0 b) q0)  ((q1 b) q1))))

; Here is another acceptor, which accepts strings of a's and strings
; of b's, but no string that contains both a's and b's.

(define fsa2
  '(q0 (q0 q1 q2) (((q0 a) q1)  ((q1 a) q1)  ((q2 a) q3)  ((q3 a) q3)
		   ((q0 b) q2)  ((q1 b) q3)  ((q2 b) q2)  ((q3 b) q3))))


; ********************************************************************
; ** problem 5 ** (11 points)
; Write a procedure (accepts? fsa str)
; that takes a finite state acceptor fsa (as just specified)
; and a Finite String str,
; and returns #t if the acceptor accepts the string, #f otherwise.

; Examples:
; (accepts? fsa1 '(a b b a b a)) => #t
; (accepts? fsa1 '(b b b)) => #f
; (accepts? fsa1 '(b a b a)) => #f
; (accepts? fsa1 '(a)) => #t
; (accepts? fsa2 '()) => #t
; (accepts? fsa2 '(a a a a)) => #t
; (accepts? fsa2 '(a a b b)) => #f
; (accepts? fsa2 '(b b)) => #t
; (accepts? fsa2 '(b a b)) => #f
(define lookup
  (lambda (finder lst)
    (cond ((null? lst) #f)
          ((equal? (first (first lst)) finder) (first (rest (first lst)))) ;returns a symbol
          (else (lookup finder (rest lst))))))

(define accepts?
  (lambda (fsa str)
    (cond ((null? str) (if (list? (member (first fsa) (first (rest fsa))))
                           #t
                           #f))
          (else (accepts? (cons (lookup (list (first fsa) (first str)) (list-ref fsa 2)) (rest fsa)) (rest str))))))

; ********************************************************************
; ** problem 6 (11 points)
; Define a procedure (complement fsa)
; that takes a Finite State Acceptor
; and returns a Finite State Acceptor
; that accepts the strings that fsa
; rejects and vice versa.
; (Therefore the complement of a regular language is regular.)

; Examples
; (accepts? (complement fsa1) '(a b b a b a)) => #f
; (accepts? (complement fsa1) '(b b b)) => #t
; (accepts? (complement fsa2) '()) => #f
; (accepts? (complement fsa2) '(a a a a)) => #f
; (accepts? (complement fsa2) '(a a b b)) => #t


(define gather-helper
  (lambda (lst)
    (cond ((null? lst) '())
          (else (cons (first (rest (first lst))) (gather-helper (rest lst)))))))

(define gather
  (lambda (lst)
    (remove-duplicates (gather-helper lst))))
          
(define complement-helper
  (lambda (lst1 lst2)
    (cond ((null? lst2) '())
          ((not (list? (member (first lst2) lst1))) (cons (first lst2) (complement-helper lst1 (rest lst2))))
          (else (complement-helper lst1 (rest lst2))))))
(define complement
  (lambda (fsa)
    (let ((x (complement-helper (list-ref fsa 1) (gather (list-ref fsa 2)))))
      (list (first fsa) x (list-ref fsa 2)))))


; ********************************************************************
; Representation of context free grammars
; ********************************************************************
; A Context Free Grammar is a list of two items:
; the start symbol of the grammar, and the list
; of rules of the grammar.

; Each rule is a list of two items:
; a nonterminal of the grammar (the lefthand side of the rule) and
; a Finite String of terminals or nonterminals of the grammar
; (the righthand side of the rule).
; We will assume that the nonterminals of the grammar
; are those that appear on the lefthand side of a rule, and
; that symbols that appear on the righthand side of some
; rule but not on the lefthand side of any rule are terminal symbols.

; Here are the rules of a grammar for palindromes
; over the alphabet {a, b}.


(define rule1 '(s ()))
(define rule2 '(s (a)))
(define rule3 '(s (b)))
(define rule4 '(s (a s a)))
(define rule5 '(s (b s b)))

; Here is the corresponding Context Free Grammar

(define cfg1 (list 's (list rule1 rule2 rule3 rule4 rule5)))

; This is a grammar for a fragment of English

(define rule2-1 '(<sentence> (<subject> <verb1>)))
(define rule2-2 '(<sentence> (<subject> <verb2> that <sentence>)))
(define rule2-3 '(<subject> (<article> <noun>)))
(define rule2-4 '(<subject> (<pronoun>)))
(define rule2-5 '(<noun> (woman)))
(define rule2-6 '(<noun> (man)))
(define rule2-7 '(<noun> (truth)))
(define rule2-8 '(<noun> (lizard)))
(define rule2-9 '(<pronoun> (it)))
(define rule2-10 '(<pronoun> (he)))
(define rule2-11 '(<pronoun> (she)))
(define rule2-12 '(<article> (the)))
(define rule2-13 '(<article> (a)))
(define rule2-14 '(<article> (every)))
(define rule2-15 '(<verb1> (exists)))
(define rule2-16 '(<verb1> (swims)))
(define rule2-17 '(<verb1> (pauses)))
(define rule2-18 '(<verb2> (believes)))
(define rule2-19 '(<verb2> (hopes)))
(define rule2-20 '(<verb2> (fears)))

; and the grammar:

(define cfg2
  (list '<sentence>
	(list rule2-1 rule2-2 rule2-3 rule2-4 rule2-5 rule2-6 rule2-7
	      rule2-8 rule2-9 rule2-10 rule2-11 rule2-12 rule2-13 rule2-14
	      rule2-15 rule2-16 rule2-17 rule2-18 rule2-19 rule2-20)))

; ********************************************************************
; ** problem 7 ** (11 points)

; Write a Context Free Grammar named
;     my-cfg
; in this representation
; to generate a set of strings that form one of the following:

; a.) An entry for the Bulwer-Lytton contest https://www.bulwer-lytton.com
;  Your sentence may be simpler, but the original sentence that inspired the contest was:
; 'It was a dark and stormy night; the rain fell in torrents —
; except at occasional intervals, when it was checked by a violent
; gust of wind which swept up the streets (for it is in London that our scene lies),
; rattling along the housetops, and fiercely agitating the scanty flame of the lamps that
; struggled against the darkness."
;
; b.) A brief description of a fictional Yale building similar to:
; https://visitorcenter.yale.edu/tours/architecture-yale
; Your description does not have to be as long, but should include the building
; name, year, architect as well as brief text. For example brief text for
; the Beinecke Library could be "At first decried by many as bombastic, it now
; has a strangely quiet presence in the midst of the vast granite-paved
; expanse of its plaza, punctuated by a sunken courtyard"

; c.) A "Blue Book" entry 
; This could including course number, title, and brief description, e.g.
; a string your grammar might produce would be:
; CPSC 201
; Introduction to Computer Science
; An exciting overview of the most important topic taught at Yale, illuminating
; the intellectual adventure inherent in learning Racket programming.
; 
; Your strings do not have to follow these patterns exactly,
; but the strings your grammar produces should be at least as complex as
; those produced cfg2. 
; You do not have to be concerned with punctuation or separating the entries into
; lines. Have fun with this!!!


(define rule3-1 '(<sentence> (<subject> <verb1>)))
(define rule3-2 '(<sentence> (<subject> <predicate> that <sentence>)))
(define rule3-3 '(<sentence> (<subject> <verb1> <conjunction> <sentence>)))
(define rule3-3a '(<predicate> (<adverb> <verb2>)))
(define rule3-3b '(<predicate> (<verb2>)))
(define rule3-4 '(<subject> (<article> <noun>)))
(define rule3-4a '(<subject> (<article> <adjective> <noun>)))
(define rule3-5 '(<subject> (<pronoun>)))
(define rule3-6 '(<noun> (monstor)))
(define rule3-7 '(<noun> (torrent)))
(define rule3-8 '(<noun> (abyss)))
(define rule3-9 '(<noun> (sea)))
(define rule3-10 '(<noun> (night)))
(define rule3-11 '(<noun> (day)))
(define rule3-12 '(<pronoun> (it)))
(define rule3-13 '(<pronoun> (he)))
(define rule3-14 '(<pronoun> (she)))
(define rule3-15 '(<article> (the)))
(define rule3-16 '(<article> (a)))
(define rule3-17 '(<article> (every)))
(define rule3-18 '(<verb1> (exists)))
(define rule3-19 '(<verb1> (swims)))
(define rule3-20 '(<verb1> (pauses)))
(define rule3-21 '(<verb2> (believes)))
(define rule3-22 '(<verb2> (hopes)))
(define rule3-23 '(<verb2> (fears)))
(define rule3-24 '(<adjective>  (gloomy)))
(define rule3-25 '(<adjective>  (happy)))
(define rule3-26 '(<adjective>  (scary)))
(define rule3-27 '(<adjective>  (exciting)))
(define rule3-28 '(<adjective>  (deep)))
(define rule3-29 '(<adjective>  (threatening)))
(define rule3-30 '(<adjective>  (threatening)))
(define rule3-31 '(<adverb>  (strongly)))
(define rule3-32 '(<adverb>  (often)))
(define rule3-33 '(<adverb>  (reluctantly)))
(define rule3-34 '(<conjunction>  (because)))
(define rule3-35 '(<conjunction>  (and)))
(define rule3-36 '(<conjunction>  (but)))
(define rule3-37 '(<conjunction>  (since)))

(define my-cfg
  (list '<sentence>
	(list rule3-1 rule3-2 rule3-3 rule3-3a rule3-3b rule3-4 rule3-4a rule3-5 rule3-6 rule3-7
	      rule3-8 rule3-9 rule3-10 rule3-11 rule3-12 rule3-13 rule3-14
	      rule3-15 rule3-16 rule3-17 rule3-18 rule3-19 rule3-20 rule3-21 rule3-22 rule3-23 rule3-24 rule3-25 rule3-26 rule3-27
	      rule3-28 rule3-29 rule3-30 rule3-31 rule3-32 rule3-33 rule3-34 rule3-35 rule3-36 rule3-37)))







; ********************************************************************
; ** problem  8 ** (22 points)
; Write a procedure (generate cfg)
; to generate a random Finite String
; in the language of the Context Free Grammar cfg.
; Every string in the language of the grammar should
; be a possible output of your program, and no string
; not in the language of the grammar should be a possible output.
; You may assume that every nonterminal in the grammar
; generates at least one string of terminals.
; (This is to avoid problems like (s (s (t)) (t (s))).)
; Recall the procedure (pick lst) that you wrote above.

; Please include explanations of how your procedures work.

; Examples:
; (generate cfg1) => '()
; (generate cfg1) => '(a b b b a)
; (generate cfg1) => '(a b a b b b a b a)
; (generate cfg2) => '(a lizard exists)
; (generate cfg2) => '(a truth hopes that she pauses)
; (generate cfg2) => '(he exists)
; (generate cfg2) => '(it exists)
; (generate cfg2) => '(she exists)
; (generate cfg2) => '(he fears that the man believes that it pauses)
; (generate cfg2) => '(the woman hopes that she exists)
; (generate cfg2) => '(it pauses)
; (generate cfg2) => '(she believes that every man hopes that it exists)


;gathers all non-terminals
(define gather-cfg-helper
  (lambda (lst)
    (cond ((null? lst) '())
          (else (cons (first (first lst)) (gather-cfg-helper (rest lst)))))))

;removes duplicates from the above function
(define gather-cfg
  (lambda (cfg)
    (remove-duplicates (gather-cfg-helper (list-ref cfg 1)))))

;creates list for each individual key
(define create-dictionary-cfg-helper
  (lambda (key lst-of-rules)
    (cond ((null? lst-of-rules) '())
          ((equal? key (first (first lst-of-rules))) (cons (first (rest (first lst-of-rules))) (create-dictionary-cfg-helper key (rest lst-of-rules))))
          (else (create-dictionary-cfg-helper key (rest lst-of-rules))))))

;creates dictionary with each non-termianl symbol as the key and each thing it could output as the value
(define create-dictionary-cfg
  (lambda (lst-of-keys lst-of-rules)
    (cond ((null? lst-of-keys) '())
          (else (cons (list (first lst-of-keys) (create-dictionary-cfg-helper (first lst-of-keys) lst-of-rules)) (create-dictionary-cfg (rest lst-of-keys) lst-of-rules))))))

;simplify create-dictionary-cfg
(define create-dictionary
  (lambda (cfg)
    (create-dictionary-cfg (gather-cfg cfg) (list-ref cfg 1))))

;outputs the list for the target symbol
(define lookup1
  (lambda (target lst)
    (cond ((null? lst) #f)
          ((equal? (first (first lst)) target) (first (rest (first lst))))
          (else (lookup1 target (rest lst))))))
;alterante version of map1 that applies generate-heler to each member in lst
(define map1
  (lambda (lst cfg)
    (cond ((null? lst) '())
          (else (cons (generate-helper (first lst) cfg) (map1 (rest lst) cfg))))))

;if the symbol is non-terminal, it recursively uses map1 and generate-helper until it reaches a base case of when the symbol is terminal
(define generate-helper
  (lambda (symbol cfg)
    (cond ((list? (member symbol (gather-cfg cfg))) (append* (map1 (pick (lookup1 symbol (create-dictionary cfg))) cfg)))
          (else (list symbol)))))
;simplifies the generat-heleper function so that it only takes in one input
(define generate
  (lambda (cfg)
    (generate-helper (first cfg) cfg)))


; ********************************************************************
; End of homework #6
; ********************************************************************
