# formalgo

## Preface

> "The notion of an algorithm is basic to all of computer programming,
> so we should begin with a careful analysis of this concept."
>
> (Donald Knuth - The Art of Computer Programming Vol. 1)

Once upon a time A. A. Markov produced The Theory of Algorithms and within it 
were wonderful explorations of said constructs, 
formalizing and analyzing their study.

I did not read that text, but Donald Knuth sure did, and decided to include the 
Markov algorithm in Secion 1.1 of his book The Art of Computer Programming, 
rendering me equally puzzled and intrigued.

It is truly a wonderful algorithm, so incredibly simple that anyone 
could realize it with pen and paper, filled to the brim with potential
both theoretical and practical. Enamored with the concept I sought out 
to implement an evaluator as well as an assembler for generating the algorithm, 
to experiment with and further understand it.

## The Algorithm

> "54. Beware of the Turing tar-pit in which everything is possible 
> but nothing of interest is easy" 
> 
> (Alan Perlis - Epigrams on Programming)

The Markov Algorithm is a simple set of steps of string manipulation.
We'll explore the algorithm that computes the difference between two numbers,
of the form

  - a<sup>x</sup>b<sup>y</sup> => a<sup>|x - y|</sup>

Take the following formalization:

| j   | theta | phi     | b   | a   |
| --- | ----- | ------- | --- | --- |
| 0   | ab    | (empty) | 0   | 1   | 
| 1   | b     | a       | 1   | 2   | 

And an initial string:

  - &sigma; = "aaabb",  where a<sup>x = 3</sup>b<sup>y = 2</sup>

We are going to go through the following states. 
(This should be skimmed and used as a reference as we go through the breakdown below.)

  - (&sigma;, j)
  1. ("aaabb", 0)
  2. ("aab", 0)  
  3. ("a", 0)    
  4. ("a", 1)    
  5. ("a", 2)

Where "a" is our final state, of the form a<sup>1</sup> = a<sup>|x - y|</sup>

**BREAKDOWN**

We start with step j=0 and look for the corresponding 
`theta` of that instruction in our string. We replace that `theta` with 
the corresponding `phi`.

  | j   | theta | phi     | b   | a   |
  | --- | ----- | ------- | --- | --- |
  | 0   | ab    | (empty) | 0   | 1   | 
  - aa(ab)b
  - aa()b
  - aab

Since we matched for `theta`, we move to instruction `b` which is still 0

  - ("aaabb", 0)
  - ("aab", `b`)
  - ("aab", 0)

We repeat this process for instruction 0

  - ("aab", 0) 
  - ("a", 0)

We once again repeat the process for instruction 0, but since we don't 
match for theta, we move onto `a` which is 1

  - ("a", 0)
  - ("a", 1)

We evaluate instruction 1, fail to match for `theta`, 
and move onto instruction 2

  | j   | theta | phi     | b   | a   |
  | --- | ----- | ------- | --- | --- |
  | 1   | b     | a       | 1   | 2   | 
  - ("a", 1)
  - ("a", 2)

Since there is no instruction 2<sup>\*</sup> we are done with our algorithm.

<sup>\*</sup> The formal algorithm requires an `N` for which the algorithm
terminates, but we implicitly assume `N = 2`.

**BONUS**

Here is the proper formalization of the algorithm itself, in all of it's 
esoteric glory:

  - f((&sigma;,j)) = (&sigma;,a<sub>j</sub>)              if &theta;<sub>j</sub> does not occur in &sigma;
  - f((&sigma;,j)) = (&alpha;&phi;&omega;,b<sub>j</sub>)  if &alpha; is the shortest possible string for which &sigma; = &alpha;&theta;<sub>j</sub>&omega;
  - f((&sigma;,N)) = (&sigma;,N)

Where 

  - *&sigma;* (sigma) is the current string.
  - *&alpha;* (alpha) is the matched portion of the string *before* the matched *&theta;* (theta).
  - *&omega;* (omega) is the matched portion of the string *after* the matched *&theta;*
  - *N* is the value *j* for which you are effectively done. 
    - In the explored example, *N* was implicitly 2.

## The Evaluator

> "eval('1 + 2')"
> 
> (Evaluation in Python, JavaScript, PHP, Perl, and various other languages)

To evaluate a formal algorithm we first make an appropriate .fa file.
We'll show what that might look like for the algorithm explored in 
the previous section. Consider this file analogous to a binary file, a 
format that is nearest to what the raw machine interprets.

NOTE: Remove all comments (anything including and after "//") 
from your final diff.fa file, the evaluator
does not support comments. 

- diff.fa
```
2 ab      
0 ab _ 0 1
1 b  a 1 2
```

Breaking down the lines

```
2 ab       // N = 2, A = "ab"

           // j theta  phi      b  a
0 ab _ 0 1 // 0 ab     (empty)  0  1
1 b  a 1 2 // 1 b      a        1  2
```

The first line `2 ab` corresponds to: 

  - N, the 'terminal' instruction = 2
  - A, the set of characters involved in the algorithm = "ab"

We evaluate this file by the simple command

```
> formalgo diff.fa
("", 0)
("", 1)
("", 2)
```

What? Basically nothing happened??

Well, formalgo's initial state is always the empty string, we need to populate 
it so that we can then do interesting things. (Much like normal computers, 
where every register is effectively uninitialized before an instruction
utilizes them)

We'll add a few instructions so we get to our "aaabb" string before we 
start the meat of the algorithm.

- diff.fa
```
4 ab       
0 _ bb  1 1
1 _ aaa 2 2
2 ab _ 2 3 
3 b  a 3 4 
```

```
> formalgo diff.fa
("",0)
("bb",1)    // we prepended b's with instruction 0 (because we match on the first empty string, which is just the beginning of the string)
("aaabb",2) // we prepended a's with instruction 1
("aab",2)
("a",2)
("a",3)
("a",4)
```

## The Assembler

> "I started out with machine code and assembly language"
> 
> (Charles Petzold)
