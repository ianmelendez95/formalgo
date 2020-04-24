# formalgo

## Preface

Once upon a time A. A. Markov produced The Theory of Algorithms and within it 
were wonderful explorations of algorithms, formalizing and analyzing their study.
I did not read that text, but Donald Knuth sure did, and decided to include the 
Markov algorithm in Secion 1.1 of his book The Art of Computer Programming, 
rendering me equally puzzled and intrigued.

It is truly a wonderful algorithm, so incredibly simple that anyone 
could realize it with pen and paper and filled to the brim with potential
both theoretically and practically. Enamored with the concept I sought out 
to implement an evaluator as well as an assembler for generating the algorithm, 
to experiment with and further understand it.

## The Algorithm

The Markov Algorithm is a simple set of steps of string manipulation.
We'll explore the algorithm that computes the difference between two numbers,
of the form

    a<sup>x</sup>b<sup>y</sup> => a<sup>|x - y|</sup>

Take the following formalization:

    | j   | theta | phi     | b   | a   |
    | --- | ----- | ------- | --- | --- |
    | 0   | ab    | (empty) | 0   | 1   | 
    | 1   | b     | a       | 1   | 2   | 

And an initial string:

    aaabb = a<sup>x = 3</sup>b<sup>y = 2</sup>

So say you're on step 0. Look for the instruction for j = 0. 

Then you are going to look for `theta` of that instruction in your string.

    aa(ab)b

Now replace that `theta` with corresponding `phi`. 
In our case we replace it with the empty string.

    aab

Since we found `theta` we go to instruction `b`.
(If we hadn't matched we would have gone to `a`, more on that later.)

Since `b` is zero for instruction 0, we repeat instruction 0, so

    aab -> a

Then once again repeat instruction 0.
This time the string "a" does not contain 
`theta`, so we go to `a` which is instruction 1

For instruction 1, the string "a" does not contain 'b', so we go to instruction
`a` again. Since there is no instruction 2, we are done, and our answer is
correctly of the form

    a = a<sup>1</sup> = a<sup>|3 - 2|</sup>

As an exercise, in what case would the instruction 1 been effective - what 
relation would x have to y for there to have been any b characters left?

**BONUS**

Here is the proper formalization of the algorithm itself, in all of it's 
esoteric glory.

    f((&sigma;,j)) = (&sigma;,a<sub>j</sub>)              if &theta;<sub>j</sub> does not occur in &sigma;
    f((&sigma;,j)) = (&alpha;&phi;&omega;,b<sub>j</sub>)  if &alpha; is the shortest possible string for which &sigma; = &alpha;&theta;<sub>j</sub>&omega;
    f((&sigma;,N)) = (&sigma;,N)

Where *&sigma;* (sigma) is the current string.

*&alpha;* (alpha) is the matched portion of the string *before* the matched *&theta;* (theta).

*&omega;* (omega) is the matched portion of the string *after* the matched *&theta;*

*N* is the value *j* for which you are effectively done. 
In the explored example, *N* was implicitly 2.

