## Simple regex in haskell 

I was reading https://swtch.com/~rsc/regexp/regexp2.html recently and wanted to play around with this VM approach and compare it to the 
[derivative approach](https://matt.might.net/articles/implementation-of-regular-expression-matching-in-scheme-with-derivatives/)
as well as the [derivative to DFA approach](https://www.ccs.neu.edu/home/turon/re-deriv.pdf).

I was surprised to see that the naive interpreted derivaative approach was pretty fast, and went down a bit of rabbithole trying to understand why. 
I am still planning to implement a compiler to DFA based on the Owens et al paper as well as the vm approach. 
