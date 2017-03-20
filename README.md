# quantum-programming-in-Quipper

This reprository contains some quantum programs in Quipper. 
Specifically, it contains 
1. hello_quantum_world.hs 
2. entanglement.hs
3. deutsch.hs 
4. deutsch_jozsa.hs 
5. grover_search.hs
6. linear combination of unitary algorithm.

Programs in 3-5 is based on the work in arXiv:1406.4481v2, while I added more oracles and simulations into them. I also 
modified some specific details, like the number of oracles used in grover search.

Quantum programming is fundamentally different from classical programming. It's not like using command to tell the computer 
what to do, rather treat the computer like nature. We prepare initial states, apply gates to it(evolve it), and finally
measure it to get the results. Sounds more like simulating nature in the (quantum) computer, aha? You may want to read some
other comments from American Scientist and New Scientist.

American Scientist: http://www.americanscientist.org/issues/pub/2014/1/programming-your-quantum-computer

New Scientist: https://www.newscientist.com/article/dn23820-new-language-helps-quantum-coders-build-killer-apps#.UdbSHOEQ60z

Quipper is hosted in Haskell, which is a functional language that is very different from imperative languages(C, C++, Java,
Fortran). A large part of functional lanuage is expressions and functions, and no commands like in the imperative languages.
If you have worked in imperative language a long time, it may be a little more difficult for you to shift your mind.(I go
through this too.) :)

The official website for Quipper is http://www.mathstat.dal.ca/~selinger/quipper/. In this website you can also learn to 
install Quipper(Win, Mac, Linux). Note that during the process of installation, you may probably encounter an "annoying"
problem, which is called "cabal hell". One solution for it can be found at
https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md. By the way, I used Emacs to edit, and command line
to compile and run programs, because majority of people used Emacs for haskell development, and the above link gives a 
detailed instruction to set up Emacs.

For those who had no experience programming in any functional language before, wikibook "Learn you a haskell for great good"
is a good start point. English Version can be found at http://learnyouahaskell.com/chapters, and Chinese version at 
http://www.code123.cc/docs/haskell-notes/index.html.

Have fun in the world of quantum programming! :)

qWalker
