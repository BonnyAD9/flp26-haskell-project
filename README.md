# Haskell project FLP

I had to modify tests because they didn't import required types.

I'm not sure if my implementation of `computeStats` and `makeHistogram` in
`Report.hs` is what is expected. The descriptions say that I should use the
number of tests that pass or the total number of tests. This seems weird
because the structure `CategoryReport` with which the functions work doesn't
contain counts in number of tests but in number of points (which may differ). I
sticked to what the comments say and implemented it on the counts of tests
(which required counting them each time).
