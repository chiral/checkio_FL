module Main where

import Control.Monad
import Data.List
import Data.Char

checkio :: (String,String) -> String
checkio (text, ws) = zip [0..] text >>= trans
    where
      span = [(i,w) | w<-words $ map toLower ws,
                      (i,t)<-zip [0..] (tails $ map toLower text),
                      isPrefixOf w t]
      hit i = (sort.nub) [w | (j,w)<-span, i>=j && i-j<length w]
      border a b = guard $ a/=[] && (a `intersect` b)==[]
      trans (i,c) = let [h1,h2,h3]=map hit [i-1..i+1]
                        from = border h2 h1 >> "<span>"
                        to = border h2 h3 >> "</span>"
                    in from++[c]++to

assert info args expected = let ans = checkio args in
                            if ans /= expected then do
                              putStrLn $ "Failed case: " ++ show info
                              print ans
                            else return ()

main = do
  assert 1 ("This is only a text example for task example.", "example") 
       "This is only a text <span>example</span> for task <span>example</span>."

  assert 2 ("Python is a widely used high-level programming language.", "pyThoN")
       "<span>Python</span> is a widely used high-level programming language."
 
  assert 3 ("It is experiment for control groups with similar distributions.", "is im")
       "It <span>is</span> exper<span>im</span>ent for control groups with s<span>im</span>ilar d<span>is</span>tributions."
 
  assert 4 ("The National Aeronautics and Space Administration (NASA).", "nasa  THE")
       "<span>The</span> National Aeronautics and Space Administration (<span>NASA</span>)."
 
  assert 5 ("Did you find anything?", "word space tree")
             "Did you find anything?"
 
  assert 6 ("Hello World! Or LOL", "hell world or lo")
             "<span>Hello</span> <span>World</span>! <span>Or</span> <span>LO</span>L"

  assert 6 ("aaaaaaaaaaaaa", "a aaa aa")
             "<span>aaaaaaaaaaaaa</span>"

  putStrLn "done."

  
