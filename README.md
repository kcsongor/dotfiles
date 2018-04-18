# dotfiles

Random things I eventually want to incorporate into my dotfiles when they become a little more organised:


## Haskell

After `cabal update`, we populate a file with a list of all available packages
`cabal list --simple-output > ~/.cabal-list.txt`

then pick a package with `cat ~/.cabal-list.txt | fzf`
