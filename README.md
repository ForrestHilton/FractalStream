# This is a Personal Copy

This is a version of Matt Noonan's 2nd FractalStream (in Haskell). This copy is modified to be conveniently buildable on Arch Linux. There is still an undocumented symbolic link needed when installing on Arch.

## run in repl with compiled core and backend:
```
cd fractalstream-ui-wx
stack setup
stack build
stack ghci fractalstream-ui-wx:exe:FractalStream
```

## run in repl with all modules loaded:
```
stack repl --ghci-options="-fdefer-type-errors -w"
```
