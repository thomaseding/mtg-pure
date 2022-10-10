rem TODO: Enable more than -Wall...
rem https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-warnings.html
rem https://medium.com/mercury-bank/enable-all-the-warnings-a0517bc081c3
ghci -hidir .output -odir .output -fobject-code -Wall -Werror -Wno-type-defaults -XDataKinds -XOverloadedStrings -XTypeApplications MtgPure
