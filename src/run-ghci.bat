rem https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-warnings.html
rem https://medium.com/mercury-bank/enable-all-the-warnings-a0517bc081c3

ghci -hidir .output -odir .output^
 -package parsec^
 -Wall^
 -Werror^
 -Wno-type-defaults^
 -Wincomplete-uni-patterns^
 -Wincomplete-record-updates^
 -Wcpp-undef^
 -Wmissing-export-lists^
 -XAllowAmbiguousTypes^
 -XBangPatterns^
 -XBlockArguments^
 -XConstraintKinds^
 -XDataKinds^
 -XDefaultSignatures^
 -XDeriveFunctor^
 -XEmptyDataDecls^
 -XFlexibleContexts^
 -XFlexibleInstances^
 -XFunctionalDependencies^
 -XGADTs^
 -XLambdaCase^
 -XMagicHash^
 -XMultiParamTypeClasses^
 -XMultiWayIf^
 -XOverloadedStrings^
 -XPatternSynonyms^
 -XPolyKinds^
 -XQuantifiedConstraints^
 -XRankNTypes^
 -XRecursiveDo^
 -XScopedTypeVariables^
 -XStandaloneDeriving^
 -XStrictData^
 -XTemplateHaskellQuotes^
 -XTypeApplications^
 -XTypeFamilyDependencies^
 -XViewPatterns^
 %*
