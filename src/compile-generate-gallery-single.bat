rem TODO: Use Haskell to shell out instead.
rem Want less batch files, and also Haskell is easier to libraritize.
rem E.g. It is much easier to ensure the exe names all match in shelled
rem out commands if using a Haskell lib variable for the base name.
rem Would make the build more robust vs refactoring.

ghc^
 -main-is Script.GenerateGallerySingle.Main^
 --make Script/GenerateGallerySingle/Main.hs^
 -hidir .output -odir .output^
 -O2^
 -o .output/generate-gallery-single.exe^
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
 -XViewPatterns
