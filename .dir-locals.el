(
 (haskell-mode . ((dante-target . "tiny-lang:lib")))
 ("test" . ((haskell-mode . ((dante-target . "tiny-lang:tiny-lang-test")
			     (haskell-process-args-stack-ghci . ("--ghci-options=-ferror-spans"
								  "--no-build"
								  "--no-load"
								  "--test"))))))
)

