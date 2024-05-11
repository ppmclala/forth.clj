# forth.clj



# references

* https://galileo.phys.virginia.edu/classes/551.jvn.fall01/primer.htm
* https://github.com/nornagon/jonesforth
* https://www.ioccc.org/years.html#1992_buzzard.2
* https://gitlab.com/tsoding/porth

## notes

  ;; memory layout
  {1  {:kind :word
       :code-word (fn []) ;; intrinsic
       :word "WORD1"
       :prev 123}
   2  {:kind :word
       :code-word nil;; ["WORD1" "WORD9"] ;; DOCOL word
       :word "WORD2"}
   3  {:kind :val
       :val 123}}

  ;; execute
  ;; bootstrap at 0
  ;; run ENTER ;; pushes ip+1 to return stack
  ;; execute current word
  ;; if word -> check code-word if intrinsic -> execute
  ;; if val -> fault

  ;; NEXT
  ;; loads IP into a reg (eax)
  ;; increments IP
  ;; jmp to (eax)
  ;; needed on all native and in DOCOL

  ;; EXIT pops, set IP and calls NEXT
  ;; DOCOL runs functions not compile them

  {0   {:kind :word
        :code-word (fn []) ;; intrinsic
        :word "ENTER"
        :prev nil}
   1   {:kind :word
        :code-word (fn []) ;; intrinsic
        :word "EXIT"
        :prev 0}
   2   {:kind :word
        :code-word (fn []) ;; intrinsic
        :word "DOCOL"
        :prev nil}
   3   {:kind :word
        :code-word (fn []) ;; intrinsic
        :word "PUSH"
        :prev 0}
   4   {:kind :word
        :code-word (fn []) ;; intrinsic
        :word "NATIVEFOO"
        :prev nil}
   5   {:kind :word
        :code-word (fn []) ;; intrinsic
        :word "NATIVEBAR"
        :prev 0}
   454 {:kind :word
        :code-word nil ;; : COMPILEDFOO NATIVEFOO NATIVEBAR ;
        :word "COMPILEDFOO"
        :prev 333}
   ;; do we need explict ENTER or is this the code-word
   455 {:kind :word
        :code-word 4}
   456 {:kind :word
        :code-word 5}
   457 {:kind :word
        :code-word 1} ;; this is EXIT
   458 {:kind :word
        :code-word nil ;; : COMPILEDBAR NATIVEBAR 123 NATIVEFOO COMPILEDFOO ;
        :word "COMPILEDBAR"
        :prev 454} ;; ???
   459 {:kind :word
        :code-word 1}
   460 {:kind :val
        :val 123}
   461 {:kind :word
        :code-word 0}
   462 {:kind :word
        :code-word 454}
   463 {:kind :word
        :code-word 1} ;; this is EXIT
   }

  ;; COMPILEDFOO
  ;; search from LATEST (457) via :prev
  ;; check :word for match
  ;; set IP to 457 ;; ENTER moves from here
  ;; move to 457 + 1
  ;; ENTER (458) -> push 459 onto return stack
  ;; jump to :code-word (1)
  ;; check :code-word
  ;; native? -> call native fn
  ;; EXIT pop rs and jump to 459
  ;; ENTER (459) -> push 460 onto return stack
  ;; ENTER (460) -> push 461 onto return stack
