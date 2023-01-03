(declare-fun $n () Int)
(declare-fun ADDR!1 () Int)
(declare-fun ADDR!2 () Int)
(declare-fun ADDR!3 () Int)
(declare-fun ADDR!4 () Int)
(declare-fun MEM!1 () Int)
(declare-fun MEM!2 () Int)
(declare-fun MEM!3 () Int)
(declare-fun MEM!4 () Int)
(declare-fun ap!<2=inc/root>@0 () Int)
(declare-fun ap!<root>@0 () Int)
(declare-fun ap!<root>@1 () Int)
(declare-fun fp!<2=inc/root> () Int)
(declare-fun fp!<root> () Int)
(declare-fun prime () Int)
(assert (and (<= 0 $n) (< $n prime)))
(assert (and (<= 0 ADDR!1) (< ADDR!1 prime)))
(assert (and (<= 0 ADDR!2) (< ADDR!2 prime)))
(assert (and (<= 0 ADDR!3) (< ADDR!3 prime)))
(assert (and (<= 0 ADDR!4) (< ADDR!4 prime)))
(assert (and (<= 0 MEM!1) (< MEM!1 prime)))
(assert (and (<= 0 MEM!2) (< MEM!2 prime)))
(assert (and (<= 0 MEM!3) (< MEM!3 prime)))
(assert (and (<= 0 MEM!4) (< MEM!4 prime)))
(assert (and (<= 0 ap!<2=inc/root>@0) (< ap!<2=inc/root>@0 prime)))
(assert (and (<= 0 ap!<root>@0) (< ap!<root>@0 prime)))
(assert (and (<= 0 ap!<root>@1) (< ap!<root>@1 prime)))
(assert (and (<= 0 fp!<2=inc/root>) (< fp!<2=inc/root> prime)))
(assert (and (<= 0 fp!<root>) (< fp!<root> prime)))
(assert (= prime 3618502788666131213697322783095070105623107215331596699973092056135872020481))
(assert (=> (= ADDR!1 ADDR!2) (= MEM!1 MEM!2)))
(assert (=> (= ADDR!1 ADDR!3) (= MEM!1 MEM!3)))
(assert (=> (= ADDR!1 ADDR!4) (= MEM!1 MEM!4)))
(assert (=> (= ADDR!2 ADDR!3) (= MEM!2 MEM!3)))
(assert (=> (= ADDR!2 ADDR!4) (= MEM!2 MEM!4)))
(assert (=> (= ADDR!3 ADDR!4) (= MEM!3 MEM!4)))
(assert (= ADDR!1 (mod (+ fp!<root> (- 3)) prime)))
(assert (= ADDR!2 ap!<root>@0))
(assert (= ADDR!3 (mod (+ ap!<root>@0 1) prime)))
(assert (= ADDR!4 (mod (+ ap!<root>@0 2) prime)))
(assert (<= fp!<root> ap!<root>@0))
(assert (and (= MEM!1 $n) (= ap!<root>@0 fp!<root>)))
(assert (= (mod (+ MEM!1 1) prime) MEM!2))
(assert (and (= fp!<2=inc/root> (mod (+ ap!<root>@0 3) prime)) (= MEM!3 fp!<root>) (= MEM!4 4)))
(assert (< (mod (+ ap!<2=inc/root>@0 1) prime) ap!<root>@1))
(assert (not (exists (($n Int)) (and (<= 0 $n) (< $n prime) (= (mod (memory (mod (+ fp (- 3)) prime)) prime) $n)))))