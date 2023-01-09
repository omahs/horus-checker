(declare-fun ADDR!1 () Int)
(declare-fun ADDR!2 () Int)
(declare-fun ADDR!3 () Int)
(declare-fun ADDR!4 () Int)
(declare-fun ADDR!5 () Int)
(declare-fun ADDR!6 () Int)
(declare-fun ADDR!7 () Int)
(declare-fun ADDR!8 () Int)
(declare-fun ADDR!9 () Int)
(declare-fun MEM!1 () Int)
(declare-fun MEM!2 () Int)
(declare-fun MEM!3 () Int)
(declare-fun MEM!4 () Int)
(declare-fun MEM!5 () Int)
(declare-fun MEM!6 () Int)
(declare-fun MEM!7 () Int)
(declare-fun MEM!8 () Int)
(declare-fun MEM!9 () Int)
(declare-fun ap!<root>@8 () Int)
(declare-fun fp!<24=alloc/root> () Int)
(declare-fun fp!<30=f3/root> () Int)
(declare-fun fp!<root> () Int)
(declare-fun prime () Int)
(assert (and (<= 0 ADDR!1) (< ADDR!1 prime)))
(assert (and (<= 0 ADDR!2) (< ADDR!2 prime)))
(assert (and (<= 0 ADDR!3) (< ADDR!3 prime)))
(assert (and (<= 0 ADDR!4) (< ADDR!4 prime)))
(assert (and (<= 0 ADDR!5) (< ADDR!5 prime)))
(assert (and (<= 0 ADDR!6) (< ADDR!6 prime)))
(assert (and (<= 0 ADDR!7) (< ADDR!7 prime)))
(assert (and (<= 0 ADDR!8) (< ADDR!8 prime)))
(assert (and (<= 0 ADDR!9) (< ADDR!9 prime)))
(assert (and (<= 0 MEM!1) (< MEM!1 prime)))
(assert (and (<= 0 MEM!2) (< MEM!2 prime)))
(assert (and (<= 0 MEM!3) (< MEM!3 prime)))
(assert (and (<= 0 MEM!4) (< MEM!4 prime)))
(assert (and (<= 0 MEM!5) (< MEM!5 prime)))
(assert (and (<= 0 MEM!6) (< MEM!6 prime)))
(assert (and (<= 0 MEM!7) (< MEM!7 prime)))
(assert (and (<= 0 MEM!8) (< MEM!8 prime)))
(assert (and (<= 0 MEM!9) (< MEM!9 prime)))
(assert (and (<= 0 ap!<root>@8) (< ap!<root>@8 prime)))
(assert (and (<= 0 fp!<24=alloc/root>) (< fp!<24=alloc/root> prime)))
(assert (and (<= 0 fp!<30=f3/root>) (< fp!<30=f3/root> prime)))
(assert (and (<= 0 fp!<root>) (< fp!<root> prime)))
(assert (= prime 3618502788666131213697322783095070105623107215331596699973092056135872020481))
(assert (=> (= ADDR!1 ADDR!2) (= MEM!1 MEM!2)))
(assert (=> (= ADDR!1 ADDR!3) (= MEM!1 MEM!3)))
(assert (=> (= ADDR!1 ADDR!4) (= MEM!1 MEM!4)))
(assert (=> (= ADDR!1 ADDR!5) (= MEM!1 MEM!5)))
(assert (=> (= ADDR!1 ADDR!6) (= MEM!1 MEM!6)))
(assert (=> (= ADDR!1 ADDR!7) (= MEM!1 MEM!7)))
(assert (=> (= ADDR!1 ADDR!8) (= MEM!1 MEM!8)))
(assert (=> (= ADDR!1 ADDR!9) (= MEM!1 MEM!9)))
(assert (=> (= ADDR!2 ADDR!3) (= MEM!2 MEM!3)))
(assert (=> (= ADDR!2 ADDR!4) (= MEM!2 MEM!4)))
(assert (=> (= ADDR!2 ADDR!5) (= MEM!2 MEM!5)))
(assert (=> (= ADDR!2 ADDR!6) (= MEM!2 MEM!6)))
(assert (=> (= ADDR!2 ADDR!7) (= MEM!2 MEM!7)))
(assert (=> (= ADDR!2 ADDR!8) (= MEM!2 MEM!8)))
(assert (=> (= ADDR!2 ADDR!9) (= MEM!2 MEM!9)))
(assert (=> (= ADDR!3 ADDR!4) (= MEM!3 MEM!4)))
(assert (=> (= ADDR!3 ADDR!5) (= MEM!3 MEM!5)))
(assert (=> (= ADDR!3 ADDR!6) (= MEM!3 MEM!6)))
(assert (=> (= ADDR!3 ADDR!7) (= MEM!3 MEM!7)))
(assert (=> (= ADDR!3 ADDR!8) (= MEM!3 MEM!8)))
(assert (=> (= ADDR!3 ADDR!9) (= MEM!3 MEM!9)))
(assert (=> (= ADDR!4 ADDR!5) (= MEM!4 MEM!5)))
(assert (=> (= ADDR!4 ADDR!6) (= MEM!4 MEM!6)))
(assert (=> (= ADDR!4 ADDR!7) (= MEM!4 MEM!7)))
(assert (=> (= ADDR!4 ADDR!8) (= MEM!4 MEM!8)))
(assert (=> (= ADDR!4 ADDR!9) (= MEM!4 MEM!9)))
(assert (=> (= ADDR!5 ADDR!6) (= MEM!5 MEM!6)))
(assert (=> (= ADDR!5 ADDR!7) (= MEM!5 MEM!7)))
(assert (=> (= ADDR!5 ADDR!8) (= MEM!5 MEM!8)))
(assert (=> (= ADDR!5 ADDR!9) (= MEM!5 MEM!9)))
(assert (=> (= ADDR!6 ADDR!7) (= MEM!6 MEM!7)))
(assert (=> (= ADDR!6 ADDR!8) (= MEM!6 MEM!8)))
(assert (=> (= ADDR!6 ADDR!9) (= MEM!6 MEM!9)))
(assert (=> (= ADDR!7 ADDR!8) (= MEM!7 MEM!8)))
(assert (=> (= ADDR!7 ADDR!9) (= MEM!7 MEM!9)))
(assert (=> (= ADDR!8 ADDR!9) (= MEM!8 MEM!9)))
(assert (= ADDR!1 ap!<root>@8))
(assert (= ADDR!2 (mod (+ ap!<root>@8 1) prime)))
(assert (= ADDR!3 (mod (+ ap!<root>@8 3) prime)))
(assert (= ADDR!4 (mod (+ ap!<root>@8 2) prime)))
(assert (= ADDR!5 MEM!4))
(assert (= ADDR!6 (mod (+ ap!<root>@8 4) prime)))
(assert (= ADDR!7 (mod (+ ap!<root>@8 5) prime)))
(assert (= ADDR!8 (mod (+ ap!<root>@8 6) prime)))
(assert (= ADDR!9 (mod (+ fp!<30=f3/root> (- 3)) prime)))
(assert (<= fp!<root> ap!<root>@8))
(assert (= ap!<root>@8 fp!<root>))
(assert (and (= fp!<24=alloc/root> (mod (+ ap!<root>@8 2) prime)) (= MEM!1 fp!<root>) (= MEM!2 26)))
(assert true)
(assert true)
(assert (= 42 MEM!3))
(assert (= MEM!5 MEM!3))
(assert (= MEM!4 MEM!6))
(assert (and (= fp!<30=f3/root> (mod (+ ap!<root>@8 7) prime)) (= MEM!7 fp!<root>) (= MEM!8 32)))
(assert (< (mod (+ ap!<root>@8 5) prime) (mod (+ ap!<root>@8 9) prime)))
(assert (not (exists (($x Int)) (and (<= 0 $x) (< $x prime) (exists ((MEM!10 Int)) (and (<= 0 MEM!10) (< MEM!10 prime) (= MEM!9 $x) (= 0 MEM!10) (=> (= $x (mod (+ ap!<root>@8 6) prime)) (= MEM!10 MEM!8)) (=> (= $x (mod (+ ap!<root>@8 5) prime)) (= MEM!10 MEM!7)) (=> (= $x (mod (+ ap!<root>@8 4) prime)) (= MEM!10 MEM!6)) (=> (= $x MEM!4) (= MEM!10 MEM!5)) (=> (= $x (mod (+ ap!<root>@8 2) prime)) (= MEM!10 MEM!4)) (=> (= $x (mod (+ ap!<root>@8 3) prime)) (= MEM!10 MEM!3)) (=> (= $x (mod (+ ap!<root>@8 1) prime)) (= MEM!10 MEM!2)) (=> (= $x ap!<root>@8) (= MEM!10 MEM!1))))))))
(check-sat)