okay
STk> (twenty-one (majority stop-at-17 dealer-sensitive valentine))
.. -> majority with strategy1 = #[closure arglist=l 42d180],  strategy2 = #[closure arglist=l 422a40],  strategy3 = #[closure arglist=l 41d470]
.. <- majority returns #[closure arglist=(customer-hand-so-far dealer-up-card) 450ff0]
.. -> twenty-one with strategy = #[closure arglist=(customer-hand-so-far dealer-up-card) 450ff0]
.... -> stop-at-17 with customer-hand-so-far = ("6c" "4d"),  dealer-up-card = "7d"
.... <- stop-at-17 returns #t
.... -> dealer-sensitive with customer-hand-so-far = ("6c" "4d"),  dealer-up-card = "7d"
.... <- dealer-sensitive returns #t
.... -> valentine with customer-hand-so-far = ("6c" "4d"),  dealer-up-card = "7d"
.... <- valentine returns #t
.... -> stop-at-17 with customer-hand-so-far = ("6c" "4d" "5d"),  dealer-up-card = "7d"
.... <- stop-at-17 returns #t
.... -> dealer-sensitive with customer-hand-so-far = ("6c" "4d" "5d"),  dealer-up-card = "7d"
.... <- dealer-sensitive returns #t
.... -> valentine with customer-hand-so-far = ("6c" "4d" "5d"),  dealer-up-card = "7d"
.... <- valentine returns #t
.. <- twenty-one returns -1
-1
STk> (twenty-one (majority stop-at-17 dealer-sensitive valentine))
.. -> majority with strategy1 = #[closure arglist=l 42d180],  strategy2 = #[closure arglist=l 422a40],  strategy3 = #[closure arglist=l 41d470]
.. <- majority returns #[closure arglist=(customer-hand-so-far dealer-up-card) 4540f0]
.. -> twenty-one with strategy = #[closure arglist=(customer-hand-so-far dealer-up-card) 4540f0]
.... -> stop-at-17 with customer-hand-so-far = ("2d" ah),  dealer-up-card = "2c"
.... <- stop-at-17 returns #t
.... -> dealer-sensitive with customer-hand-so-far = ("2d" ah),  dealer-up-card = "2c"
.... <- dealer-sensitive returns #f
.... -> valentine with customer-hand-so-far = ("2d" ah),  dealer-up-card = "2c"
.... <- valentine returns #t
.... -> stop-at-17 with customer-hand-so-far = ("2d" ah kh),  dealer-up-card = "2c"
.... <- stop-at-17 returns #t
.... -> dealer-sensitive with customer-hand-so-far = ("2d" ah kh),  dealer-up-card = "2c"
.... <- dealer-sensitive returns #f
.... -> valentine with customer-hand-so-far = ("2d" ah kh),  dealer-up-card = "2c"
.... <- valentine returns #t
.. <- twenty-one returns -1
-1
STk> (twenty-one (majority stop-at-17 dealer-sensitive valentine))
.. -> majority with strategy1 = #[closure arglist=l 42d180],  strategy2 = #[closure arglist=l 422a40],  strategy3 = #[closure arglist=l 41d470]
.. <- majority returns #[closure arglist=(customer-hand-so-far dealer-up-card) 44e980]
.. -> twenty-one with strategy = #[closure arglist=(customer-hand-so-far dealer-up-card) 44e980]
.... -> stop-at-17 with customer-hand-so-far = ("3d" "2h"),  dealer-up-card = "6h"
.... <- stop-at-17 returns #t
.... -> dealer-sensitive with customer-hand-so-far = ("3d" "2h"),  dealer-up-card = "6h"
.... <- dealer-sensitive returns #t
.... -> valentine with customer-hand-so-far = ("3d" "2h"),  dealer-up-card = "6h"
.... <- valentine returns #t
.... -> stop-at-17 with customer-hand-so-far = ("3d" "2h" "9h"),  dealer-up-card = "6h"
.... <- stop-at-17 returns #t
.... -> dealer-sensitive with customer-hand-so-far = ("3d" "2h" "9h"),  dealer-up-card = "6h"
.... <- dealer-sensitive returns #f
.... -> valentine with customer-hand-so-far = ("3d" "2h" "9h"),  dealer-up-card = "6h"
.... <- valentine returns #t
.... -> stop-at-17 with customer-hand-so-far = ("3d" "2h" "9h" "7h"),  dealer-up-card = "6h"
.... <- stop-at-17 returns #f
.... -> dealer-sensitive with customer-hand-so-far = ("3d" "2h" "9h" "7h"),  dealer-up-card = "6h"
.... <- dealer-sensitive returns #f
.... -> valentine with customer-hand-so-far = ("3d" "2h" "9h" "7h"),  dealer-up-card = "6h"
.... <- valentine returns #f
.. <- twenty-one returns 1
1
STk> (twenty-one (majority stop-at-17 dealer-sensitive valentine))
.. -> majority with strategy1 = #[closure arglist=l 42d180],  strategy2 = #[closure arglist=l 422a40],  strategy3 = #[closure arglist=l 41d470]
.. <- majority returns #[closure arglist=(customer-hand-so-far dealer-up-card) 451c40]
.. -> twenty-one with strategy = #[closure arglist=(customer-hand-so-far dealer-up-card) 451c40]
.... -> stop-at-17 with customer-hand-so-far = ("7s" ah),  dealer-up-card = "6s"
.... <- stop-at-17 returns #f
.... -> dealer-sensitive with customer-hand-so-far = ("7s" ah),  dealer-up-card = "6s"
.... <- dealer-sensitive returns #f
.... -> valentine with customer-hand-so-far = ("7s" ah),  dealer-up-card = "6s"
.... <- valentine returns #t
.. <- twenty-one returns 1
1
STk> (transcript-off)
