com :: (a -> a) -> (a -> a) -> a -> (a, a)            com f g x = ((f . g) x, (g . f) x)

Reduziere den Ausdruck: com (* 10) (mod 10) 7

=> (((*10).(mod 10) 7), ((mod 10).(*10) 7))
=> ((mod 10 (7 * 10)), ((mod 7 10) * 10))
=> ((mod 10 (7 * 10)), ((7) * 10))
=> ((mod 10 (7 * 10)), 70)
=> ((mod 10 70), 70)
=> (10, 70)

