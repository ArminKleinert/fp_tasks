filter p (xs ++ ys) == filter p xs ++ filter p ys
Zu beweisen durch strukturelle Induktion

I.A. xs=[]
filter p ([] ++ ys) =? filter p [] ++ filter p ys
filter p ys =? [] ++ filter p ys | (++).1
filter p ys == filter p ys | (++).1
-- Ausdrücke sind äquivalent

I.V. Für xs=xs' gilt
filter p (xs' ++ ys) == filter p xs' ++ filter p ys

I.S. xs = x:xs'
filter p ((x:xs') ++ ys) =? filter p (x:xs') ++ filter p ys

1. Fall: (p x) = True
x:filter p (xs' ++ ys) =? (x:filter p xs') ++ filter p ys | filter.2
x:filter p xs' ++ filter p ys =? x:filter p xs' ++ filter p ys | I.V.

2. Fall: (p x) = False
filter p (xs' ++ ys) == filter p xs' ++ filter p ys | filter.2
-- Bestätigt wegen I.V.

Bewiesen durch strukturelle Induktion für alle endlichen Listen



'|' nutzen statt '--' !!!




1. Fall: (p x)
x:filter p xs' ++ filter p ys =? x:filter p xs' ++ filter p ys | I.V.

1. Fall: (p x)
x:filter p (xs' ++ ys) =? (x:filter p xs') ++ filter p ys | filter.2
