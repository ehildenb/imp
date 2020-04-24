// [ 1 : 2 : 3 : 4 : 5 : .Vals ]

letrec
    cons h [ t ] = [ h : t ]
and take n = fun [       ] -> [ ]
             |   [ h : t ] -> if n > 0 then cons h (take (n - 1) [ t ]) else [ ]
 in take 5 [ 1 : 2 : 3 : 4 : 5 : 6 : 7 : .Vals ]
