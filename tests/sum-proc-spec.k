module VERIFICATION
    imports IMP
    syntax Id ::= "n" | "s" | "sum"
endmodule

module SUM-PROC-SPEC
    imports VERIFICATION

    rule <k> while ( ! (n <= 0) ) { s = s + n ; n = n - 1 ; } => . ... </k>
         <mem>
           s |-> (S:Int => S +Int ((N *Int (N +Int 1)) /Int 2))
           n |-> (N:Int => 0)
         </mem>
      requires N >=Int 0

    rule <k> sum ( N:Int , .Ints ) => ((N *Int (N +Int 1)) /Int 2) ... </k>
         <procs>
           sum |-> def sum ( n , .Ids ) {
                     int s , .Ids ;
                     s = 0 ;
                     while ( ! (n <= 0) ) { s = s + n ; n = n - 1 ; }
                     return s ;
                   }
         </procs>
      requires N >=Int 0
endmodule
