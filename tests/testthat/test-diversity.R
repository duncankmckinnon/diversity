set.seed(2020)

describe( 'diversity', {
  describe( 'diversity()', {
    describe( 'vector input', {
      it( 'should get maximum diversity for uniformly distributed groupings', {
        u1 <- runif( 100,0,10 )
        u2 <- runif( 1000,0,10 )
        d1 <- diversity( u1 )
        d2 <- diversity( u2 )
        expect_equivalent( d1['diversity'], 1 )
        expect_equivalent( d1['entropy'], d1['potential_entropy'] )
        expect_equivalent( d2['diversity'], 1 )
        expect_equivalent( d2['entropy'], d2['potential_entropy'] )
        expect_true( d1['entropy'] < d2['entropy'] )
        expect_true( d1['potential_entropy'] < d2['potential_entropy'] )
      } )
      it( 'should get higher diversity for evenly distributed groupings', {
        r1 <- diversity( c( 'a','b','l',rep( 'w',97 ) ) )
        r2 <- diversity( c( rep( 'a', 20 ), rep( 'b', 5 ), rep( 'l', 19 ), rep( 'w', 56 ) ) )
        r3 <- diversity( c( rep( 'a', 30 ), rep( 'b', 14 ), rep( 'l', 12 ), rep( 'w', 44 ) ) )
        r4 <- diversity( c( rep( 'a', 22 ), rep( 'b', 26 ), rep( 'l', 28 ), rep( 'w', 24 ) ) )
        r5 <- diversity( 1:100 )
        r6 <- diversity( c( rep( 'a', 25 ), rep( 'b', 25 ), rep( 'l', 25 ), rep( 'w', 25 ) ) )
        expect_true( r1[ 'diversity' ] < r2[ 'diversity' ] )
        expect_true( r2[ 'diversity' ] < r3[ 'diversity' ] )
        expect_true( r3[ 'diversity' ] < r4[ 'diversity' ] )
        expect_true( r4[ 'diversity' ] < r5[ 'diversity' ] )
        expect_true( r4[ 'diversity' ] < r6[ 'diversity' ] )
        expect_equivalent( r1[ 'potential_entropy' ], r4[ 'potential_entropy' ] )
        expect_equivalent( r2[ 'potential_entropy' ], r3[ 'potential_entropy' ] )
        expect_equivalent( r2[ 'potential_entropy' ], r4[ 'potential_entropy' ] )

        expect_equivalent( r5[ 'diversity'], r6[ 'diversity' ] )
        expect_true( r6[ 'potential_entropy' ] < r5[ 'potential_entropy' ] )
      })
      it( 'should get zero diversity for scalar or singular input', {
        expect_equal( diversity( 1 ), c( 'diversity'=0, 'entropy'=0,'potential_entropy'=0 ) )
        expect_equal( diversity( 'a' ), c( 'diversity'=0, 'entropy'=0,'potential_entropy'=0 ) )
        expect_equal( diversity( T ), c( 'diversity'=0, 'entropy'=0,'potential_entropy'=0 ) )
      } )
    } )
    describe( 'data.frame input', {

    } )
    describe( 'errors', {

    } )
  } )
  describe('unique_counts()', {

  })
} )
