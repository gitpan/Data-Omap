use strict;
use warnings;

use Test::More 'no_plan';

use Data::Dumper;
$Data::Dumper::Terse=1;
$Data::Dumper::Indent=0;

BEGIN { use_ok('Data::Omap') };

my( $omap, @values, @keys, @array, $aref, @pos );

$omap = Data::Omap->new( [ {c=>3}, {a=>1}, {b=>2}, ] );

@values = $omap->get_values();
is( "@values", "3 1 2",
    "get_values(), all values, like 'values %hash'" );

@values = $omap->get_values( qw( a b c ) );
is( "@values", "3 1 2",
    "get_values(), selected values, like '\@hash{'c','a','b'}', i.e., data-ordered" );

@keys = $omap->get_keys();
is( "@keys", "c a b",
    "get_keys(), like 'keys %hash'" );

@keys = $omap->get_keys( qw( a b c ) );
is( "@keys", "c a b",
    "get_keys() for selected keys, data-ordered" );

@pos = $omap->get_pos( qw( a b c ) ); # 1 is pos of 'a', 2 of 'b', 0 of 'c'
is( "@pos", "1 2 0",
    "get_pos() for selected keys, parameter-ordered" );

@array = $omap->get_array();
is( Dumper(\@array), "[{'c' => 3},{'a' => 1},{'b' => 2}]",
    "get_array(), list context" );

$aref = $omap->get_array();
is( Dumper($aref), "[{'c' => 3},{'a' => 1},{'b' => 2}]",
    "get_array(), scalar context" );

@array = $omap->get_array( qw( b c ) );
is( Dumper(\@array), "[{'c' => 3},{'b' => 2}]",
    "get_array() for selected keys, data-ordered" );

$omap->set( a=>0 ); @values = $omap->get_values( qw( a b c ) );
is( "@values", "3 0 2",
    "set() a value" );

# at pos 1, overwrite 'a'
$omap->set( A=>1,1 ); @values = $omap->get_values( qw( A b c ) );
is( "@values", "3 1 2",
    "set() a value at a position" );

$omap->add( d=>4 ); @values = $omap->get_values( qw( A b c d ) );
is( "@values", "3 1 2 4",
    "add() a value" );

# add at pos 2, between 'A' and 'b'
$omap->add( a=>0,2 ); @values = $omap->get_values( qw( A a b c d ) );
is( "@values", "3 1 0 2 4",
    "add() a value at a position" );

# firstkey/nextkey to support TIEHASH
is( $omap->firstkey(), 'c',
    "firstkey()" );  
is( $omap->nextkey('c'), 'A',
    "nextkey()" );
is( $omap->nextkey('b'), 'd',
    "nextkey()" );

is( $omap->exists('a'), 1,
    "exists() true" );
is( $omap->exists('B'), '',
    "exists() false" );

$omap->delete('A');
is( Dumper($omap), "bless( [{'c' => 3},{'a' => 0},{'b' => 2},{'d' => 4}], 'Data::Omap' )",
    "delete()" );

$omap->clear();
is( Dumper($omap), "bless( [], 'Data::Omap' )",
    "clear()" );

