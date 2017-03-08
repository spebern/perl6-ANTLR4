use v6;
use ANTLR4::Grammar;
use Test;

plan 57;

my $parser = ANTLR4::Grammar.new;

for dir 'corpus' -> $grammar-file {
    if $grammar-file ~~ 'corpus/VisualBasic6.g4' {
        skip 'Need to fix UTF-8 issue', 1;
    }
    else {
        ok $parser.parsefile($grammar-file), $grammar-file.Str;
    }
}
