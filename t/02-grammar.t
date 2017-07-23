use v6;
use ANTLR4::Grammar;
use Test;

plan 56;

my $parser = ANTLR4::Grammar.new;

my $test-dir = IO::Path.new($?FILE).parent;

for dir("$test-dir/test-files").grep({ $_.ends-with('.g4') }) -> $grammar-file {
    if $grammar-file.ends-with('VisualBasic6.g4') {
        skip 'Need to fix UTF-8 issue', 1;
    }
    else {
        ok $parser.parsefile($grammar-file), $grammar-file.Str;
    }
}
