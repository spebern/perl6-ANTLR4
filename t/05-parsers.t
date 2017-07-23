use v6;
use Test;
use ANTLR4;

plan 2;

my @grammars-to-test = [
    {
	name             => 'JSON',
	token-top        => 'json',
	test-file-ending => 'json',
    },
    {
	name             => 'CSV',
	token-top        => 'file',
	test-file-ending => 'csv',
    },
];

my $test-files-dir = IO::Path.new($?FILE).parent.add('test-files');

for @grammars-to-test -> $grammar-to-test {
    my $grammar-name = $grammar-to-test<name>;
    my $grammar = g4-to-perl6("$test-files-dir/$grammar-name.g4".IO.slurp);

    my $token-top = $grammar-to-test<token-top>;
    $grammar ~~ s/[rule || token] \s $token-top/rule TOP/;

    subtest sub {
	if eval-lives-ok "$grammar", "syntax of translated grammar" {
	    EVAL $grammar;

	    my $test-file-ending = $grammar-to-test<test-file-ending>;
	    my $parser-test-input = "$test-files-dir/{$grammar-name}_example.$test-file-ending".IO.slurp;
	    isnt Nil, EVAL("$grammar-name" ~ ".parse('$parser-test-input')"), "test-file parsing";
	}
	else {
	    skip "syntax of translated grammar $grammar-name is broken";
	}
    }, "$grammar-name translation and parsing";
}
