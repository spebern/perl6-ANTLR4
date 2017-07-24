use v6;
use Test;
use ANTLR4;

plan 5;

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
    {
	name             => 'fasta',
	token-top        => 'sequence',
	test-file-ending => 'fasta',
    },
    # TODO: comments don't work and also the following struct
    # because of :type :apple
    #(defn create-apple []
    #  {:location [(rand-int width) (rand-int height)]
    #   :color (Color. 210 50 90)
    #   :type :apple})
    # the problem with the comment is that ';' is missing in the character class of SYMBOL_HEAD
    #   rule SYMBOL_HEAD { <-[0..9 ^ ` \'  " # ~ @ : / % ( ) \[ \] { } \s \n \r \t \,]> }
    # ';' will be matched as SYMBOL_HEAD
    {
	name             => 'Clojure',
	token-top        => 'file',
	test-file-ending => 'clj',
    },
    {
	name             => 'SQLite',
	token-top        => 'parse',
	test-file-ending => 'sql',
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

	    # TODO: this most likely is not the nicest way..., BUT it works
	    my $parse-func = EVAL('sub (Str $input) {' ~ $grammar-name ~ '.parse($input) }');
	    isnt Nil, $parse-func($parser-test-input), "test-file parsing";
	}
	else {
	    skip "syntax of translated grammar $grammar-name is broken";
	}
    }, "$grammar-name translation and parsing";
}
