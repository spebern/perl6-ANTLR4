=begin pod

=head1 ANTLR4::Actions::Perl6

C<ANTLR4> exports function to translate ANTLR grammar to perl6.

=head1 Synopsis

    use ANTLR4;

    say g4-to-perl6('grammar Minimal { identifier : [A-Z][A-Za-z]+ ; }');

=head1 Documentation

The function g4-to-perl6 will return the g4 grammar translated to perl6 syntax.

=end pod

use v6;

unit module ANTLRv4::Translator;
use JSON::Tiny;
use ANTLR4::Grammar;
use ANTLR4::Actions::AST;

sub java-to-perl-utf8(Str $utf8 is copy --> Str) {
    $utf8 ~~ s/\\u(....)/\\x[$0]/;
    return $utf8;
}

sub rules($ast) {
    my Str ($ws-token-content, $ws-token-json-info);
    my Str @rule-bodies;
    for $ast<rules>.flat -> $rule-ast {
	my Str $translation = join ' ', term($rule-ast<content>);

	my @commands = ($rule-ast<content><commands> || do {
	    my $command-info = $rule-ast<content><contents>.first( { $_ ~~ Hash and $_<commands>:exists; } );
	    $command-info<commands>
	}).flat;

	# in ANTLR4 the "skip" command makes it possible for the lexer to skip
	# the elements described inside that rule/token
	# "-> channel(HIDDEN)" will also be skipped by the lexer
	if 'skip' ~~ any(@commands».keys.flat) or
	   'hidden' ~~ any(@commands.grep({ $_<channel>:exists }).map({ lc $_<channel> })) {
	    if $ws-token-content {
		$ws-token-content ~~ s/\)$/ \| $translation\)/;
	    }
	    else {
		if $translation.starts-with('(') and $translation.ends-with(')') {
		    $ws-token-content = $translation;
		}
		else {
		    $ws-token-content = "($translation)";
		}
	    }
	    next;
	}
	my $rule-body = qq{$rule-ast<name> { $translation }}
	    ~ json-info($rule-ast, <attribute action returns throws locals options>);;

	@rule-bodies.append($rule-body);
    }

    my Str @rules;
    if $ws-token-content {
	@rules = map { "rule $_" }, @rule-bodies;

	# the "token ws" is inserted between every element inside a rule
	# a "+"-modifier makes the white space required, but it needs to
	# be optional, thus the modifier needs to be changed to a "*"
	if $ws-token-content.ends-with('+') {
	    $ws-token-content ~~ s/\+$/\*/;
	}
	else {
	    $ws-token-content ~= '*';
	}
	@rules.append("token ws \{ $ws-token-content \}");
    }
    else {
	@rules = map { "token $_" }, @rule-bodies;
    }

    return @rules;
}

sub modify($ast, $term is copy --> Str) {
    $term ~= $ast<modifier> if $ast<modifier>;
    $term ~= '?' if $ast<greedy>;
    return $term;
}

sub alternation($ast --> Str) {
    my @terms = $ast<contents>.flat.map({ term($_) });

    my $translation = @terms[0];
    if @terms.elems > 1 {
	for @terms[1 .. *] -> $term {
	    if $term.starts-with('#={<EOF>}') {
		$translation ~= " $term";
	    }
	    else {
		$translation ~= " | $term";
	    }
	}
    }

    return $translation;
}

sub concatenation($ast --> Str) {
    my $translation = '';

    # empty string is '' in a perl6 grammar
    if !$ast<contents>.elems {
	return q{''} ~ json-info($ast, (<commands>, ));
    }

    # this most likely has some errors in it
    # the idea is to use "%%"
    # value ( ',' value )* should become ( <value>+ %% ',' )
    # this eases the use of the generated grammar
    my $i = 0;
    while $i < $ast<contents>.elems {
        my $content = $ast<contents>[$i];

        if $content<type> eq 'terminal' | 'nonterminal' {
            my $content-translation = term($content);

            my $next-content = ++$i < $ast<contents>.elems ?? $ast<contents>[$i] !! Nil;

            if $next-content && $next-content<type> eq 'capturing group' {
                if $next-content<content><type> eq 'alternation' {
                    my $alternation = $next-content<content>;

                    if $alternation<contents>.elems == 1
                      && $alternation<contents>[0]<type> eq 'concatenation' {
                        my $concatination          = $alternation<contents>[0];
                        my $last-concatinated-term = $concatination<contents>[ * - 1];

                        if term($last-concatinated-term) eq $content-translation {
                            my Str $deliminator = join ' ', map {
                                term($_)
                            }, $concatination<contents>.flat[0 .. * -2];
                            $translation ~= qq{ ( $content-translation+ %% $deliminator )};
                            ++$i;
                            next;
                        }
                    }
                }
            }
            $translation ~= ' ' ~ $content-translation;
        }
        else {
            $translation ~= ' ' ~ term($content);
            ++$i;
        }
    }
    # $translation ~= join ' ', map { term($_) }, $term<contents>.flat;
    return $translation.trim ~ json-info($ast, (<commands>, ));
};

sub terminal($ast --> Str) {
    my Str $content;
    given $ast<content> {
        when q{'\r'} { $content = '\r'    }
        when q{'\n'} { $content = '\n'    }
        when q{'´´'} { $content = q{'\´'} }
        when q{'`'}  { $content = q{'`'}  }
        when q{'"'}  { $content = q{'"'}  }
	when q{'\''} { $content = q{\' }  }
        default      { $content = java-to-perl-utf8($_) }
    }
    if $ast<complemented> {
        $content ~~ s/^\'(.*?)\'$/$0/; # ' repair sytanx highlighting
        given $content {
            when ']' { $content = '\]' }
            when '[' { $content = '\[' }
        }
        $content = '<-[' ~ $content ~ ']>';
    }
    return modify($ast, $content) ~ json-info($ast, <options label commands>);
};

sub nonterminal($ast --> Str) {
    my $translation = '<';
    $translation ~= '!' if $ast<complemented>;
    $translation ~= $ast<content> ~ '>';

    # EOF rule is not needed in perl6
    if $translation eq '<EOF>' {
	return '#={<EOF>}';
    }

    return modify($ast, $translation);
};

sub range($ast --> Str) {
    my $translation = '';
    $translation ~= '!' if $ast<complemented>;
    my ($from, $to) = ($ast<from>, $ast<to>).map({ java-to-perl-utf8($_) });
    $translation ~= qq{$from..$to};
    return modify($ast, $translation);
};

sub character-class($ast --> Str) {
    my $translation = '<';
    $translation ~= '-' if $ast<complemented>;
    $translation ~= '[';

    $translation ~= join ' ', $ast<contents>.flat.map(-> $content {
        if $content !~~ Str {
	    if $content<type> eq 'character class' {
                ~$content<contents>;
            }
            elsif $content<type> eq 'range' {
	        term($content);
            }
            elsif $content<type> eq 'terminal' {
                my $terminal = term($content);
                if $terminal ~~ /^\'(.*?)\'$/ {
		    $terminal = ~$0;
		    if $terminal eq '[' | ']' {
		        $terminal = '\\' ~ $terminal;
                    }
                }
                $terminal;
	    }
            else {
	        die "unsupported type '$content<type>' in character class";
            }
	}
	elsif $content ~~ /^(.) '-' (.)/ {
	    qq{$0 .. $1};
        }
        elsif $content ~~ /^\\u(....) '-' \\u(....)/ {
            qq{\\x[$0] .. \\x[$1]};
        }
        elsif $content ~~ /^\\u(....)/ {
            qq{\\x[$0]};
        }
        elsif $content ~~ /' '/ {
            q{' '};
        }
        elsif $content ~~ /\\\-/ {
            q{-};
        }
        else {
            $content;
        }
    });

    $translation ~= ']>';

    return modify($ast, $translation);
};

sub regular-expression($ast --> Str) {
    my $translation = '';
    $translation ~= '!' if $ast<complemented>;
    $translation ~= $ast<content>;
    return modify($ast, $translation);
};

sub capturing-group($ast --> Str) {
    my $translation = '';
    $translation ~= '!' if $ast<complemented>;

    my $group = term($ast<content>);
    
    $translation ~= qq{($group)};
    return modify($ast, $translation);
}

sub action($ast --> Str) {
    return json-info($ast, (<content>, ));
}

sub term($ast --> Str) {
    my $translation = '';

    given $ast<type> {
        when 'alternation' {
            $translation = alternation($ast);
        }
        when 'concatenation' {
            $translation = concatenation($ast);
        }
        when 'terminal' {
            $translation = terminal($ast);
        }
        when 'nonterminal' {
            $translation = nonterminal($ast);
        }
        when 'range' {
            $translation = range($ast);
        }
        when 'character class' {
            $translation = character-class($ast);
        }
        when 'capturing group' {
            $translation = capturing-group($ast);
        }
        when 'regular expression' {
            $translation = regular-expression($ast);
        }
        when 'action' {
            $translation = action($ast);
        }
        default {
            if $ast<type> {
                die "unkown type '$ast<type>'";
            }
            else {
                die "missing type";
            }
        }
    }

    return $translation;
}

sub json-info($ast, @keys --> Str) {
    my %json = |@keys.grep({ $ast{$_} }).map({$_ => $ast{$_}});
    return %json.elems ?? ' #=' ~ to-json(%json) !! '';
}

sub ast($ast --> Str) {
    my Str $rules = '';
    $rules = join "\n", rules($ast);

    my Str $grammar = qq{grammar $ast<name> { $rules }};
    $grammar ~= json-info($ast, <type options imports tokens actions>);
    return $grammar;
}

sub g4-to-perl6(Str $g4, --> Str) is export {
    my $ast = ANTLR4::Grammar.new.parse(
       $g4, actions => ANTLR4::Actions::AST 
    ).made;

    return ast($ast);
}
