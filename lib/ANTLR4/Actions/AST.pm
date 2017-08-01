=begin pod

=head1 ANTLR4::Actions::AST

C<ANTLR4::Actions::AST> encompasses the grammar actions needed to create a
perl6 AST from an ANTLR4 parser specification.

=head1 Synopsis

    use ANTLR4::Actions::AST;
    use ANTLR4::Grammar;
    my $a = ANTLR4::Actions::AST.new;
    my $g = ANTLR4::Grammar.new;

    say $g.parsefile('ECMAScript.g4', :actions($a) ).ast;

=head1 Documentation

The action in this file will return a completely unblessed abstract syntax
tree (AST) of the ANTLR4 grammar perl6 has been asked to parse. Other variants
may return an object or even the nearest perl6 equivalent grammar, but this
just returns a hash reference with nested array references.

If you're unfamiliar with ASTs, please check out the test suites, specifically
L<t/03-actions.t> in order to see what this particular action generates.

Broadly speaking you'll get back a hash reference containing data gleaned from
the ANTLR4 grammar, minus the comments and unimportant syntax. Where order is
important (and generally, even where it isn't) data will be kept in line in
an array reference, and usually these array references will have hash
references inside them.

The top-level keys are listed below, and contain the important stuff that can
be gleaned from a quick perusal of the grammar file. The C<content> key is the
most complex, and is described in detail at the appropriate place.

  =item name

  The name of the grammar, derived from 'grammar ECMAScript;'

  =item type

  The type of the grammar, either 'lexer' or 'parser' as specified in the text,
  or 'DEFAULT' if no type is specified.

  =item options

  An array reference of options specified in the grammar file.  The most common
  option is 'tokenVocab', which would appear as
  C<options => [ tokenVocab => 'ECMAScriptLexer' ]>.

  =item imports

  An array reference of grammar files the current file imports, and their
  optional aliases. This action doesn't load imported files, but feel free
  to do so on your own.

  =item tokens

  An array reference of token names predefined in other grammar files, such as
  the files in the C<imports> key. While tokens may be defined in other files,
  they're beyond the scope of this action.

  =item actions

  An array reference consisting of the actions performed by the top level of the
  grammar. It's just a reference to a single pair, even though the grammar
  doesn't seem to support multiple actions at the top level. Again, an array
  reference just for consistency's sake.

  The action text itself will remain unparsed, mostly because it's a
  monolithic block of Java code. If you're converting a grammar from ANTLR4 to
  Perl6 you'll need to take note of this behavior, but it's currently beyond
  the scope of this action to parse the text here.

  =item contents

  To preserve ordering in case we want to round-trip ANTLR-Perl6-ANTLR, this
  is also an array reference. It's also the most complex of the data
  structures in the module.

  At this juncture you may want to keep L<t/03-actions.t> open in order to
  follow along with the story.

  The C<contents> key is an arrayref of hashrefs. Each of these hashrefs
  contains a full rule, laid out in a more or less conistent fashion. All of
  these hashrefs contain a fixed set of keys, only two of them important to
  Perl6 users in general.

  The C<name> and C<contents> are the most important items, C<name> is the
  rule's name (go figure) and C<contents> being the actual meat of the rule.
  C<attribute>, C<action>, C<return> and C<throws> are useful for the Java
  author to restrict visibiity of the rule, and add additional arguments to the
  Java method that is called by the generated parser.

  The real fun begins inside the C<contents> key. Even a simple ANTLR4 rule
  such as C<number : digits ;> will have several layers of what looks like
  redundant nesting. This is mostly for consistency's sake, and might change
  later on, especially for single-term rules where you wouldn't expect the
  complexities of nesting.

  The ANTLR4 grammar assumes that rules always start with a list of
  alternatives, even if there's only one alternative. These alternatives
  can themselves be a list of concatenations, even if there's only one
  term to be "concatenated", thus appearing redundant.

  ANTLR4 has two general kinds of groups - Implicit groups and explicit.
  Implicit groups are those inferred from their surroundings, such as the
  concatenation implicit in C<number : sign? digits ;>. Explicit groups are
  those that override ANTLR4's assumptions, such as C<number : sign? (a b)?>.

  Groups will always be a hashref, with a C<type> and C<contents> key. The type
  is one of C<alternation>, C<concatenation> or C<capturing>. The contents
  will always be an arrayref of either groups or terms.

  Terms are the basics of the grammar, such as C<'foo'>, C<[0-9]+> or
  C<digits>. Each term has a C<type>, C<contents>, C<modifier>, C<greedy> and
  C<complemented> key.

  The C<type> is one of C<terminal>, C<nonterminal> or C<character class>.
  The contents is the actual text of the term (such as C<foo> if the term is
  C<'foo'>, or the individual "characters" of the character class.

  The C<modifier> is the C<+>, C<*> or C<?> modifier at the end of the term,
  or C<Nil> if no modifier is present. Just like in Perl6, terms can have
  greedy quantifiers, and that's set by the C<greedy> flag. The
  C<complemented> flag acts similarly, since terms can be complemented like
  C<~'foo'> meaning "No 'foo' occurs here".

=end pod

use v6;

unit class ANTLR4::Actions::AST;

method TOP($/) {
    make {
        name  => ~$<name>,
        type  => $<grammarType>.made,
        rules => $<ruleSpec>».made,
        |<options imports tokens actions>.map(
            -> $key {
                $key => $<prequelConstruct>.grep({ $_{$key} }).map({ |$_{$key}.made })
            }
        ),
    }
}

method throwsSpec($/) {
    make $<ID>».made;
}

method action($/) {
    make ~$<action_name> => ~$<ACTION>;
}

method tokensSpec($/) {
    make $<ID_list_trailing_comma>.made;
}

method ID_list_trailing_comma($/) {
    make $<ID>».made;
}

method delegateGrammars($/) {
    make $<delegateGrammar>».made;
}

method delegateGrammar($/) {
    make ~$<key> => $<value>.made;
}

method optionsSpec($/) {
    make $<option>».made;
}

method option($/) {
    make ~$<key> => $<optionValue>.made;
}

method elementOptions($/) {
    make $<option>».made;
}

method elementOption($/) {
    make ~$<key> => $<value>.made;
}

method optionValue($/) {
    make $<DIGITS> ?? +$<DIGITS>            !! $<STRING_LITERAL>
                   ?? ~$<STRING_LITERAL>[0] !! $<ID_list>.made;
}

method ID_list($/) {
    make $<ID>».made;
}

method ID($/) {
    make ~$/;
}

method grammarType($/) {
    make ~$/[0] if $/[0];
}

method ruleSpec($/) {
    make $<parserRuleSpec>.made || $<lexerRuleSpec>.made;
}

method lexerRuleSpec($/) {
    make {
        name      => ~$<name>,
        content   => $<lexerAltList>.made,
    }
}

method parserRuleSpec($/) {
    make {
        name      => ~$<name>,
        content   => $<parserAltList>.made,
        attribute => $<attribute> ?? ~$<attribute>           !! Nil,
        action    => $<action>    ?? ~$<action>              !! Nil,
        returns   => $<returns>   ?? ~$<returns><ARG_ACTION> !! Nil,
        throws    => $<throws>    ??  $<throws>.made         !! Nil,
        locals    => $<locals>    ?? ~$<locals><ARG_ACTION>  !! Nil,
        options   => $<options>   ??  $<options>.made        !! Nil,
    }
}

method lexerAltList($/) {
    # There must be a nicer way...
    my @contents = $<lexerAlt>».made;
    # @contents.append: |$<lexerAlt>».made;
    if @contents.elems == 1 {
        make @contents[0];
    }
    else {
        make {
            type     => 'alternation',
            contents => @contents,
        }
    }
}

method parserAltList($/) {
    if $<parserAlt>.elems == 1 {
        make $<parserAlt>[0].made;
    }
    else {
        make {
            type     => 'alternation',
            contents => $<parserAlt>».made,
        }
    }
}

method lexerAlt($/) {
    my @elements = $<lexerElement>».made;

    if @elements.elems == 1 {
        make @elements[0];
    }
    else {
        make {
            type     => 'concatenation',
            contents => @elements,
        }
    }

    if $<lexerCommands> {
        $/.made<commands> = $<lexerCommands>.made;
    }
}

method lexerCommands($/) {
    make $<lexerCommand>».made;
}

method lexerCommand($/) {
    make { ~$<ID> => $/<lexerCommandExpr>[0] ~~ Nil ?? Nil !! ~$/<lexerCommandExpr>[0] };
}

method parserAlt($/) {
    make $<parserElement>.made;
    $/.made<options label> =
        $<parserElement><options> ?? $<parserElement><options>.made !! Nil,
        $<label>                  ?? ~$<label>                      !! Nil;
}

method blockAltList($/) {
    make {
        type     => 'alternation',
        contents => $<parserElement>».made,
    }
}

method parserElement($/) {
    my @contents = $<element>».made;
    if @contents.elems == 1 {
        make @contents[0];
    }
    else {
        make {
            type     => 'concatenation',
            contents => @contents,
        }
    }
}

method element($/) {
    my Str $modifier = $<ebnfSuffix><MODIFIER>        ?? ~$<ebnfSuffix><MODIFIER>
                    !! $<ebnf><ebnfSuffix><MODIFIER>  ?? ~$<ebnf><ebnfSuffix><MODIFIER> !! '';
    my Bool $greedy  = $<ebnfSuffix><GREED>       ?? True
                    !! $<ebnf><ebnfSuffix><GREED> ?? True !! False;

    if $<atom> {
        make $<atom>.made;
    }
    elsif $<ACTION> {
        make {
            type    => 'action',
            content => ~$<ACTION>,
        }
    }
    elsif $<ebnf><block> {
        make $<ebnf><block>.made;
    }

    $/.made<modifier> = $modifier;
    $/.made<greedy>   = $greedy;
}

method atom($/) {
    if $<notSet> {
        my $notSet = $<notSet>;
        if $notSet<setElement> {
            make $notSet<setElement>.made;
        }
        elsif $notSet<blockSet> {
            make $notSet<blockSet>.made;
        }

        $/.made<complemented> = True;
    }
    elsif $<LEXER_CHAR_SET> {
        make $<LEXER_CHAR_SET>.made;
    }
    elsif $<range> {
        make $<range>.made;
    }
    elsif $<terminal> {
        make $<terminal>.made;
    }
    else {
        make {
            type    => 'regular expression',
            content => $/.Str.trim,
        }
    }
}

method lexerElement($/) {
    my Str $modifier = $<ebnfSuffix><MODIFIER> ?? ~$<ebnfSuffix><MODIFIER> !! '';
    my Bool $greedy  = $<ebnfSuffix><GREED> ?? True !! False;

    if $<lexerAtom> {
        make $<lexerAtom>.made;
    }
    elsif $<lexerBlock> {
        make $<lexerBlock>.made;
    }

    $/.made<modifier> = $modifier;
    $/.made<greedy>   = $greedy;
}

method lexerBlock($/) {
    if $<NOT> {
	# ignore me...
	# there are pretty good reasons for not having negated character groups...
	# the reason for all this madness is the following antlr rule:
	#SYMBOL_HEAD
	#    : ~('0' .. '9'
	#        | '^' | '`' | '\'' | '"' | '#' | '~' | '@' | ':' | '/' | '%' | '(' | ')' | '[' | ']' | '{' | '}' // FIXME: could be one group
	#        | [ \n\r\t\,] // FIXME: could be WS
	#        )
	#    ;
	my $lexer-alt-list = $<lexerAltList>.made;
	my @contents = $lexer-alt-list<contents>:exists
	    ?? $lexer-alt-list<contents>.flat
            !! [$lexer-alt-list];

	make {
	    type         => 'character class',
	    contents     =>  @contents,
	    complemented => True,
	}
    }
    else {
	make {
	    type         => 'capturing group',
	    content      =>  $<lexerAltList>.made,
	    complemented => $<NOT> ?? True !! False,
	}
    }
}

method block($/) {
    make {
        type         => 'capturing group',
        content      =>  $<blockAltList>.made,
        complemented => $<NOT> ?? True !! False,
    }
}

method lexerAtom($/) {
    if $<notSet> {
        my $notSet = $<notSet>;
        if $notSet<setElement> {
            make $notSet<setElement>.made;
        }
        elsif $notSet<blockSet> {
            make $notSet<blockSet>.made;
        }
        $/.made<complemented> = True;
    }
    elsif $<LEXER_CHAR_SET> {
        make $<LEXER_CHAR_SET>.made;
    }
    elsif $<range> {
        make $<range>.made;
    }
    elsif $<terminal> {
        make $<terminal>.made;
    }
    else {
        make {
            type    => 'regular expression',
            content => $/.Str.trim,
        }
    }
}

method LEXER_CHAR_SET($/) {
    make {
        type     => 'character class',
        contents => $/[0]».<LEXER_CHAR_SET_RANGE>».made,
    }
}

method LEXER_CHAR_SET_RANGE($/) {
    make ~$/ eq ' ' ?? '\s' !! ~$/;
}

method range($/) {
    make {
        type => 'range',
        from => ~$<from>[0],
        to   => ~$<to>[0],
    }
}

method setElementAltList($/) {
    my @contents = $<setElement>».made;

    if @contents.elems == 1 {
        make @contents[0];
    }
    else {
        make {
            type     => 'alternation',
            contents => @contents,
        }
    }
}

method setElement($/) {
    if $<LEXER_CHAR_SET> {
        make $<LEXER_CHAR_SET>.made;
    }
    else {
        make {
            type    => $<terminal><STRING_LITERAL> ?? 'terminal' !! 'nonterminal',
            content => $/.Str.trim,
        }
    }
}

method blockSet($/) {
    # TODO: is this always used complemened?
    make {
	type    => 'character class',
	contents => $<setElementAltList>.made<contents>,
    }
}

method terminal($/) {
    make {
        type    => $<STRING_LITERAL> ?? 'terminal' !! 'nonterminal',
        content => $/.Str.trim,
    }
}
