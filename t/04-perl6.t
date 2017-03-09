use v6;
use Test;
use ANTLR4;

plan 9;

subtest sub {
    is g4-to-perl6( q{grammar Minimal;} ),
       q{grammar Minimal {  }},
       'minimal grammar';

    is g4-to-perl6( q{lexer grammar Minimal;} ),
       q{grammar Minimal {  } #={ "type" : "lexer" }},
       'optional type';

    is g4-to-perl6( q{grammar Minimal; options {a=2;}} ),
       q{grammar Minimal {  } #={ "options" : [ { "a" : 2 } ] }},
       'optional options';

    is g4-to-perl6( q{grammar Minimal; import Foo;} ),
       q{grammar Minimal {  } #={ "imports" : [ { "Foo" : null } ] }},
        'optional import';

    is g4-to-perl6( q{grammar Minimal; tokens { INDENT, DEDENT }} ),
       q{grammar Minimal {  } #={ "tokens" : [ "INDENT", "DEDENT" ] }},
       'optional tokens';

    is g4-to-perl6( q{grammar Minimal; @members { int i = 0; }} ),
      q{grammar Minimal {  } #={ "actions" : [ { "@members" : "{ int i = 0; }" } ] }},
      'optional actions';
}, 'Grammar and its options';

subtest sub {
    is g4-to-perl6( q{grammar Minimal; number : '1' ;}),
       q{grammar Minimal { rule number { '1' } }},
       'minimal rule';

    subtest sub {
        is g4-to-perl6( q{grammar Minimal; number : '1'* ;}),
           q{grammar Minimal { rule number { '1'* } }},
           'star';

        is g4-to-perl6( q{grammar Minimal; number : '1'+ ;}),
           q{grammar Minimal { rule number { '1'+ } }},
           'plus';

        is g4-to-perl6( q{grammar Minimal; number : ~'1' ;}),
           q{grammar Minimal { rule number { <-[ 1 ]> } }},
          'complement';

        is g4-to-perl6( q{grammar Minimal; number : '1'*? ;}),
           q{grammar Minimal { rule number { '1'*? } }},
           'greedy star';
    }, 'terminal with options';

    subtest sub {
        is g4-to-perl6( q{grammar Minimal; number : 'a' ;}),
           q{grammar Minimal { rule number { 'a' } }},
           'alpha terminal';

        is g4-to-perl6( q{grammar Minimal; number : 'a123b' ;}),
           q{grammar Minimal { rule number { 'a123b' } }},
           'mixed alphanumeric terminal';

        is g4-to-perl6( q{grammar Minimal; number : '\u263a' ;}),
           q{grammar Minimal { rule number { '\x[263a]' } }},
           'Unicode terminal';
    }, 'terminal of different types';

    subtest sub {
        is g4-to-perl6( q{grammar Minimal; protected number : '1';}),
           q{grammar Minimal { rule number { '1' } #={ "attribute" : "protected" } }},
           'rule with attribute';

        is g4-to-perl6( q{grammar Minimal; number [int x] : '1';}),
           q{grammar Minimal { rule number { '1' } #={ "action" : "[int x]" } }},
           'optional action';

        is g4-to-perl6( q{grammar Minimal; number returns [int x] : '1';}),
           q{grammar Minimal { rule number { '1' } #={ "returns" : "[int x]" } }},
           'optional return type';

        is g4-to-perl6( q{grammar Minimal; number throws XFoo : '1';}),
           q{grammar Minimal { rule number { '1' } #={ "throws" : [ "XFoo" ] } }},
           'optional exception';

        is g4-to-perl6( q{grammar Minimal; number locals [int y] : '1';}),
           q{grammar Minimal { rule number { '1' } #={ "locals" : "[int y]" } }},
           'optional local';

        is g4-to-perl6( q{grammar Minimal; number options{a=2;} : '1';}),
           q{grammar Minimal { rule number { '1' } #={ "options" : [ { "a" : 2 } ] } }},
           'optional local variables';
    }, 'Single rule and rule-level options';

    subtest sub {
        is g4-to-perl6( q{grammar Minimal; number : <assoc=right> '1' ;}),
           q{grammar Minimal { rule number { '1' #={ "options" : [ { "assoc" : "right" } ] } } }},
           'optional option';

        is g4-to-perl6( q{grammar Minimal; number : '1' # One ;}),
           q{grammar Minimal { rule number { '1' #={ "label" : "One" } } }},
           'optional label';

        is g4-to-perl6( q{grammar Minimal; number : '1' -> skip ;}),
           q{grammar Minimal { rule number { '1' #={ "commands" : [ { "skip" : null } ] } } }},
           'optional command';

        is g4-to-perl6( q{grammar Minimal; number : {$amount = 0;} '1' ;}),
           q{grammar Minimal { rule number { #={ "content" : "{$amount = 0;}" } '1' } }},
           'optional action';
    }, 'Single rule and term-level options';
}

subtest sub {
    is g4-to-perl6( q{grammar Minimal; number : ab ;}),
       q{grammar Minimal { rule number { <ab> } }},
       'non-terminal';

    subtest sub {
        is g4-to-perl6( q{grammar Minimal; number : ab* ;}),
           q{grammar Minimal { rule number { <ab>* } }},
           'star';

        is g4-to-perl6( q{grammar Minimal; number : ab+ ;}),
           q{grammar Minimal { rule number { <ab>+ } }},
           'plus';

        is g4-to-perl6( q{grammar Minimal; number : ~ab ;}),
           q{grammar Minimal { rule number { <!ab> } }},
           'complement';

        is g4-to-perl6( q{grammar Minimal; number : ab*? ;}),
           q{grammar Minimal { rule number { <ab>*? } }},
           'greedy star';
    }, 'non-terminal modifiers';

    is g4-to-perl6( q{grammar Minimal; number : 'a'..'z' ;}),
       q{grammar Minimal { rule number { 'a'..'z' } }},
       'range';

    is g4-to-perl6( q{grammar Minimal; number : '\u263a'..'\u263f' ;}),
       q{grammar Minimal { rule number { '\x[263a]'..'\x[263f]' } }},
       'Unicode range';

    subtest sub {
        is g4-to-perl6( q{grammar Minimal; number : 'a'..'z'* ;}),
           q{grammar Minimal { rule number { 'a'..'z'* } }},
           'star';

        is g4-to-perl6( q{grammar Minimal; number : 'a'..'z'+ ;}),
           q{grammar Minimal { rule number { 'a'..'z'+ } }},
           'plus';
        #
        # The grammar doesn't allow ~'a'..'z', so skip it.
        #
        #is g4-to-perl6( q{grammar Minimal; number : ~'a'..'z' ;}),
        #   q{grammar Minimal { rule number { ( ( !'a'..z' ) ) } }},
        #   'complement';

        is g4-to-perl6( q{grammar Minimal; number : 'a'..'z'*? ;}),
           q{grammar Minimal { rule number { 'a'..'z'*? } }},
           'greedy star';
    }, 'range modifiers';

    is g4-to-perl6( q{grammar Minimal; number : [] ;}),
       q{grammar Minimal { rule number { <[  ]> } }},
       'empty character class';

    subtest sub {
        is g4-to-perl6( q{grammar Minimal; number : []* ;}),
           q{grammar Minimal { rule number { <[  ]>* } }},
           'star';

        is g4-to-perl6( q{grammar Minimal; number : []+ ;}),
           q{grammar Minimal { rule number { <[  ]>+ } }},
           'plus';

        is g4-to-perl6( q{grammar Minimal; number : ~[] ;}),
           q{grammar Minimal { rule number { <-[  ]> } }},
           'complement';

        is g4-to-perl6( q{grammar Minimal; number : []*? ;}),
           q{grammar Minimal { rule number { <[  ]>*? } }},
           'greedy star';
    }, 'empty character class modifiers';

    is g4-to-perl6( q{grammar Minimal; number : [a] ;}),
       q{grammar Minimal { rule number { <[ a ]> } }},
       'character class';

    is g4-to-perl6( q{grammar Minimal; number : [ ] ;}),
       q{grammar Minimal { rule number { <[ \s ]> } }},
       'character class';

    subtest sub {
        is g4-to-perl6( q{grammar Minimal; number : [a]* ;}),
           q{grammar Minimal { rule number { <[ a ]>* } }},
           'star';

        is g4-to-perl6( q{grammar Minimal; number : [a]+ ;}),
           q{grammar Minimal { rule number { <[ a ]>+ } }},
           'plus';

        is g4-to-perl6( q{grammar Minimal; number : ~[a] ;}),
           q{grammar Minimal { rule number { <-[ a ]> } }},
           'complement';

        is g4-to-perl6( q{grammar Minimal; number : [a]*? ;}),
           q{grammar Minimal { rule number { <[ a ]>*? } }},
           'greedy star';
    }, 'character class modifiers';

    subtest sub {
        is g4-to-perl6( q{grammar Minimal; number : [a-b] ;}),
           q{grammar Minimal { rule number { <[ a .. b ]> } }},
           'hyphenated character class';

        is g4-to-perl6( q{grammar Minimal; number : [-a-b] ;}),
           q{grammar Minimal { rule number { <[ - a .. b ]> } }},
           'hyphenated character class';

        is g4-to-perl6( q{grammar Minimal; number : [-a-b\u000d] ;}),
           q{grammar Minimal { rule number { <[ - a .. b \\x[000d] ]> } }},
           'Unicode character class';
    }, 'character class variants';

    is g4-to-perl6( q{grammar Minimal; number : . ;} ),
       q{grammar Minimal { rule number { . } }},
       'regular expression';

}, 'Single rule and remaining basic term types';

subtest sub {
    is g4-to-perl6( q{grammar Minimal; number : 'a' 'b';}),
       q{grammar Minimal { rule number { 'a' 'b' } }},
       'two concatenated terms';
    is g4-to-perl6( q{grammar Minimal; number : 'a' 'b' -> skip ;}),
       q{grammar Minimal { rule number { 'a' 'b' #={ "commands" : [ { "skip" : null } ] } } }},
       'two concatenated terms with skipping';
}, 'concatenation test';

subtest sub {
    # TODO investigate this test further
    # is g4-to-perl6( q{grammar Minimal; number : 'a' | ;}),
    #    q{grammar Minimal { rule number { 'a' | (Nil) } }},
    #    'one term with blank alternation';
    is g4-to-perl6( q{grammar Minimal; number : 'a' | 'b';}),
       q{grammar Minimal { rule number { 'a' | 'b' } }},
       'two alternated terms';
    is g4-to-perl6( q{grammar Minimal; number : 'a' | 'b' -> skip ;}),
       q{grammar Minimal { rule number { 'a' | 'b' #={ "commands" : [ { "skip" : null } ] } } }},
       'two alternated terms with skipping';
}, 'alternation test';

subtest sub {
    is g4-to-perl6( q{grammar Minimal; number : <assoc=right> ~'1'+? ;}),
       q{grammar Minimal { rule number { <-[ 1 ]>+? #={ "options" : [ { "assoc" : "right" } ] } } }},
       'with option';

    is g4-to-perl6( q{grammar Minimal; number : ~'1'+? # One ;}),
       q{grammar Minimal { rule number { <-[ 1 ]>+? #={ "label" : "One" } } }},
       'with label';
}, 'concatenated options';

subtest sub {
    is g4-to-perl6( q{grammar Minimal; number : ~'1'+? -> skip ;}),
       q{grammar Minimal { rule number { <-[ 1 ]>+? #={ "commands" : [ { "skip" : null } ] } } }},
       'with complement';
}, 'concatenated commands';

subtest sub {
    is g4-to-perl6( q{grammar Minimal; number : ( '1' ) ;}),
       q{grammar Minimal { rule number { ( '1' ) } }},
       'redundant parenthesis';

    is g4-to-perl6( q{grammar Minimal; number : ( '1' '2' ) ;}),
       q{grammar Minimal { rule number { ( '1' '2' ) } }},
       'redundant parenthesis with two terms';

    is g4-to-perl6( q{grammar Minimal; number : ( '1' | '2' ) ;}),
       q{grammar Minimal { rule number { ( '1' | '2' ) } }},
       'redundant parenthesis with two terms';
}, 'rule with redundant parentheses';

subtest sub {
    my Str $with-blank-line =  q{
        /* comment */
        grammar Minimal;
        number : ( '1' ) ;
    };
    is g4-to-perl6( q{grammar Minimal; number : ( '1' ) ;}),
       q{grammar Minimal { rule number { ( '1' ) } }},
       'blank line with with comment';

    is g4-to-perl6( q{grammar Minimal; number : '1' ; // line comment }),
       q{grammar Minimal { rule number { '1' } }},
       'line comment';

    my Str $with-multiple-comments-at-beginning = q{
        /** Taken from "The Definitive ANTLR 4 Reference" by Terence Parr */

        // Derived from http://json.org
        grammar JSON;

        json
            : value
            ;
    };
    is g4-to-perl6($with-multiple-comments-at-beginning),
       q{grammar JSON { rule json { <value> } }},
       'multiple comments at beginning';

    my Str $character-class-with-space = q{
        grammar JSON;
        WS
            : [ \t\n\r] + -> skip
            ;
    };
    is g4-to-perl6($character-class-with-space),
       q{grammar JSON { rule WS { <[ \s \t \n \r ]>+ } }},
       'char class with space';

    my Str $repetition = q{
        grammar JSON;
        array
            : '[' value (',' value)* ']'
            | '[' ']'
            ;
    };
    is g4-to-perl6($repetition),
       q{grammar JSON { rule array { '[' ( <value>+ %% ',' ) ']' | '[' ']' } }},
       'repetition (generate "%%")';

    is g4-to-perl6(q{grammar CSV; STRING : '"' ('""'|~'"')* '"' ;}),
       q{grammar CSV { rule STRING { '"' ( '\"' | <-[ " ]> )* '"' } }},
       'complex quote escapes';

    is g4-to-perl6(q{grammar CSV; STRING : '"' ('""'|~'"')* '"' ;}),
       q{grammar CSV { rule STRING { '"' ( '\"' | <-[ " ]> )* '"' } }},
       'complex quote escapes (1)';

    my Str $sqlite-identifier = q{
        grammar SQLite;
        IDENTIFIER
            : '"' (~'"' | '""')* '"'
            | '`' (~'`' | '``')* '`'
            | '[' ~']'* ']'
            | [a-zA-Z_] [a-zA-Z_0-9]* // TODO check: needs more chars in set
            ;
    };
    is g4-to-perl6($sqlite-identifier),
       q{grammar SQLite { rule IDENTIFIER { '"' ( <-[ " ]> | '\"' )* '"' | '`' ( <-[ ` ]> | '``' )* '`' | '[' <-[ \] ]>* ']' | <[ a .. z A .. Z _ ]> <[ a .. z A .. Z _ 0 .. 9 ]>* } }},
       'complex quote escapes (2)';

    is g4-to-perl6(q{grammar CSV; row : field (',' field)* '\r'? '\n' ;}),
       q{grammar CSV { rule row { ( <field>+ %% ',' ) \r? \n } }},
       'carriage return and newline';

    is g4-to-perl6(q{grammar Minimal; group : ~('0' .. '9'); }),
       q{grammar Minimal { rule group { !( '0'..'9' ) } }},
       'negated capturing group';
}, 'longer fragments';

# # vim: ft=perl6
