A lexer for syntaxes that use shell-like rules for quoting and commenting. It is a port of the `shlex` module from Python’s standard library.

The point of entry is `split`:

    (shlex:split "a 'b c' \"d e\" \"f ' g\"")
    => '("a" "b c" "d e" "f ' g")

For a parse that is closer to a shell, you can pass the `:punctuation-chars` keyword:

    ;; Not what you want.
    (shlex:split "a && b; c && d || e; f >'abc'; (def \"ghi\")")
    => '("a" "&&" "b;" "c" "&&" "d" "||" "e;" "f" ">abc;" "(def" "ghi)")

    (shlex:split "a && b; c && d || e; f >'abc'; (def \"ghi\")" :punctuation-chars t)
    => ("a" "&&" "b" ";" "c" "&&" "d" "||" "e" ";" "f" ">" "abc" ";" "(" "def" "ghi" ")")
