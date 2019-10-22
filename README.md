# Gratt

Gratt is a _generic_ implementation of a [Pratt parser] in C# for .NET
Standard.

A _Pratt parser_ is the affectionate name for a simple parsing
technique presented by [Vaughn Pratt][pratt] in his 1973 paper “[Top
down operator precedence][TDOP]” ([TDOP]).

Gratt was inspired by the design of the Java implementation presented by [Bob
Nystrom] in his journal entry “[Pratt Parsers: Expression Parsing Made
Easy][easy-pratt]”. The actual inspiration was the simplification and
separation of interfaces in terms of _prefix_ (_nud_) and _infix (led)
parselets_ (rather than the full Java implementation):

```java
interface PrefixParselet {
  Expression parse(Parser parser, Token token);
}

interface InfixParselet {
  Expression parse(Parser parser, Expression left, Token token);
}
```

The unit tests for Gratt re-create and test the parser for the toy language
[Bantam] discussed in Bob's journal entry.

The unit tests also contain a complete implementation for parsing and
evaluating [C# pre-processing expressions][cs-pp-expr] used in conditional
directives `#if` and `#elif`.

Unlike Bob's Java implementation, Gratt's parser is completely generic:

```c#
class Parser<TState, TKind, TToken, TPrecedence, TResult>
{
    // ...
}
```

It can therefore work with any model of tokens (`TToken`), token types
(`TKind`), precedence (`TPrecedence`) and result (`TResult`). It can also hold
any user-defined state (`TState`) that may be needed during parsing although it
is not required. An overload without any user-state simply assumes a
[_unit_][unit].

Unlike Bob's Java implementation, Gratt does not define any interfaces to
represent (prefix or infix) parselets. Instead, it relies on them being simple
functions:

```c#
public static TResult
    Parse<TState, TKind, TToken, TPrecedence, TResult>(
        TState state,
        TPrecedence initialPrecedence, IComparer<TPrecedence> precedenceComparer,
        IEqualityComparer<TKind> kindEqualityComparer,
        Func<TKind, TToken, TState, Func<TToken, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>> prefixFunction,
        Func<TKind, TToken, TState, (TPrecedence, Func<TToken, TResult, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixFunction,
        IEnumerable<(TKind, TToken)> lexer)
```

The above might read a little dense so below is a brief explanation of each
parameter in order:

- a user-defined state that is associated with the parser object during parsing
- the initial precedence (this is usually 0 if `TPrecedence` is an `int`)
- a precedence comparer
- an equality comparer that can compare two token kinds
- a prefix function
- an infix function
- a sequence of token kind and token pairs (2-tuple) yielded by a lexer
  implementation

A prefix function receives a token kind, a token and a user-defined state as
input and it _must_ return a parsing function if the token kind has prefix
parsing semantics. If the token kind is invalid as a prefix, then the token and
user-defined state may be used for the purpose of providing details in a thrown
exception (e.g., while `TKind` may be a simple `enum` type, `TToken` will
usually be a rich object containing the position of the token in the source
text among other data). The parsing function returned by the prefix function
then receives the token and the parser as arguments and produces some result
(`TResult`).

An infix function receives a token kind, a token and a user-defined state as
input as arguments and _optionally_ returns a precedence (_left binding power_)
and parsing function pair. The parsing function then receives the token, the
left result and the parser as arguments and produces some result (`TResult`).


[pratt]: https://en.wikipedia.org/wiki/Vaughan_Pratt
[TDOP]: https://web.archive.org/web/20151223215421/http://hall.org.ua/halls/wizzard/pdf/Vaughan.Pratt.TDOP.pdf
[Pratt parser]: https://en.wikipedia.org/wiki/Pratt_parser
[Bob Nystrom]: http://stuffwithstuff.com/
[easy-pratt]: http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
[Bantam]: https://github.com/munificent/bantam
[unit]: https://docs.microsoft.com/en-us/dotnet/api/system.valuetuple
[cs-pp-expr]: https://github.com/dotnet/csharplang/blob/892af9016b3317a8fae12d195014dc38ba51cf16/spec/lexical-structure.md#pre-processing-expressions
