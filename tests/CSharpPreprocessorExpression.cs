#region Copyright (c) 2019 Atif Aziz. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
#endregion

namespace CSharp.Preprocessing
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Linq;
    using Parser = Gratt.Parser<ParseContext, TokenKind, Token, Precedence, bool>;
    using PrefixParselet = System.Func<Token, Gratt.Parser<ParseContext, TokenKind, Token, Precedence, bool>, bool>;
    using InfixParselet = System.Func<Token, bool, Gratt.Parser<ParseContext, TokenKind, Token, Precedence, bool>, bool>;

    //
    // This is an implementation of a C# pre-processing expression parser found in conditional
    // directives:
    //
    // https://github.com/dotnet/csharplang/blob/892af9016b3317a8fae12d195014dc38ba51cf16/spec/lexical-structure.md#pre-processing-expressions
    //
    // It uses Pratt parser to parse and evaluate the expression immediately as opposed to returning
    // an abstract syntax tree (AST) of the expression.
    //

    static class PreprocessorExpression
    {
        public static bool Evaluate(string expression, Func<string, bool> symbolPredicate) =>
            Gratt.Parser.Parse(
                new ParseContext(expression, symbolPredicate),
                Precedence.Default,
                (_, token, __) => Spec.Instance.Prefix(token),
                (kind, _, __) => Spec.Instance.Infix(kind),
                from t in Scanner.Scan(expression)
                where t.Kind != TokenKind.WhiteSpace
                select (t.Kind, t));
    }

    sealed class ParseContext
    {
        public string SourceText { get; }
        public Func<string, bool> SymbolPredicate { get; }

        public ParseContext(string sourceText, Func<string, bool> symbolPredicate)
        {
            SourceText = sourceText;
            SymbolPredicate = symbolPredicate;
        }
    }

    enum TokenKind
    {
        WhiteSpace,
        Symbol,
        True,
        False,
        AmpersandAmpersand,
        PipePipe,
        LParen,
        RParen,
        Bang,
        EqualEqual,
        BangEqual,
        Eoi,
    }

    readonly struct Token : IEquatable<Token>
    {
        public readonly TokenKind Kind;
        public readonly int StartOffset;
        public readonly int EndOffset;

        public Token(TokenKind kind, int startOffset, int endOffset) =>
            (Kind, StartOffset, EndOffset) = (kind, startOffset, endOffset);

        public int Length => EndOffset - StartOffset;

        public bool Equals(Token other) =>
            Kind == other.Kind
            && StartOffset.Equals(other.StartOffset)
            && EndOffset.Equals(other.EndOffset);

        public override bool Equals(object obj) =>
            obj is Token other && Equals(other);

        public override int GetHashCode() =>
            unchecked(((((int)Kind * 397) ^ StartOffset.GetHashCode()) * 397) ^ EndOffset.GetHashCode());

        public static bool operator ==(Token left, Token right) => left.Equals(right);
        public static bool operator !=(Token left, Token right) => !left.Equals(right);

        public override string ToString() =>
            $"{Kind} [{StartOffset}..{EndOffset})";

        public string Substring(string source) =>
            source.Substring(StartOffset, Length);
    }

    enum Precedence
    {
        Default    = 0,
        Logical    = 10, // || &&
        Relational = 20, // == !=
        Prefix     = 30, // !
    }

    class SyntaxErrorException : Exception
    {
        public SyntaxErrorException() {}
        public SyntaxErrorException(string message) : base(message) {}
        public SyntaxErrorException(string message, Exception inner) : base(message, inner) {}
    }

    sealed class Spec : IEnumerable
    {
        public static readonly Spec Instance = new Spec
        {
            { TokenKind.Symbol, (token, parser) => parser.State.SymbolPredicate(token.Substring(parser.State.SourceText)) },

            { TokenKind.True , delegate { return true;  } },
            { TokenKind.False, delegate { return false; } },

            {
                TokenKind.LParen, (token, parser) =>
                {
                    var result = parser.Parse(0);
                    parser.Read(TokenKind.RParen, (TokenKind expected, (TokenKind, Token Token) actual) =>
                                    throw new SyntaxErrorException($"Expected {expected} token at {actual.Token.StartOffset}."));
                    return result;
                }
            },

            { TokenKind.Bang, (_, parser) => !parser.Parse(Precedence.Prefix) },

            { TokenKind.AmpersandAmpersand, Precedence.Logical   , (a, rbp, p) => a && p.Parse(rbp) },
            { TokenKind.PipePipe          , Precedence.Logical   , (a, b) => a || b },
            { TokenKind.EqualEqual        , Precedence.Relational, (a, b) => a == b },
            { TokenKind.BangEqual         , Precedence.Relational, (a, b) => a != b },
        };

        Spec() { }

        readonly Dictionary<TokenKind, PrefixParselet> _prefixes = new Dictionary<TokenKind, PrefixParselet>();
        readonly Dictionary<TokenKind, (Precedence, InfixParselet)> _infixes = new Dictionary<TokenKind, (Precedence, InfixParselet)>();

        void Add(TokenKind type, PrefixParselet prefix) =>
            _prefixes.Add(type, prefix);

        void Add(TokenKind type, Precedence precedence, InfixParselet prefix) =>
            _infixes.Add(type, (precedence, prefix));

        void Add(TokenKind type, Precedence precedence, Func<bool, bool, bool> f) =>
            Add(type, precedence, (token, left, parser) => f(left, parser.Parse(precedence)));

        void Add(TokenKind type, Precedence precedence, Func<bool, Precedence, Parser, bool> f) =>
            Add(type, precedence, (token, left, parser) => f(left, precedence, parser));

        public PrefixParselet Prefix(Token token)
            => _prefixes.TryGetValue(token.Kind, out var v) ? v
             : throw new SyntaxErrorException($"Unexpected <{token.Kind}> token at offset {token.StartOffset}.");

        public (Precedence, InfixParselet)? Infix(TokenKind type) =>
            _infixes.TryGetValue(type, out var v) ? ((Precedence, InfixParselet)?)v : null;

        IEnumerator IEnumerable.GetEnumerator() =>
            _prefixes.Cast<object>().Concat(_infixes.Cast<object>()).GetEnumerator();
    }

    static class Scanner
    {
        public static IEnumerable<Token> Scan(string expression)
        {
            if (expression == null) throw new ArgumentNullException(nameof(expression));
            return ScanImpl(expression);
        }

        enum State
        {
            Scan,
            IdentifierOrTrue,
            IdentifierOrFalse,
            True,
            False,
            Symbol,
            Ampersand,     // &
            Pipe,          // |
            Equal,         // =
            Bang,          // !
            WhiteSpace,    // space or tab
        }

        static IEnumerable<Token> ScanImpl(string s)
        {
            var resetState = false;

            Token Token(TokenKind kind, int fi, int ei)
            {
                resetState = true;
                return new Token(kind, fi, ei);
            }

            var state = State.Scan;
            var si = 0;
            var i = 0;
            for (; i < s.Length; i++)
            {
                var ch = s[i];
            restart:
                if (resetState)
                    (state, resetState) = (State.Scan, false);
                switch (state)
                {
                    case State.Scan:
                    {
                        switch (ch)
                        {
                            case ' ': case '\t': state = State.WhiteSpace; break;
                            case 't': state = State.IdentifierOrTrue; break;
                            case 'f': state = State.IdentifierOrFalse; break;
                            case '&': state = State.Ampersand; break;
                            case '|': state = State.Pipe; break;
                            case '!': state = State.Bang; break;
                            case '=': state = State.Equal; break;
                            case '(': yield return Token(TokenKind.LParen, i, i + 1); break;
                            case ')': yield return Token(TokenKind.RParen, i, i + 1); break;
                            case var c when char.IsLetter(c): state = State.Symbol; break;
                            default:
                                throw new SyntaxErrorException($"Unexpected at offset {i}: {ch}");
                        }
                        si = i;
                        break;
                    }
                    case State.IdentifierOrTrue:
                    {
                        switch (i - si)
                        {
                            case 1 when ch == 'r': break;
                            case 2 when ch == 'u': break;
                            case 3 when ch == 'e':
                                state = State.True;
                                break;
                            default:
                                state = State.Symbol;
                                goto restart;
                        }
                        break;
                    }
                    case State.IdentifierOrFalse:
                    {
                        switch (i - si)
                        {
                            case 1 when ch == 'a': break;
                            case 2 when ch == 'l': break;
                            case 3 when ch == 's': break;
                            case 4 when ch == 'e':
                                state = State.False;
                                break;
                            default:
                                state = State.Symbol;
                                goto restart;
                        }
                        break;
                    }
                    case State.True:
                    case State.False:
                    {
                        if (char.IsLetterOrDigit(ch))
                        {
                            state = State.Symbol;
                            break;
                        }
                        else
                        {
                            yield return Token(state == State.True ? TokenKind.True : TokenKind.False, si, i);
                            goto restart;
                        }
                    }
                    case State.Symbol:
                    {
                        if (char.IsLetterOrDigit(ch))
                            break;
                        yield return Token(TokenKind.Symbol, si, i);
                        goto restart;
                    }
                    case State.WhiteSpace:
                    {
                        if (ch == ' ' || ch == '\t')
                            break;
                        yield return Token(TokenKind.WhiteSpace, si, i);
                        goto restart;
                    }
                    case State.Ampersand:
                    {
                        if (ch != '&')
                            throw new SyntaxErrorException($"Unexpected at offset {i}: {ch}");
                        yield return Token(TokenKind.AmpersandAmpersand, si, i + 1);
                        break;
                    }
                    case State.Pipe:
                    {
                        if (ch != '|')
                            throw new SyntaxErrorException($"Unexpected at offset {i}: {ch}");
                        yield return Token(TokenKind.PipePipe, si, i + 1);
                        break;
                    }
                    case State.Equal:
                    {
                        if (ch != '=')
                            throw new SyntaxErrorException($"Unexpected at offset {i}: {ch}");
                        yield return Token(TokenKind.EqualEqual, si, i + 1);
                        break;
                    }
                    case State.Bang:
                    {
                        if (ch == '=')
                        {
                            yield return Token(TokenKind.BangEqual, si, i + 1);
                            break;
                        }
                        else
                        {
                            yield return Token(TokenKind.Bang, si, i);
                            goto restart;
                        }
                    }
                    default:
                        throw new Exception("Internal error due to unhandled state: " + state);
                }
            }

            TokenKind kind;

            switch (state)
            {
                case State.Scan:
                    goto eoi;
                case State.IdentifierOrTrue:
                case State.IdentifierOrFalse:
                case State.Symbol: kind = TokenKind.Symbol; break;
                case State.WhiteSpace: kind = TokenKind.WhiteSpace; break;
                case State.True: kind = TokenKind.True; break;
                case State.False: kind = TokenKind.False; break;
                case State.Bang: kind = TokenKind.Bang; break;
                case State.Ampersand:
                case State.Pipe:
                case State.Equal:
                    throw new SyntaxErrorException($"Unexpected end of input at offset {i}.");
                default:
                    throw new Exception("Internal error due to unhandled state: " + state);
            }

            yield return Token(kind, si, i);

            eoi:
            yield return Token(TokenKind.Eoi, i, i);
        }
    }
}
