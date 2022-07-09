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
    using Token = Token<TokenKind>;
    using Parser = Gratt.Parser<ParseContext, TokenKind, Token<TokenKind>, Precedence, bool>;
    using PrefixParselet = System.Func<Token<TokenKind>, Gratt.Parser<ParseContext, TokenKind, Token<TokenKind>, Precedence, bool>, bool>;
    using InfixParselet = System.Func<Token<TokenKind>, bool, Gratt.Parser<ParseContext, TokenKind, Token<TokenKind>, Precedence, bool>, bool>;

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
                TokenKind.Eoi, t => new SyntaxErrorException($"Unexpected <{t.Kind}> token at offset {t.Offset}."),
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

    readonly struct Token<T> : IEquatable<Token<T>>
    {
        public readonly T   Kind;
        public readonly int Offset;
        public readonly int Length;

        public Token(T kind, int offset, int length) =>
            (Kind, Offset, Length) = (kind, offset, length);

        public int EndOffset => Offset + Length;

        public bool Equals(Token<T> other) =>
            EqualityComparer<T>.Default.Equals(Kind, other.Kind)
            && Offset == other.Offset && Length == other.Length;

        public override bool Equals(object? obj) =>
            obj is Token<T> other && Equals(other);

        public override int GetHashCode() =>
            unchecked((((Kind is { } kind ? EqualityComparer<T>.Default.GetHashCode(kind) * 397 : 0) ^ Offset.GetHashCode()) * 397) ^ EndOffset.GetHashCode());

        public static bool operator ==(Token<T> left, Token<T> right) => left.Equals(right);
        public static bool operator !=(Token<T> left, Token<T> right) => !left.Equals(right);

        public override string ToString() =>
            $"{Kind} [{Offset}..{EndOffset})";

        public string Substring(string source) =>
            source.Substring(Offset, Length);
    }

    enum Precedence
    {
        Default    = 0,
        LogicalOr  = 10, // ||
        LogicalAnd = 15, // &&
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
                                    throw new SyntaxErrorException($"Expected {expected} token at {actual.Token.Offset}."));
                    return result;
                }
            },

            { TokenKind.Bang, (_, parser) => !parser.Parse(Precedence.Prefix) },
            // NOTE! The "&&" evaluation cannot be short-circuited since the
            //       right side needs to be parsed to ensure it is syntactically
            //       correct and the end-of-input is reached by full parsing of
            //       the input.
            { TokenKind.AmpersandAmpersand, Precedence.LogicalAnd, (a, rbp, p) => p.Parse(rbp) is {} b && a && b },
            { TokenKind.PipePipe          , Precedence.LogicalOr , (a, b) => a || b },
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
             : throw new SyntaxErrorException($"Unexpected <{token.Kind}> token at offset {token.Offset}.");

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
            var si = 0;

            Token Token(TokenKind kind, int len)
            {
                resetState = true;
                return new Token(kind, si, len);
            }

            var state = State.Scan;
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
                        si = i;
                        switch (ch)
                        {
                            case ' ': case '\t': state = State.WhiteSpace; break;
                            case 't': state = State.IdentifierOrTrue; break;
                            case 'f': state = State.IdentifierOrFalse; break;
                            case '&': state = State.Ampersand; break;
                            case '|': state = State.Pipe; break;
                            case '!': state = State.Bang; break;
                            case '=': state = State.Equal; break;
                            case '(': yield return Token(TokenKind.LParen, 1); break;
                            case ')': yield return Token(TokenKind.RParen, 1); break;
                            case var c when char.IsLetter(c): state = State.Symbol; break;
                            default:
                                throw new SyntaxErrorException($"Unexpected at offset {i}: {ch}");
                        }
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
                            yield return Token(state == State.True ? TokenKind.True : TokenKind.False, i - si);
                            goto restart;
                        }
                    }
                    case State.Symbol:
                    {
                        if (char.IsLetterOrDigit(ch))
                            break;
                        yield return Token(TokenKind.Symbol, i - si);
                        goto restart;
                    }
                    case State.WhiteSpace:
                    {
                        if (ch == ' ' || ch == '\t')
                            break;
                        yield return Token(TokenKind.WhiteSpace, i - si);
                        goto restart;
                    }
                    case State.Ampersand:
                    {
                        if (ch != '&')
                            throw new SyntaxErrorException($"Unexpected at offset {i}: {ch}");
                        yield return Token(TokenKind.AmpersandAmpersand, 2);
                        break;
                    }
                    case State.Pipe:
                    {
                        if (ch != '|')
                            throw new SyntaxErrorException($"Unexpected at offset {i}: {ch}");
                        yield return Token(TokenKind.PipePipe, 2);
                        break;
                    }
                    case State.Equal:
                    {
                        if (ch != '=')
                            throw new SyntaxErrorException($"Unexpected at offset {i}: {ch}");
                        yield return Token(TokenKind.EqualEqual, 2);
                        break;
                    }
                    case State.Bang:
                    {
                        if (ch == '=')
                        {
                            yield return Token(TokenKind.BangEqual, 2);
                            break;
                        }
                        else
                        {
                            yield return Token(TokenKind.Bang, 1);
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
                case State.Scan             : goto eoi;
                case State.IdentifierOrTrue :
                case State.IdentifierOrFalse:
                case State.Symbol           : kind = TokenKind.Symbol    ; break;
                case State.WhiteSpace       : kind = TokenKind.WhiteSpace; break;
                case State.True             : kind = TokenKind.True      ; break;
                case State.False            : kind = TokenKind.False     ; break;
                case State.Bang             : kind = TokenKind.Bang      ; break;
                case State.Ampersand        :
                case State.Pipe             :
                case State.Equal            :
                    throw new SyntaxErrorException($"Unexpected end of input at offset {i}.");
                default:
                    throw new Exception("Internal error due to unhandled state: " + state);
            }

            yield return Token(kind, i - si);

            eoi:
            yield return new Token(TokenKind.Eoi, i, 0);
        }
    }
}
