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

namespace PrattParsing
{
    using System;
    using System.Collections.Generic;

    partial interface ILexer<TKind, TToken>
    {
        (bool, TKind, TToken) Read();
        void Unread(TKind kind, TToken token);
    }

    static partial class LexerExtensions
    {
        public static bool TryRead<TKind, TToken>(this ILexer<TKind, TToken> lexer, out TKind kind, out TToken token)
        {
            bool read;
            (read, kind, token) = lexer.Read();
            return read;
        }

        public static bool TrySeek<TKind, TToken>(this ILexer<TKind, TToken> lexer, out TKind kind, out TToken token)
        {
            if (!lexer.TryRead(out kind, out token))
                return false;
            lexer.Unread(kind, token);
            return true;
        }

        public static (bool, TKind, TToken) Seek<TKind, TToken>(this ILexer<TKind, TToken> lexer)
        {
            var (read, kind, token) = lexer.Read();
            if (read)
                lexer.Unread(kind, token);
            return (read, kind, token);
        }
    }

    static partial class Lexer
    {
        /// <summary>
        /// Creates a lexer implementation on top of an
        /// <see cref="IEnumerator{T}"/> that supports a single unread between
        /// reads.
        /// </summary>

        public static ILexer<TKind, TToken> Create<TKind, TToken>(IEnumerator<(TKind, TToken)> enumerator) =>
            new SingletonLexer<TKind, TToken>(enumerator);

        sealed class SingletonLexer<TKind, TToken> : ILexer<TKind, TToken>
        {
            (bool, TKind, TToken) _next;
            IEnumerator<(TKind, TToken)> _enumerator;

            public SingletonLexer(IEnumerator<(TKind, TToken)> enumerator) =>
                _enumerator = enumerator;

            public (bool, TKind, TToken) Read()
            {
                switch (_next)
                {
                    case (true, var kind, var token):
                        _next = default;
                        return (true, kind, token);
                    default:
                        switch (_enumerator)
                        {
                            case null: return default;
                            case var e:
                                if (!e.MoveNext())
                                {
                                    _enumerator.Dispose();
                                    _enumerator = null;
                                    return default;
                                }
                                var (kind, token) = e.Current;
                                return (true, kind, token);
                        }
                }
            }

            public void Unread(TKind kind, TToken token) => _next = _next switch
            {
                (true, _, _) => throw new InvalidOperationException(),
                _ => (true, kind, token)
            };
        }
    }

    static partial class Parser
    {
        public static TResult
            Parse<TLexer, TKind, TToken, TPrecedence, TResult>(
                TPrecedence initialPrecedence,
                Func<TKind, Func<TToken, Parser<TLexer, TKind, TToken, TPrecedence, TResult>, TResult>> prefixFunction,
                Func<TKind, (bool, TPrecedence, Func<TToken, TResult, Parser<TLexer, TKind, TToken, TPrecedence, TResult>, TResult>)> infixFunction,
                TLexer lexer)
                where TLexer : ILexer<TKind, TToken> =>
            Parse(initialPrecedence, Comparer<TPrecedence>.Default, EqualityComparer<TKind>.Default, prefixFunction, infixFunction, lexer);

        public static TResult
            Parse<TLexer, TKind, TToken, TPrecedence, TResult>(
                TPrecedence initialPrecedence, IComparer<TPrecedence> precedenceComparer,
                IEqualityComparer<TKind> kindEqualityComparer,
                Func<TKind, Func<TToken, Parser<TLexer, TKind, TToken, TPrecedence, TResult>, TResult>> prefixFunction,
                Func<TKind, (bool, TPrecedence, Func<TToken, TResult, Parser<TLexer, TKind, TToken, TPrecedence, TResult>, TResult>)> infixFunction,
                TLexer lexer)
                where TLexer : ILexer<TKind, TToken> =>
            new Parser<TLexer, TKind, TToken, TPrecedence, TResult>(precedenceComparer, kindEqualityComparer, prefixFunction, infixFunction, lexer).Parse(initialPrecedence);
    }

    partial class Parser<TLexer, TKind, TToken, TPrecedence, TResult>
        where TLexer : ILexer<TKind, TToken>
    {
        readonly IComparer<TPrecedence> _precedenceComparer;
        readonly IEqualityComparer<TKind> _tokenEqualityComparer;
        readonly Func<TKind, Func<TToken, Parser<TLexer, TKind, TToken, TPrecedence, TResult>, TResult>> _prefixFunction;
        readonly Func<TKind, (bool, TPrecedence, Func<TToken, TResult, Parser<TLexer, TKind, TToken, TPrecedence, TResult>, TResult>)> _infixFunction;

        internal Parser(IComparer<TPrecedence> precedenceComparer,
                        IEqualityComparer<TKind> tokenEqualityComparer,
                        Func<TKind, Func<TToken, Parser<TLexer, TKind, TToken, TPrecedence, TResult>, TResult>> prefixFunction,
                        Func<TKind, (bool, TPrecedence, Func<TToken, TResult, Parser<TLexer, TKind, TToken, TPrecedence, TResult>, TResult>)> infixFunction,
                        TLexer lexer)
        {
            _precedenceComparer = precedenceComparer;
            _tokenEqualityComparer = tokenEqualityComparer;
            _prefixFunction = prefixFunction;
            _infixFunction = infixFunction;
            Lexer = lexer;
        }

        public TLexer Lexer { get; }

        public TResult Parse(TPrecedence precedence)
        {
            var (some, (kind, token)) = Read();
            if (!some)
                throw new Exception("Unexpected end of input.");
            var prefix = _prefixFunction(kind);
            var left = prefix(token, this);

            (some, (kind, token)) = Seek();

            while (some)
            {
                switch (_infixFunction(kind))
                {
                    case var (someInfix, p, infix) when someInfix && _precedenceComparer.Compare(precedence, p) < 0:
                        Read();
                        left = infix(token, left, this);
                        (some, (kind, token)) = Seek();
                        break;
                    default:
                        return left;
                }
            }

            return left;
        }

        public bool Match(TKind kind)
        {
            var (some, (k, _)) = Seek();
            if (!some || !_tokenEqualityComparer.Equals(k, kind))
                return false;
            Consume();
            return true;
        }

        public TToken Consume()
        {
            var (some, (_, token)) = Seek();
            if (!some)
                throw new InvalidOperationException();
            Read();
            return token;
        }

        public void Consume(TKind kind, Func<TKind, bool, TKind, TLexer, Exception> onError)
        {
            var (some, (k, _)) = Seek();
            if (!some)
                throw onError(kind, false, default, Lexer);
            if (!_tokenEqualityComparer.Equals(k, kind))
                throw onError(kind, true, k, Lexer);
            Consume();
        }

        (bool, (TKind, TToken)) Read()
        {
            var (read, kind, token) = Lexer.Read();
            return (read, (kind, token));
        }

        (bool, (TKind, TToken)) Seek()
        {
            var (sought, kind, token) = Lexer.Seek();
            return (sought, (kind, token));
        }
    }
}
