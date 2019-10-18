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

    static partial class Parser
    {
        public static TResult
            Parse<TKind, TToken, TPrecedence, TResult>(
                TPrecedence initialPrecedence,
                Func<TKind, Func<TToken, Parser<TKind, TToken, TPrecedence, TResult>, TResult>> prefixFunction,
                Func<TKind, (bool, TPrecedence, Func<TToken, TResult, Parser<TKind, TToken, TPrecedence, TResult>, TResult>)> infixFunction,
                IEnumerable<(TKind, TToken)> lexer) =>
            Parse(initialPrecedence, Comparer<TPrecedence>.Default, EqualityComparer<TKind>.Default, prefixFunction, infixFunction, lexer);

        public static TResult
            Parse<TKind, TToken, TPrecedence, TResult>(
                TPrecedence initialPrecedence, IComparer<TPrecedence> precedenceComparer,
                IEqualityComparer<TKind> kindEqualityComparer,
                Func<TKind, Func<TToken, Parser<TKind, TToken, TPrecedence, TResult>, TResult>> prefixFunction,
                Func<TKind, (bool, TPrecedence, Func<TToken, TResult, Parser<TKind, TToken, TPrecedence, TResult>, TResult>)> infixFunction,
                IEnumerable<(TKind, TToken)> lexer)
        {
            var parser =
                new Parser<TKind, TToken, TPrecedence, TResult>(precedenceComparer,
                                                                kindEqualityComparer,
                                                                prefixFunction, infixFunction,
                                                                lexer.GetEnumerator());
            return parser.Parse(initialPrecedence);
        }
    }

    partial class Parser<TKind, TToken, TPrecedence, TResult>
    {
        readonly IComparer<TPrecedence> _precedenceComparer;
        readonly IEqualityComparer<TKind> _tokenEqualityComparer;
        readonly Func<TKind, Func<TToken, Parser<TKind, TToken, TPrecedence, TResult>, TResult>> _prefixFunction;
        readonly Func<TKind, (bool, TPrecedence, Func<TToken, TResult, Parser<TKind, TToken, TPrecedence, TResult>, TResult>)> _infixFunction;
        (bool, TKind, TToken) _next;
        IEnumerator<(TKind, TToken)> _enumerator;

        internal Parser(IComparer<TPrecedence> precedenceComparer,
                        IEqualityComparer<TKind> tokenEqualityComparer,
                        Func<TKind, Func<TToken, Parser<TKind, TToken, TPrecedence, TResult>, TResult>> prefixFunction,
                        Func<TKind, (bool, TPrecedence, Func<TToken, TResult, Parser<TKind, TToken, TPrecedence, TResult>, TResult>)> infixFunction,
                        IEnumerator<(TKind, TToken)> lexer)
        {
            _precedenceComparer = precedenceComparer;
            _tokenEqualityComparer = tokenEqualityComparer;
            _prefixFunction = prefixFunction;
            _infixFunction = infixFunction;
            _enumerator = lexer;
        }

        public TResult Parse(TPrecedence precedence)
        {
            var read = Read();
            if (!read.HasValue)
                throw new Exception("Unexpected end of input.");
            var (kind, token) = read.Value;
            var prefix = _prefixFunction(kind);
            var left = prefix(token, this);

            var sought = Seek();

            while (sought.HasValue)
            {
                (kind, token) = sought.Value;
                switch (_infixFunction(kind))
                {
                    case var (someInfix, p, infix) when someInfix && _precedenceComparer.Compare(precedence, p) < 0:
                        Read();
                        left = infix(token, left, this);
                        sought = Seek();
                        break;
                    default:
                        return left;
                }
            }

            return left;
        }

        public bool Match(TKind kind)
        {
            switch (Seek())
            {
                case var (k, _) when _tokenEqualityComparer.Equals(k, kind): Consume(); return true;
                default: return false;
            }
        }

        public TToken Consume()
        {
            switch (Seek())
            {
                case var (_, token): Read(); return token;
                default: throw new InvalidOperationException();
            }
        }

        public void Consume(TKind kind, Func<TKind, bool, TKind, Exception> onError)
        {
            switch (Seek())
            {
                case null: throw onError(kind, false, default);
                case var (k, _) when _tokenEqualityComparer.Equals(k, kind): Consume(); break;
                case var (k, _): throw onError(kind, true, k);
            }
        }

        public (TKind, TToken)? Seek()
        {
            switch (Read())
            {
                case var (kind, token): Unread(kind, token); return (kind, token);
                default: return default;
            }
        }

        public (TKind, TToken)? Read()
        {
            switch (_next)
            {
                case (true, var kind, var token):
                    _next = default;
                    return (kind, token);
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
                            return (kind, token);
                    }
            }
        }

        void Unread(TKind kind, TToken token) => _next = _next switch
        {
            (true, _, _) => throw new InvalidOperationException(),
            _ => (true, kind, token)
        };
    }
}
