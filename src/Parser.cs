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

namespace Gratt
{
    using System;
    using System.Collections.Generic;
    using Unit = System.ValueTuple;

    static partial class Parser
    {
        public static TResult
            Parse<TKind, TToken, TPrecedence, TResult>(
                TPrecedence initialPrecedence,
                Func<TKind, TToken, Func<TToken, Parser<Unit, TKind, TToken, TPrecedence, TResult>, TResult>> prefixSelector,
                Func<TKind, TToken, (TPrecedence, Func<TToken, TResult, Parser<Unit, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixSelector,
                IEnumerable<(TKind, TToken)> lexer) =>
            Parse(default(Unit), initialPrecedence,
                  (k, t, s) => prefixSelector(k, t), (k, t, s) => infixSelector(k, t), lexer);

        public static TResult
            Parse<TKind, TToken, TPrecedence, TResult>(
                TPrecedence initialPrecedence, IComparer<TPrecedence> precedenceComparer,
                IEqualityComparer<TKind> kindEqualityComparer,
                Func<TKind, TToken, Func<TToken, Parser<Unit, TKind, TToken, TPrecedence, TResult>, TResult>> prefixSelector,
                Func<TKind, TToken, (TPrecedence, Func<TToken, TResult, Parser<Unit, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixSelector,
                IEnumerable<(TKind, TToken)> lexer) =>
            Parse(default(Unit), initialPrecedence, precedenceComparer, kindEqualityComparer,
                  (k, t, s) => prefixSelector(k, t), (k, t, s) => infixSelector(k, t), lexer);

        public static TResult
            Parse<TState, TKind, TToken, TPrecedence, TResult>(
                TState state,
                TPrecedence initialPrecedence,
                Func<TKind, TToken, TState, Func<TToken, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>> prefixSelector,
                Func<TKind, TToken, TState, (TPrecedence, Func<TToken, TResult, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixSelector,
                IEnumerable<(TKind, TToken)> lexer) =>
            Parse(state, initialPrecedence, Comparer<TPrecedence>.Default, EqualityComparer<TKind>.Default,
                  prefixSelector, infixSelector, lexer);

        public static TResult
            Parse<TState, TKind, TToken, TPrecedence, TResult>(
                TState state,
                TPrecedence initialPrecedence, IComparer<TPrecedence> precedenceComparer,
                IEqualityComparer<TKind> kindEqualityComparer,
                Func<TKind, TToken, TState, Func<TToken, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>> prefixSelector,
                Func<TKind, TToken, TState, (TPrecedence, Func<TToken, TResult, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixSelector,
                IEnumerable<(TKind, TToken)> lexer)
        {
            using var e = lexer.GetEnumerator();
            var parser =
                new Parser<TState, TKind, TToken, TPrecedence, TResult>(state,
                                                                        precedenceComparer,
                                                                        kindEqualityComparer,
                                                                        prefixSelector, infixSelector,
                                                                        e);
            return parser.Parse(initialPrecedence);
        }
    }

    partial class Parser<TState, TKind, TToken, TPrecedence, TResult>
    {
        readonly IComparer<TPrecedence> _precedenceComparer;
        readonly IEqualityComparer<TKind> _tokenEqualityComparer;
        readonly Func<TKind, TToken, TState, Func<TToken, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>> _prefixSelector;
        readonly Func<TKind, TToken, TState, (TPrecedence, Func<TToken, TResult, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>)?> _infixSelector;
        (bool, TKind, TToken) _next;
        IEnumerator<(TKind, TToken)> _enumerator;

        internal Parser(TState state,
                        IComparer<TPrecedence> precedenceComparer,
                        IEqualityComparer<TKind> tokenEqualityComparer,
                        Func<TKind, TToken, TState, Func<TToken, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>> prefixSelector,
                        Func<TKind, TToken, TState, (TPrecedence, Func<TToken, TResult, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixSelector,
                        IEnumerator<(TKind, TToken)> lexer)
        {
            State = state;
            _precedenceComparer = precedenceComparer;
            _tokenEqualityComparer = tokenEqualityComparer;
            _prefixSelector = prefixSelector;
            _infixSelector = infixSelector;
            _enumerator = lexer;
        }

        public TState State { get; set; }

        public TResult Parse(TPrecedence precedence)
        {
            var (kind, token) = Read();
            var prefix = _prefixSelector(kind, token, State);
            var left = prefix(token, this);

            while (true)
            {
                (kind, token) = Peek();
                switch (_infixSelector(kind, token, State))
                {
                    case var (p, infix) when _precedenceComparer.Compare(precedence, p) < 0:
                        Read();
                        left = infix(token, left, this);
                        break;
                    default:
                        return left;
                }
            }
        }

        public bool Match(TKind kind)
        {
            var (pk, _) = Peek();
            if (!_tokenEqualityComparer.Equals(pk, kind))
                return false;
            Read();
            return true;
        }

        public TToken Read(TKind kind, Func<TKind, (TKind, TToken), Exception> onError)
        {
            var peeked = Peek();
            var (pk, token) = peeked;
            if (!_tokenEqualityComparer.Equals(pk, kind))
                throw onError(kind, peeked);
            Read();
            return token;
        }

        public (TKind, TToken) Peek()
        {
            var (kind, token) = Read();
            Unread(kind, token);
            return (kind, token);
        }

        public (TKind, TToken) Read()
        {
            switch (_next)
            {
                case (true, var kind, var token):
                    _next = default;
                    return (kind, token);
                default:
                    switch (_enumerator)
                    {
                        case null:
                            throw new InvalidOperationException();
                        case var e:
                            if (!e.MoveNext())
                            {
                                _enumerator.Dispose();
                                _enumerator = null;
                                throw new InvalidOperationException();
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
