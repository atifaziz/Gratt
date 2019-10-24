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
        /// <summary>
        /// Parses tokens into a result using top-down operator precedence. A
        /// default comparer is used to compare precedence for order and token
        /// kinds for equality.
        /// </summary>
        /// <typeparam name="TKind">The type of token kinds.</typeparam>
        /// <typeparam name="TToken">The type of tokens.</typeparam>
        /// <typeparam name="TPrecedence">The type of precedence.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="initialPrecedence">The initial precedence.</param>
        /// <param name="prefixSelector">
        /// A function that maps a token to a prefix parser function. If the
        /// given token is not a prefix then the function must fail by throwing
        /// an <see cref="Exception"/> or derived object. The prefix parser
        /// function receives the token and parser as arguments and returns
        /// the result of parsing.</param>
        /// <param name="infixSelector">
        /// A function that attempts to map a token to an infix parser
        /// function. The infix parser function receives the token, the left
        /// operand/result and parser as arguments and returns the result of
        /// parsing.</param>
        /// <param name="lexer">
        /// A sequence of token kind and token pairs produced by a lexer.</param>
        /// <returns>The result of parsing.</returns>
        /// <remarks>
        /// The last token kind and token pair yielded by the <see cref="lexer"/>
        /// must represent the end-of-input otherwise the behavior of this
        /// function, and by extension parsing, is undefined.
        /// </remarks>

        public static TResult
            Parse<TKind, TToken, TPrecedence, TResult>(
                TPrecedence initialPrecedence,
                Func<TKind, TToken, Func<TToken, Parser<Unit, TKind, TToken, TPrecedence, TResult>, TResult>> prefixSelector,
                Func<TKind, TToken, (TPrecedence, Func<TToken, TResult, Parser<Unit, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixSelector,
                IEnumerable<(TKind, TToken)> lexer) =>
            Parse(default(Unit), initialPrecedence,
                  (k, t, s) => prefixSelector(k, t), (k, t, s) => infixSelector(k, t), lexer);

        /// <summary>
        /// Parses tokens into a result using top-down operator precedence.
        /// Additional parameters specify how precedence compares in order and
        /// token kind in terms of equality.
        /// </summary>
        /// <typeparam name="TKind">The type of token kinds.</typeparam>
        /// <typeparam name="TToken">The type of tokens.</typeparam>
        /// <typeparam name="TPrecedence">The type of precedence.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="initialPrecedence">The initial precedence.</param>
        /// <param name="precedenceComparer">
        /// An <see cref="IComparer{T}"/> to use to compare one precedence
        /// with another for order.</param>
        /// <param name="kindEqualityComparer">
        /// An <see cref="IEqualityComparer{T}"/> to use to compare one token
        /// kind with another for equality.</param>
        /// <param name="prefixSelector">
        /// A function that maps a token to a prefix parser function. If the
        /// given token is not a prefix then the function must fail by throwing
        /// an <see cref="Exception"/> or derived object. The prefix parser
        /// function receives the token and parser as arguments and returns
        /// the result of parsing.</param>
        /// <param name="infixSelector">
        /// A function that attempts to map a token to an infix parser
        /// function. The infix parser function receives the token, the left
        /// operand/result and parser as arguments and returns the result of
        /// parsing.</param>
        /// <param name="lexer">
        /// A sequence of token kind and token pairs produced by a lexer.</param>
        /// <returns>The result of parsing.</returns>
        /// <remarks>
        /// The last token kind and token pair yielded by the <see cref="lexer"/>
        /// must represent the end-of-input otherwise the behavior of this
        /// function, and by extension parsing, is undefined.
        /// </remarks>

        public static TResult
            Parse<TKind, TToken, TPrecedence, TResult>(
                TPrecedence initialPrecedence, IComparer<TPrecedence> precedenceComparer,
                IEqualityComparer<TKind> kindEqualityComparer,
                Func<TKind, TToken, Func<TToken, Parser<Unit, TKind, TToken, TPrecedence, TResult>, TResult>> prefixSelector,
                Func<TKind, TToken, (TPrecedence, Func<TToken, TResult, Parser<Unit, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixSelector,
                IEnumerable<(TKind, TToken)> lexer) =>
            Parse(default(Unit), initialPrecedence, precedenceComparer, kindEqualityComparer,
                  (k, t, s) => prefixSelector(k, t), (k, t, s) => infixSelector(k, t), lexer);

        /// <summary>
        /// Parses tokens into a result using top-down operator precedence. A
        /// default comparer is used to compare precedence for order and token
        /// kinds for equality. An additional parameter specifies some user-
        /// defined state to associate with the parser object used during
        /// parsing.
        /// </summary>
        /// <typeparam name="TState">The type of user-defined state object.</typeparam>
        /// <typeparam name="TKind">The type of token kinds.</typeparam>
        /// <typeparam name="TToken">The type of tokens.</typeparam>
        /// <typeparam name="TPrecedence">The type of precedence.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="state">A user-defined state.</param>
        /// <param name="initialPrecedence">The initial precedence.</param>
        /// <param name="prefixSelector">
        /// A function that maps a token to a prefix parser function. If the
        /// given token is not a prefix then the function must fail by throwing
        /// an <see cref="Exception"/> or derived object. The prefix parser
        /// function receives the token and parser as arguments and returns
        /// the result of parsing.</param>
        /// <param name="infixSelector">
        /// A function that attempts to map a token to an infix parser
        /// function. The infix parser function receives the token, the left
        /// operand/result and parser as arguments and returns the result of
        /// parsing.</param>
        /// <param name="lexer">
        /// A sequence of token kind and token pairs produced by a lexer.</param>
        /// <returns>The result of parsing.</returns>
        /// <remarks>
        /// The last token kind and token pair yielded by the <see cref="lexer"/>
        /// must represent the end-of-input otherwise the behavior of this
        /// function, and by extension parsing, is undefined.
        /// </remarks>

        public static TResult
            Parse<TState, TKind, TToken, TPrecedence, TResult>(
                TState state,
                TPrecedence initialPrecedence,
                Func<TKind, TToken, TState, Func<TToken, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>> prefixSelector,
                Func<TKind, TToken, TState, (TPrecedence, Func<TToken, TResult, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixSelector,
                IEnumerable<(TKind, TToken)> lexer) =>
            Parse(state, initialPrecedence, Comparer<TPrecedence>.Default, EqualityComparer<TKind>.Default,
                  prefixSelector, infixSelector, lexer);

        /// <summary>
        /// Parses tokens into a result using top-down operator precedence.
        /// Additional parameters specify how precedence compares in order,
        /// how token kinds compare in terms of equality and some user-defined
        /// state to associate with the parser object used during parsing.
        /// </summary>
        /// <typeparam name="TState">The type of user-defined state object.</typeparam>
        /// <typeparam name="TKind">The type of token kinds.</typeparam>
        /// <typeparam name="TToken">The type of tokens.</typeparam>
        /// <typeparam name="TPrecedence">The type of precedence.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="state">A user-defined state.</param>
        /// <param name="initialPrecedence">The initial precedence.</param>
        /// <param name="precedenceComparer">
        /// An <see cref="IComparer{T}"/> to use to compare one precedence
        /// with another for order.</param>
        /// <param name="kindEqualityComparer">
        /// An <see cref="IEqualityComparer{T}"/> to use to compare one token
        /// kind with another for equality.</param>
        /// <param name="prefixSelector">
        /// A function that maps a token to a prefix parser function. If the
        /// given token is not a prefix then the function must fail by throwing
        /// an <see cref="Exception"/> or derived object. The prefix parser
        /// function receives the token and parser as arguments and returns
        /// the result of parsing.</param>
        /// <param name="infixSelector">
        /// A function that attempts to map a token to an infix parser
        /// function. The infix parser function receives the token, the left
        /// operand/result and parser as arguments and returns the result of
        /// parsing.</param>
        /// <param name="lexer">
        /// A sequence of token kind and token pairs produced by a lexer.</param>
        /// <returns>The result of parsing.</returns>
        /// <remarks>
        /// The last token kind and token pair yielded by the <see cref="lexer"/>
        /// must represent the end-of-input otherwise the behavior of this
        /// function, and by extension parsing, is undefined.
        /// </remarks>

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

    /// <summary>
    /// A top-down precedence order parser implementation.
    /// </summary>
    /// <typeparam name="TState">The type of user-defined state object.</typeparam>
    /// <typeparam name="TKind">The type of token kinds.</typeparam>
    /// <typeparam name="TToken">The type of tokens.</typeparam>
    /// <typeparam name="TPrecedence">The type of precedence.</typeparam>
    /// <typeparam name="TResult">The type of the result.</typeparam>

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

        /// <summary>
        /// Gets or sets some user-defined state associated with the parser.
        /// </summary>

        public TState State { get; set; }

        /// <summary>
        /// Parses the result.
        /// </summary>
        /// <param name="precedence">
        /// The precedence in terms of right-binding power.</param>
        /// <returns>The result of parsing.</returns>

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

        /// <summary>
        /// Peeks at the next token and reads it if it matches the given kind.
        /// </summary>
        /// <param name="kind">The kind of token to match and read.</param>
        /// <returns>
        /// A Boolean value that is <c>true</c> if the next token was matched
        /// and read; otherwise <c>false</c>.</returns>

        public bool Match(TKind kind)
        {
            var (pk, _) = Peek();
            if (!_tokenEqualityComparer.Equals(pk, kind))
                return false;
            Read();
            return true;
        }

        /// <summary>
        /// Reads the next token and ensures it matches an expected kind
        /// otherwise it throws a user-defined exception.
        /// </summary>
        /// <param name="kind">The kind of token expected.</param>
        /// <param name="errorSelector">
        /// A function that receives the expected token kind, the
        /// actual token kind and token pair read and returns the
        /// <see cref="Exception"/> to be thrown.</param>
        /// <returns>The token that was read.</returns>

        public TToken Read(TKind kind, Func<TKind, (TKind, TToken), Exception> errorSelector)
        {
            var peeked = Peek();
            var (pk, token) = peeked;
            if (!_tokenEqualityComparer.Equals(pk, kind))
                throw errorSelector(kind, peeked);
            Read();
            return token;
        }

        /// <summary>
        /// Peeks at the next token kind and token pair without consuming it.
        /// </summary>
        /// <returns>The token kind and token pair that was peeked.</returns>

        public (TKind, TToken) Peek()
        {
            var (kind, token) = Read();
            Unread(kind, token);
            return (kind, token);
        }

        /// <summary>
        /// Reads the next token kind and token pair.
        /// </summary>
        /// <returns>The token kind and token pair that was read.</returns>

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
