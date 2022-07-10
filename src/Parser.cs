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
    using System.Diagnostics.CodeAnalysis;
    using Unit = System.ValueTuple;

    static partial class Parser
    {
        /// <summary>
        /// Parses tokens into a result using top-down operator precedence. A default comparer is
        /// used to compare precedence for order and token kinds for equality.
        /// </summary>
        /// <typeparam name="TKind">The type of token kinds.</typeparam>
        /// <typeparam name="TToken">The type of tokens.</typeparam>
        /// <typeparam name="TPrecedence">The type of precedence.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="initialPrecedence">The initial precedence.</param>
        /// <param name="eoi">A token kind that marks the end of input.</param>
        /// <param name="eoiErrorSelector">
        /// A function that projects an <see cref="Exception"/> when the end-of-input token is not
        /// the the last token of the <paramref name="tokens"/> sequence.</param>
        /// <param name="prefixSelector">
        /// A function that maps a token to a prefix parser function. If the given token is not a
        /// prefix then the function must fail by throwing an <see cref="Exception"/> or derived
        /// object. The prefix parser function receives the token and parser as arguments and
        /// returns the result of parsing.</param>
        /// <param name="infixSelector">
        /// A function that attempts to map a token to an infix parser function. The infix parser
        /// function receives the token, the left operand/result and parser as arguments and returns
        /// the result of parsing.</param>
        /// <param name="tokens">
        /// A sequence of token kind and token pairs produced by a tokens.</param>
        /// <returns>The result of parsing.</returns>
        /// <remarks>
        /// The last token kind and token pair yielded by the <see cref="tokens"/> must represent
        /// the end-of-input otherwise the behavior of this function, and by extension parsing, is
        /// undefined.
        /// </remarks>

        public static TResult
            Parse<TKind, TToken, TPrecedence, TResult>(
                TPrecedence initialPrecedence,
                TKind eoi, Func<TToken, Exception> eoiErrorSelector,
                Func<TKind, TToken, Func<TToken, Parser<Unit, TKind, TToken, TPrecedence, TResult>, TResult>> prefixSelector,
                Func<TKind, TToken, (TPrecedence, Func<TToken, TResult, Parser<Unit, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixSelector,
                ILexer<(TKind, TToken)> tokens) =>
            Parse(default(Unit), initialPrecedence, eoi, eoiErrorSelector,
                  (k, t, _) => prefixSelector(k, t), (k, t, _) => infixSelector(k, t), tokens);

        /// <summary>
        /// Parses tokens into a result using top-down operator precedence. Additional parameters
        /// specify how precedence compares in order and token kind in terms of equality.
        /// </summary>
        /// <typeparam name="TKind">The type of token kinds.</typeparam>
        /// <typeparam name="TToken">The type of tokens.</typeparam>
        /// <typeparam name="TPrecedence">The type of precedence.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="initialPrecedence">The initial precedence.</param>
        /// <param name="precedenceComparer">
        /// An <see cref="IComparer{T}"/> to use to compare one precedence with another for
        /// order.</param>
        /// <param name="kindEqualityComparer">
        /// An <see cref="IEqualityComparer{T}"/> to use to compare one token kind with another for
        /// equality.</param>
        /// <param name="eoi">A token kind that marks the end of input.</param>
        /// <param name="eoiErrorSelector">
        /// A function that projects an <see cref="Exception"/> when the end-of-input token is not
        /// the the last token of the <paramref name="tokens"/> sequence.</param>
        /// <param name="prefixSelector">
        /// A function that maps a token to a prefix parser function. If the given token is not a
        /// prefix then the function must fail by throwing an <see cref="Exception"/> or derived
        /// object. The prefix parser function receives the token and parser as arguments and
        /// returns the result of parsing.</param>
        /// <param name="infixSelector">
        /// A function that attempts to map a token to an infix parser function. The infix parser
        /// function receives the token, the left operand/result and parser as arguments and returns
        /// the result of parsing.</param>
        /// <param name="tokens">
        /// A sequence of token kind and token pairs produced by a tokens.</param>
        /// <returns>The result of parsing.</returns>
        /// <remarks>
        /// The last token kind and token pair yielded by the <see cref="tokens"/> must represent
        /// the end-of-input otherwise the behavior of this function, and by extension parsing, is
        /// undefined.
        /// </remarks>

        public static TResult
            Parse<TKind, TToken, TPrecedence, TResult>(
                TPrecedence initialPrecedence, IComparer<TPrecedence> precedenceComparer,
                IEqualityComparer<TKind> kindEqualityComparer,
                TKind eoi, Func<TToken, Exception> eoiErrorSelector,
                Func<TKind, TToken, Func<TToken, Parser<Unit, TKind, TToken, TPrecedence, TResult>, TResult>> prefixSelector,
                Func<TKind, TToken, (TPrecedence, Func<TToken, TResult, Parser<Unit, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixSelector,
                ILexer<(TKind, TToken)> tokens) =>
            Parse(default(Unit), initialPrecedence, precedenceComparer, kindEqualityComparer,
                  eoi, eoiErrorSelector,
                  (k, t, _) => prefixSelector(k, t), (k, t, _) => infixSelector(k, t), tokens);

        /// <summary>
        /// Parses tokens into a result using top-down operator precedence. A default comparer is
        /// used to compare precedence for order and token kinds for equality. An additional
        /// parameter specifies some user- defined state to associate with the parser object used
        /// during parsing.
        /// </summary>
        /// <typeparam name="TState">The type of user-defined state object.</typeparam>
        /// <typeparam name="TKind">The type of token kinds.</typeparam>
        /// <typeparam name="TToken">The type of tokens.</typeparam>
        /// <typeparam name="TPrecedence">The type of precedence.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="state">A user-defined state.</param>
        /// <param name="initialPrecedence">The initial precedence.</param>
        /// <param name="eoi">A token kind that marks the end of input.</param>
        /// <param name="eoiErrorSelector">
        /// A function that projects an <see cref="Exception"/> when the end-of-input token is not
        /// the the last token of the <paramref name="tokens"/> sequence.</param>
        /// <param name="prefixSelector">
        /// A function that maps a token to a prefix parser function. If the given token is not a
        /// prefix then the function must fail by throwing an <see cref="Exception"/> or derived
        /// object. The prefix parser function receives the token and parser as arguments and
        /// returns the result of parsing.</param>
        /// <param name="infixSelector">
        /// A function that attempts to map a token to an infix parser function. The infix parser
        /// function receives the token, the left operand/result and parser as arguments and returns
        /// the result of parsing.</param>
        /// <param name="tokens">
        /// A sequence of token kind and token pairs produced by a tokens.</param>
        /// <returns>The result of parsing.</returns>
        /// <remarks>
        /// The last token kind and token pair yielded by the <see cref="tokens"/> must represent
        /// the end-of-input otherwise the behavior of this function, and by extension parsing, is
        /// undefined.
        /// </remarks>

        public static TResult
            Parse<TState, TKind, TToken, TPrecedence, TResult>(
                TState state,
                TPrecedence initialPrecedence,
                TKind eoi, Func<TToken, Exception> eoiErrorSelector,
                Func<TKind, TToken, TState, Func<TToken, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>> prefixSelector,
                Func<TKind, TToken, TState, (TPrecedence, Func<TToken, TResult, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixSelector,
                ILexer<(TKind, TToken)> tokens) =>
            Parse(state, initialPrecedence, Comparer<TPrecedence>.Default, EqualityComparer<TKind>.Default,
                  eoi, eoiErrorSelector,
                  prefixSelector, infixSelector, tokens);

        /// <summary>
        /// Parses tokens into a result using top-down operator precedence. Additional parameters
        /// specify how precedence compares in order, how token kinds compare in terms of equality
        /// and some user-defined state to associate with the parser object used during parsing.
        /// </summary>
        /// <typeparam name="TState">The type of user-defined state object.</typeparam>
        /// <typeparam name="TKind">The type of token kinds.</typeparam>
        /// <typeparam name="TToken">The type of tokens.</typeparam>
        /// <typeparam name="TPrecedence">The type of precedence.</typeparam>
        /// <typeparam name="TResult">The type of the result.</typeparam>
        /// <param name="state">A user-defined state.</param>
        /// <param name="initialPrecedence">The initial precedence.</param>
        /// <param name="precedenceComparer">
        /// An <see cref="IComparer{T}"/> to use to compare one precedence with another for
        /// order.</param>
        /// <param name="kindEqualityComparer">
        /// An <see cref="IEqualityComparer{T}"/> to use to compare one token kind with another for
        /// equality.</param>
        /// <param name="eoi">A token kind that marks the end of input.</param>
        /// <param name="eoiErrorSelector">
        /// A function that projects an <see cref="Exception"/> when the end-of-input token is not
        /// the the last token of the <paramref name="tokens"/> sequence.</param>
        /// <param name="prefixSelector">
        /// A function that maps a token to a prefix parser function. If the given token is not a
        /// prefix then the function must fail by throwing an <see cref="Exception"/> or derived
        /// object. The prefix parser function receives the token and parser as arguments and
        /// returns the result of parsing.</param>
        /// <param name="infixSelector">
        /// A function that attempts to map a token to an infix parser function. The infix parser
        /// function receives the token, the left operand/result and parser as arguments and returns
        /// the result of parsing.</param>
        /// <param name="tokens">
        /// A sequence of token kind and token pairs produced by a tokens.</param>
        /// <returns>The result of parsing.</returns>
        /// <remarks>
        /// The last token kind and token pair yielded by the <see cref="tokens"/> must represent
        /// the end-of-input otherwise the behavior of this function, and by extension parsing, is
        /// undefined.
        /// </remarks>

        public static TResult
            Parse<TState, TKind, TToken, TPrecedence, TResult>(
                TState state,
                TPrecedence initialPrecedence, IComparer<TPrecedence> precedenceComparer,
                IEqualityComparer<TKind> kindEqualityComparer,
                TKind eoi, Func<TToken, Exception> eoiErrorSelector,
                Func<TKind, TToken, TState, Func<TToken, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>> prefixSelector,
                Func<TKind, TToken, TState, (TPrecedence, Func<TToken, TResult, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixSelector,
                ILexer<(TKind, TToken)> tokens)
        {
            using var e = tokens.GetTokens();
            var parser =
                new Parser<TState, TKind, TToken, TPrecedence, TResult>(state,
                                                                        precedenceComparer,
                                                                        kindEqualityComparer,
                                                                        prefixSelector, infixSelector,
                                                                        e);
            var result = parser.Parse(initialPrecedence);
            parser.Read(eoi, (TKind _, (TKind, TToken Token) a) => eoiErrorSelector(a.Token));
            return result;
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
        ITokenStream<(TKind, TToken)> _lexer;

        internal Parser(TState state,
                        IComparer<TPrecedence> precedenceComparer,
                        IEqualityComparer<TKind> tokenEqualityComparer,
                        Func<TKind, TToken, TState, Func<TToken, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>> prefixSelector,
                        Func<TKind, TToken, TState, (TPrecedence, Func<TToken, TResult, Parser<TState, TKind, TToken, TPrecedence, TResult>, TResult>)?> infixSelector,
                        ITokenStream<(TKind, TToken)> lexer)
        {
            State = state;
            _precedenceComparer = precedenceComparer;
            _tokenEqualityComparer = tokenEqualityComparer;
            _prefixSelector = prefixSelector;
            _infixSelector = infixSelector;
            _lexer = lexer;
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
        /// A Boolean value that is <c>true</c> if the next token was matched and read; otherwise
        /// <c>false</c>.</returns>

        public bool Match(TKind kind)
        {
            var (pk, _) = Peek();
            if (!_tokenEqualityComparer.Equals(pk, kind))
                return false;
            Read();
            return true;
        }

        /// <summary>
        /// Reads the next token and ensures it matches an expected kind otherwise it throws a
        /// user-defined exception.
        /// </summary>
        /// <param name="kind">The kind of token expected.</param>
        /// <param name="errorSelector">
        /// A function that receives the expected token kind, the actual token kind and token pair
        /// read and returns the <see cref="Exception"/> to be thrown.</param>
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
            _lexer.Unread((kind, token));
            return (kind, token);
        }

        /// <summary>
        /// Reads the next token kind and token pair.
        /// </summary>
        /// <returns>The token kind and token pair that was read.</returns>
        /// <exception cref="InvalidOperationException">
        /// There are no more tokens to read and usually indicates an implementation fault in
        /// parsing.
        /// </exception>

        public (TKind, TToken) Read() =>
            _lexer.TryRead(out var result) ? result : throw new InvalidOperationException();

        /// <summary>
        /// Puts back a token kind and token pair to be returned by <see cref="Read"/>.
        /// </summary>
        /// <param name="kind">The kind of token.</param>
        /// <param name="token">The token.</param>
        public void Unread(TKind kind, TToken token) =>
            _lexer.Unread((kind, token));
    }

    partial interface ILexer<T>
    {
        ITokenStream<T> GetTokens();
    }

    sealed class Lexer<T, TBuffer> : ILexer<T>
    {
        readonly IEnumerable<T> _tokens;
        readonly TokenBuffer<TBuffer, T> _buffer;

        public Lexer(IEnumerable<T> tokens, TokenBuffer<TBuffer, T> buffer)
        {
            _tokens = tokens;
            _buffer = buffer;
        }

        public ITokenStream<T> GetTokens() => new TokenStream(_tokens.GetEnumerator(), _buffer);

        sealed class TokenStream : ITokenStream<T>
        {
            TBuffer _next;
            readonly TokenBuffer<TBuffer, T> _buffer;
            IEnumerator<T>? _enumerator;

            public TokenStream(IEnumerator<T> enumerator, TokenBuffer<TBuffer, T> buffer)
            {
                _enumerator = enumerator;
                _buffer = buffer;
                _next = _buffer.Init;
            }

            public void Dispose() => _enumerator?.Dispose();

            public bool TryRead([MaybeNullWhen(false)] out T result)
            {
                if (_enumerator is not { } enumerator)
                {
                    result = default;
                    return false;
                }

                if (_buffer.TryDequeue(ref _next, out result))
                    return true;

                if (!enumerator.MoveNext())
                {
                    _enumerator.Dispose();
                    _enumerator = null;
                    result = default;
                    return false;
                }

                result = enumerator.Current;
                return true;
            }

            public void Unread(T item)
            {
                if (_enumerator is null)
                    throw new InvalidOperationException();

                if (!_buffer.TryEnqueue(ref _next, item))
                    throw new InvalidOperationException();
            }
        }
    }

    abstract class TokenBuffer<TStore, T>
    {
        public abstract TStore Init { get; }
        public abstract bool TryEnqueue(ref TStore store, T item);
        public abstract bool TryDequeue(ref TStore store, [MaybeNullWhen(false)] out T item);
    }

    sealed class SingleTokenBuffer<T> : TokenBuffer<(bool, T), T>
    {
        public static readonly SingleTokenBuffer<T> Instance = new();

        public override (bool, T) Init => default;

        public override bool TryEnqueue(ref (bool, T) store, T item)
        {
            if (store is (true, _))
                return false;

            store = (true, item);
            return true;
        }

        public override bool TryDequeue(ref (bool, T) store, [MaybeNullWhen(false)] out T item)
        {
            switch (store)
            {
                case (false, _):
                    item = default;
                    return false;
                case (true, var some):
                    item = some;
                    store = Init;
                    return true;
            }
        }
    }

    enum CountOf2 { Zero, One, Two }

    record struct TwoTokens<T>(CountOf2 Count, T First, T Second);

    sealed class TwoTokenBuffer<T> : TokenBuffer<TwoTokens<T>, T>
    {
        public static readonly TwoTokenBuffer<T> Instance = new();

        public override TwoTokens<T> Init => default;

        public override bool TryEnqueue(ref TwoTokens<T> store, T item)
        {
            switch (store)
            {
                case (CountOf2.Zero, _, _):
                    store.Count = CountOf2.One;
                    store.First = item;
                    return true;
                case (CountOf2.One, var first, _):
                    store = new(CountOf2.Two, first, item);
                    return true;
                default:
                    return false;
            }
        }

        public override bool TryDequeue(ref TwoTokens<T> store, [MaybeNullWhen(false)] out T item)
        {
            switch (store)
            {
                case (CountOf2.One, var first, _):
                    item = first;
                    store = Init;
                    return true;
                case (CountOf2.Two, _, var second):
                    item = second;
                    store.Count = CountOf2.One;
                    store.Second = default!;
                    return true;
                default:
                    item = default;
                    return false;
            }
        }
    }

    partial interface ITokenStream<T> : IDisposable
    {
        bool TryRead([MaybeNullWhen(false)] out T result);
        void Unread(T item);
    }

    static partial class Extensions
    {
        public static ILexer<T> ToLexer<T>(this IEnumerable<T> tokens) => new Lexer<T, TwoTokens<T>>(tokens, TwoTokenBuffer<T>.Instance);

        public static bool TryPeek<T>(this ITokenStream<T> source, [MaybeNullWhen(false)] out T result)
        {
            if (!source.TryRead(out result))
                return false;
            source.Unread(result);
            return true;
        }
    }
}
