#region Copyright (c) 2011 Robert Nystrom
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//
#endregion

// This is a C# version of Bantam (for testing purposes only) found at:
// https://github.com/munificent/bantam/tree/05276a6f24657630b86a3edeae0a81233512c16d/src/com/stuffwithstuff/bantam

namespace Bantam
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Collections.Immutable;
    using System.Linq;
    using System.Text;
    using Gratt;
    using static TokenType;
    using PrefixParselet = System.Func<Token, Gratt.Parser<System.ValueTuple, TokenType, Token, int, Expression>, Expression>;
    using InfixParselet = System.Func<Token, Expression, Gratt.Parser<System.ValueTuple, TokenType, Token, int, Expression>, Expression>;

    /// <summary>
    /// Defines the different precedence levels used by the infix parsers. These
    /// determine how a series of infix expressions will be grouped. For example,
    /// "a + b * c - d" will be parsed as "(a + (b * c)) - d" because "*" has higher
    /// precedence than "+" and "-". Here, bigger numbers mean higher precedence.
    /// </summary>

    static class Precedence
    {
        // Ordered in increasing precedence.
        public const int Assignment  = 1;
        public const int Conditional = 2;
        public const int Sum         = 3;
        public const int Product     = 4;
        public const int Exponent    = 5;
        public const int Prefix      = 6;
        public const int Postfix     = 7;
        public const int Call        = 8;
    }

    enum TokenType
    {
        LeftParen,
        RightParen,
        Comma,
        Assign,
        Plus,
        Minus,
        Asterisk,
        Slash,
        Caret,
        Tilde,
        Bang,
        Question,
        Colon,
        Name,
        Eof
    }

    static class TokenTypeExtensions
    {
        public static char Punctuator(this TokenType type) => type switch
        {
            LeftParen  => '(',
            RightParen => ')',
            Comma      => ',',
            Assign     => '=',
            Plus       => '+',
            Minus      => '-',
            Asterisk   => '*',
            Slash      => '/',
            Caret      => '^',
            Tilde      => '~',
            Bang       => '!',
            Question   => '?',
            Colon      => ':',
            _          => throw new ArgumentOutOfRangeException(nameof(type))
        };
    }

    sealed class Token
    {
        public TokenType Type { get; }
        public string Text { get; }

        public Token(TokenType type, string text) =>
            (Type, Text) = (type, text);

        public override string ToString() => Text;
    }

    /// <summary>
    /// A very primitive lexer. Takes a string and splits it into a series of
    /// Tokens. Operators and punctuation are mapped to unique keywords. Names,
    /// which can be any series of letters, are turned into NAME tokens. All other
    /// characters are ignored (except to separate names). Numbers and strings are
    /// not supported. This is really just the bare minimum to give the parser
    /// something to work with.
    /// </summary>

    static class Lexer
    {
        public static IEnumerable<Token> Lex(string text)
        {
            var mText = text;
            var mIndex = 0;

            while (mIndex < mText.Length)
            {
                var c = mText[mIndex++];

                var punctuator = c switch
                {
                    '(' => LeftParen,
                    ')' => RightParen,
                    ',' => Comma,
                    '=' => Assign,
                    '+' => Plus,
                    '-' => Minus,
                    '*' => Asterisk,
                    '/' => Slash,
                    '^' => Caret,
                    '~' => Tilde,
                    '!' => Bang,
                    '?' => Question,
                    ':' => Colon,
                    _ => (TokenType?)null
                };

                if (punctuator is TokenType t)
                {
                    // Handle punctuation.
                    yield return new Token(t, c.ToString());
                }
                else if (char.IsLetter(c))
                {
                    // Handle names.
                    var start = mIndex - 1;
                    while (mIndex < mText.Length)
                    {
                        if (!char.IsLetter(mText[mIndex]))
                            break;
                        mIndex++;
                    }

                    var name = mText.Substring(start, mIndex - start);
                    yield return new Token(TokenType.Name, name);
                }
                /*
                else
                {
                    // Ignore all other characters (whitespace, etc.)
                }
                */
            }

            // Once we've reached the end of the string, just return EOF tokens. We'll
            // just keeping returning them as many times as we're asked so that the
            // parser's lookahead doesn't have to worry about running out of tokens.

            yield return new Token(TokenType.Eof, string.Empty);
        }
    }

    /// <summary>
    /// Base for all expression AST node classes.
    /// </summary>

    abstract class Expression
    {
        /// <summary>
        /// Pretty-print the expression to a string.
        /// </summary>

        public abstract void Print(StringBuilder builder);
    }

    /// <summary>
    /// An assignment expression like "a = b".
    /// </summary>

    class AssignExpression : Expression
    {
        public string Name { get; }
        public Expression Right { get; }

        public AssignExpression(string name, Expression right) =>
            (Name, Right) = (name, right);

        public override void Print(StringBuilder builder)
        {
            builder.Append("(").Append(Name).Append(" = ");
            Right.Print(builder);
            builder.Append(")");
        }
    }

    /// <summary>
    /// A function call like "a(b, c, d)".
    /// </summary>

    class CallExpression : Expression
    {
        public Expression Function { get; }
        public ImmutableArray<Expression> Args { get; }

        public CallExpression(Expression function, ImmutableArray<Expression> args) =>
            (Function, Args) = (function, args);

        public override void Print(StringBuilder builder)
        {
            Function.Print(builder);
            builder.Append("(");
            var i = 0;
            foreach (var arg in Args)
            {
                if (i > 0)
                    builder.Append(", ");
                arg.Print(builder);
                i++;
            }
            builder.Append(")");
        }
    }

    /// <summary>
    /// A ternary conditional expression like "a ? b : c".
    /// </summary>

    class ConditionalExpression : Expression
    {
        public Expression Condition { get; }
        public Expression ThenArm   { get; }
        public Expression ElseArm   { get; }

        public ConditionalExpression(Expression condition, Expression thenArm, Expression elseArm) =>
            (Condition, ThenArm, ElseArm) = (condition, thenArm, elseArm);

        public override void Print(StringBuilder builder)
        {
            builder.Append("(");
            Condition.Print(builder);
            builder.Append(" ? ");
            ThenArm.Print(builder);
            builder.Append(" : ");
            ElseArm.Print(builder);
            builder.Append(")");
        }
    }

    /// <summary>
    /// A simple variable name expression like "abc".
    /// </summary>

    class NameExpression : Expression
    {
        public string Name { get; }
        public NameExpression(string name) => Name = name;
        public override void Print(StringBuilder builder) => builder.Append(Name);
    }

    /// <summary>
    /// A binary arithmetic expression like "a + b" or "c ^ d".
    /// </summary>

    class OperatorExpression : Expression
    {
        public Expression Left     { get; }
        public TokenType  Operator { get; }
        public Expression Right    { get; }

        public OperatorExpression(Expression left, TokenType @operator, Expression right) =>
            (Left, Operator, Right) = (left, @operator, right);

        public override void Print(StringBuilder builder)
        {
            builder.Append("(");
            Left.Print(builder);
            builder.Append(" ").Append(Operator.Punctuator()).Append(" ");
            Right.Print(builder);
            builder.Append(")");
        }
    }

    /// <summary>
    /// A postfix unary arithmetic expression like "a!".
    /// </summary>

    class PostfixExpression : Expression
    {
        public Expression Left { get; }
        public TokenType Operator { get; }

        public PostfixExpression(Expression left, TokenType @operator) =>
            (Left, Operator) = (left, @operator);

        public override void Print(StringBuilder builder)
        {
            builder.Append("(");
            Left.Print(builder);
            builder.Append(Operator.Punctuator()).Append(")");
        }
    }

    /// <summary>
    /// A prefix unary arithmetic expression like "!a" or "-b".
    /// </summary>

    class PrefixExpression : Expression
    {
        public Expression Right { get; }
        public TokenType Operator { get; }

        public PrefixExpression(TokenType @operator, Expression right) =>
            (Right, Operator) = (right, @operator);

        public override void Print(StringBuilder builder)
        {
            builder.Append("(").Append(Operator.Punctuator());
            Right.Print(builder);
            builder.Append(")");
        }
    }

    static class Parselets
    {
        /// <summary>
        /// Parses assignment expressions like "a = b". The left side of an assignment
        /// expression must be a simple name like "a", and expressions are
        /// right-associative. (In other words, "a = b = c" is parsed as "a = (b = c)").
        /// </summary>

        public static readonly InfixParselet Assign =
            (token, left, parser) =>
                left is NameExpression nameExpression
                ? new AssignExpression(nameExpression.Name, parser.Parse(Precedence.Assignment - 1))
                : throw new ParseException("The left-hand side of an assignment must be a name.");

        /// <summary>
        /// Generic infix parselet for a binary arithmetic operator. The only
        /// difference when parsing, "+", "-", "*", "/", and "^" is precedence and
        /// associativity, so we can use a single parselet class for all of those.
        /// </summary>

        public static InfixParselet BinaryOperator(int precedence, bool isRight) =>
            (token, left, parser) =>
                new OperatorExpression(left, token.Type, parser.Parse(precedence - (isRight ? 1 : 0)));

        /// <summary>
        /// Parselet to parse a function call like "a(b, c, d)".
        /// </summary>

        public static readonly InfixParselet Call =
            (token, left, parser) =>
            {
                // Parse the comma-separated arguments until we hit, ")".
                var args = ImmutableArray.CreateBuilder<Expression>();

                // There may be no arguments at all.
                if (!parser.Match(RightParen))
                {
                    do { args.Add(parser.Parse(0)); } while (parser.Match(Comma));
                    parser.Read(RightParen, delegate { throw new ParseException("Expected ')'."); });
                }

                return new CallExpression(left, args.ToImmutable());
            };

        /// <summary>
        /// Parselet for the condition or "ternary" operator, like "a ? b : c".
        /// </summary>

        public static readonly InfixParselet Conditional =
            (token, left, parser) =>
            {
                var thenArm = parser.Parse(0);
                parser.Read(Colon, delegate { throw new ParseException("Expected ':'."); });
                var elseArm = parser.Parse(Precedence.Conditional - 1);
                return new ConditionalExpression(left, thenArm, elseArm);
            };

        /// <summary>
        /// Parses parentheses used to group an expression, like "a * (b + c)".
        /// </summary>

        public static readonly PrefixParselet Group =
            (token, parser) =>
            {
                var expression = parser.Parse(0);
                parser.Read(RightParen, delegate { throw new ParseException("Expected ')'."); });
                return expression;
            };

        /// <summary>
        /// Simple parselet for a named variable like "abc".
        /// </summary>

        public static readonly PrefixParselet Name =
            (token, parser) => new NameExpression(token.Text);

        /// <summary>
        /// Generic infix parselet for an unary arithmetic operator. Parses postfix
        /// unary "?" expressions.
        /// </summary>

        public static readonly InfixParselet PostfixOperator =
            (token, left, parser) =>
                new PostfixExpression(left, token.Type);

        /// <summary>
        /// Generic prefix parselet for an unary arithmetic operator. Parses prefix
        /// unary "-", "+", "~", and "!" expressions.
        /// </summary>

        public static PrefixParselet PrefixOperator(int precedence) =>
            // To handle right-associative operators like "^", we allow a slightly
            // lower precedence when parsing the right-hand side. This will let a
            // parselet with the same precedence appear on the right, which will then
            // take *this* parselet's result as its left-hand argument.
            (token, parser) => new PrefixExpression(token.Type, parser.Parse(precedence));
    }

    [Serializable]
    class ParseException : Exception
    {
        public ParseException() {}
        public ParseException(string message) : base(message) {}
        public ParseException(string message, Exception inner) : base(message, inner) {}
    }

    static class BantamParser
    {
        public static Expression Parse(string source) =>
            Parser.Parse(0, Eof, t => new Exception(),
                (type, _) => Spec.Instance.Prefix(type),
                (type, _) => Spec.Instance.Infix(type),
                from t in Lexer.Lex(source)
                select (t.Type, t));

        sealed class Spec : IEnumerable
        {
            public static readonly Spec Instance = new Spec
            {
                // Register all of the parselets for the grammar.

                // Register the ones that need special parselets.

                { Name     , Parselets.Name },
                { Assign   , Precedence.Assignment, Parselets.Assign },
                { Question , Precedence.Conditional, Parselets.Conditional },
                { LeftParen, Parselets.Group },
                { LeftParen, Precedence.Call, Parselets.Call },

                // Register the simple operator parselets.

                { Plus , Parselets.PrefixOperator(Precedence.Prefix) },
                { Minus, Parselets.PrefixOperator(Precedence.Prefix) },
                { Tilde, Parselets.PrefixOperator(Precedence.Prefix) },
                { Bang , Parselets.PrefixOperator(Precedence.Prefix) },

                // For kicks, we'll make "!" both prefix and postfix, kind of like ++.
                { Bang, Precedence.Postfix, Parselets.PostfixOperator },

                { Plus,     Precedence.Sum     , Parselets.BinaryOperator(Precedence.Sum     , isRight: false) },
                { Minus,    Precedence.Sum     , Parselets.BinaryOperator(Precedence.Sum     , isRight: false) },
                { Asterisk, Precedence.Product , Parselets.BinaryOperator(Precedence.Product , isRight: false) },
                { Slash,    Precedence.Product , Parselets.BinaryOperator(Precedence.Product , isRight: false) },
                { Caret,    Precedence.Exponent, Parselets.BinaryOperator(Precedence.Exponent, isRight: true ) },
            };

            readonly Dictionary<TokenType, PrefixParselet> _prefixes = new Dictionary<TokenType, PrefixParselet>();
            readonly Dictionary<TokenType, (int, InfixParselet)> _infixes = new Dictionary<TokenType, (int, InfixParselet)>();

            Spec() {}

            void Add(TokenType type, PrefixParselet prefix) =>
                _prefixes.Add(type, prefix);

            void Add(TokenType type, int precedence, InfixParselet prefix) =>
                _infixes.Add(type, (precedence, prefix));

            public PrefixParselet Prefix(TokenType type) => _prefixes[type];

            public (int, InfixParselet)? Infix(TokenType type)
            {
                if (!_infixes.TryGetValue(type, out var v))
                    return default;
                var (precedence, infix) = v;
                return (precedence, infix);
            }

            IEnumerator IEnumerable.GetEnumerator() =>
                _prefixes.Cast<object>().Concat(_infixes.Cast<object>()).GetEnumerator();
        }
    }
}
