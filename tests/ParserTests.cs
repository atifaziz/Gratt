namespace Gratt.Tests
{
    using NUnit.Framework;

    public class ParserTests
    {
        enum TokenKind
        {
            Qux,
            Foo,
            Bar,
            Baz,
            Eoi
        }

        [Test]
        public void ReadAheadOne()
        {
            var result = Parse((TokenKind.Foo, "foo"), (TokenKind.Bar, "bar"),
                               (TokenKind.Eoi, string.Empty));

            Assert.That(result, Is.EqualTo("foobar"));
        }

        [Test]
        public void ReadAheadTwo()
        {
            var result = Parse((TokenKind.Foo, "foo"), (TokenKind.Bar, "bar"), (TokenKind.Baz, "baz"),
                               (TokenKind.Eoi, string.Empty));

            Assert.That(result, Is.EqualTo("foobarbaz"));
        }

        [Test]
        public void ReadAheadMore()
        {
            Assert.Throws<System.InvalidOperationException>(() =>
                Parse((TokenKind.Qux, "qux"), (TokenKind.Foo, "foo"),
                      (TokenKind.Bar, "bar"), (TokenKind.Baz, "baz"),
                      (TokenKind.Eoi, string.Empty))
                );
        }

        static string Parse(params (TokenKind Kind, string Token)[] tokens) =>
            Parser.Parse<TokenKind, string, int, string>(0, TokenKind.Eoi, _ => new(),
                                                         (_, _) =>
                                                             (token, parser)
                                                                 => parser.Peek() is (TokenKind.Foo, _)
                                                                    && parser.Peek(1) is (TokenKind.Bar, _)
                                                                    && parser.Peek(2) is (TokenKind.Baz, _) ? "quxfoobarbaz"
                                                                  : parser.Peek() is (TokenKind.Bar, _)
                                                                    && parser.Peek(1) is (TokenKind.Baz, _)
                                                                    && parser.Match(TokenKind.Bar)
                                                                    && parser.Match(TokenKind.Baz) ? "foobarbaz"
                                                                  : parser.Peek() is (TokenKind.Bar, _)
                                                                    && parser.Match(TokenKind.Bar) ? "foobar"
                                                                  : token,
                                                         (_, _) => null, tokens);
    }
}
