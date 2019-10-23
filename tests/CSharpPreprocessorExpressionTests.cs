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

namespace Gratt.Tests
{
    using System.Linq;
    using System.Text.RegularExpressions;
    using CSharp.Preprocessing;
    using NUnit.Framework;

    public class CSharpPreprocessorExpressionTests
    {
        [TestCase("false", null, false)]
        [TestCase("true", null, true)]
        [TestCase("!false", null, true)]
        [TestCase("!true", null, false)]
        [TestCase("!!false", null, false)]
        [TestCase("!!true", null, true)]
        [TestCase("true == true", null, true)]
        [TestCase("true == false", null, false)]
        [TestCase("true != true", null, false)]
        [TestCase("true != false", null, true)]
        [TestCase("true || false && true", null, true)]
        [TestCase("true && !false", null, true)]
        [TestCase("DEBUG", null, false)]
        [TestCase("DEBUG", "DEBUG", true)]
        [TestCase("DEBUG && FOO", "DEBUG", false)]
        [TestCase("DEBUG && FOO", "DEBUG FOO", true)]
        [TestCase("DEBUG || FOO", "DEBUG", true)]
        [TestCase("DEBUG || FOO", null, false)]
        [TestCase("DEBUG == FOO", null, true)]
        [TestCase("DEBUG == FOO", "FOO", false)]
        [TestCase("DEBUG == FOO", "DEBUG FOO", true)]
        [TestCase("(false)", null, false)]
        [TestCase("(true)", null, true)]
        [TestCase("!(false)", null, true)]
        [TestCase("!(true)", null, false)]
        [TestCase("True", null, false)]
        [TestCase("False", "False", true)]
        public void Evaluate(string expression, string symbols, bool expected) =>
            Test(expression, symbols, expected);

        [TestCase("", "Unexpected <Eoi> token at offset 0.")]
        [TestCase("&", "Unexpected end of input at offset 1.")]
        [TestCase("|", "Unexpected end of input at offset 1.")]
        [TestCase("=", "Unexpected end of input at offset 1.")]
        [TestCase("!", "Unexpected <Eoi> token at offset 1.")]
        [TestCase("(", "Unexpected <Eoi> token at offset 1.")]
        [TestCase(")", "Unexpected <RParen> token at offset 0.")]
        [TestCase("()", "Unexpected <RParen> token at offset 1.")]
        [TestCase("&&", "Unexpected <AmpersandAmpersand> token at offset 0.")]
        [TestCase("||", "Unexpected <PipePipe> token at offset 0.")]
        [TestCase("==", "Unexpected <EqualEqual> token at offset 0.")]
        [TestCase("!=", "Unexpected <BangEqual> token at offset 0.")]
        [TestCase("&*", "Unexpected at offset 1: *")]
        [TestCase("|*", "Unexpected at offset 1: *")]
        [TestCase("=*", "Unexpected at offset 1: *")]
        [TestCase("!*", "Unexpected at offset 1: *")]
        public void SyntaxError(string expression, string message)
        {
            var e =
                Assert.Throws<SyntaxErrorException>(() =>
                    PreprocessorExpression.Evaluate(expression, _ => false));
            Assert.That(e.Message, Is.EqualTo(message));
        }

        static void Test(string expression, string symbolsString, bool expected)
        {
            var symbols =
                 from s in Regex.Split(symbolsString ?? string.Empty, @"[\s,;|+]")
                 select s.Trim() into s
                 where s.Length > 0
                 select s;

            var symbolSet = symbols.ToHashSet();

            var result = PreprocessorExpression.Evaluate(expression, symbolSet.Contains);

            Assert.That(result, Is.EqualTo(expected));
        }
    }
}
