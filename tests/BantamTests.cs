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

// This is a C# version of Bantam tests found at:
// https://github.com/munificent/bantam/tree/05276a6f24657630b86a3edeae0a81233512c16d/src/com/stuffwithstuff/bantam

namespace Gratt.Tests
{
    using System.Text;
    using Bantam;
    using NUnit.Framework;

    public class BantamTests
    {
        [TestCase("a()", "a()")]
        [TestCase("a(b)", "a(b)")]
        [TestCase("a(b, c)", "a(b, c)")]
        [TestCase("a(b)(c)", "a(b)(c)")]
        [TestCase("a(b) + c(d)", "(a(b) + c(d))")]
        [TestCase("a(b ? c : d, e + f)", "a((b ? c : d), (e + f))")]
        public void FunctionCall(string source, string expected) =>
            Test(source, expected);

        [TestCase("~!-+a", "(~(!(-(+a))))")]
        [TestCase("a!!!", "(((a!)!)!)")]
        public void UnaryPrecedence(string source, string expected) =>
            Test(source, expected);

        [TestCase("a = b + c * d ^ e - f / g", "(a = ((b + (c * (d ^ e))) - (f / g)))")]
        public void BinaryPrecedence(string source, string expected) =>
            Test(source, expected);

        [TestCase("a = b = c", "(a = (b = c))")]
        [TestCase("a + b - c", "((a + b) - c)")]
        [TestCase("a * b / c", "((a * b) / c)")]
        [TestCase("a ^ b ^ c", "(a ^ (b ^ c))")]
        public void BinaryAssociativity(string source, string expected) =>
            Test(source, expected);

        [TestCase("a ? b : c ? d : e", "(a ? b : (c ? d : e))")]
        [TestCase("a ? b ? c : d : e", "(a ? (b ? c : d) : e)")]
        [TestCase("a + b ? c * d : e / f", "((a + b) ? (c * d) : (e / f))")]
        public void ConditionalOperator(string source, string expected) =>
            Test(source, expected);

        [TestCase("a + (b + c) + d", "((a + (b + c)) + d)")]
        [TestCase("a ^ (b + c)", "(a ^ (b + c))")]
        [TestCase("(!a)!",    "((!a)!)")]
        public void Grouping(string source, string expected) =>
            Test(source, expected);

        static void Test(string source, string expected)
        {
            var expression = BantamParser.Parse(source);
            var sb = new StringBuilder();
            expression.Print(sb);
            var actual = sb.ToString();
            Assert.That(actual, Is.EqualTo(expected));
        }
    }
}
