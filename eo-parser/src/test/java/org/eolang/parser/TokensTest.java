/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Tokens}.
 * @since 0.1
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnnecessaryLocalRule"})
final class TokensTest {

    @Test
    void readsIdentifierAndAdvancesCursor() {
        final Tokens tokens = new Tokens("foo", new Span("foo", 1));
        final Value value = tokens.readName();
        MatcherAssert.assertThat(
            "readName must return the parsed identifier and leave the cursor at end()",
            tokens.cursor(),
            Matchers.equalTo(value.end())
        );
    }

    @Test
    void readsIdentifierWithHyphen() {
        MatcherAssert.assertThat(
            "an identifier with a hyphen must be read as one NAME token per §2.3",
            new Tokens("foo-bar", new Span("foo-bar", 1)).readName().raw(),
            Matchers.equalTo("foo-bar")
        );
    }

    @Test
    void readsPositiveInt() {
        MatcherAssert.assertThat(
            "readInt must accept an unsigned digit run",
            new Tokens("42", new Span("42", 1)).readInt().raw(),
            Matchers.equalTo("42")
        );
    }

    @Test
    void readsNegativeInt() {
        MatcherAssert.assertThat(
            "readInt must accept a leading minus sign",
            new Tokens("-7", new Span("-7", 1)).readInt().raw(),
            Matchers.equalTo("-7")
        );
    }

    @Test
    void rejectsLeadingZero() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new Tokens("07", new Span("07", 1)).readInt(),
            "readInt must reject leading-zero literals per R-9.8.1"
        );
    }

    @Test
    void rejectsInvalidIntegerStart() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new Tokens("+abc", new Span("+abc", 1)).readInt(),
            "readInt must reject a sign that is not followed by digits"
        );
    }

    @Test
    void readsStarValue() {
        MatcherAssert.assertThat(
            "readValue must recognise the `*` star token",
            new Tokens("*", new Span("*", 1)).readValue().kind(),
            Matchers.equalTo(Value.Kind.STAR)
        );
    }

    @Test
    void readsSelfValue() {
        MatcherAssert.assertThat(
            "readValue must recognise the `%` self-reference token",
            new Tokens("% 5", new Span("% 5", 1)).readValue().kind(),
            Matchers.equalTo(Value.Kind.SELF)
        );
    }

    @Test
    void readsMethodChain() {
        final Tokens tokens = new Tokens("foo.bar.baz", new Span("foo.bar.baz", 1));
        tokens.readName();
        final List<MethodChain> chain = tokens.readChain();
        MatcherAssert.assertThat(
            "readChain must produce one MethodChain per .NAME segment after the head",
            chain,
            Matchers.hasSize(2)
        );
    }

    @Test
    void readsHorizontalArgs() {
        final Tokens tokens = new Tokens("foo a 42 b > x", new Span("foo a 42 b > x", 1));
        tokens.readName();
        final List<Value> args = tokens.readArgs();
        MatcherAssert.assertThat(
            "readArgs must collect each space-separated value until a suffix marker",
            args,
            Matchers.hasSize(3)
        );
    }

    @Test
    void readsChainOnHorizontalArg() {
        final Tokens tokens = new Tokens(
            "foo other.value.tail", new Span("foo other.value.tail", 1)
        );
        tokens.readName();
        final List<Value> args = tokens.readArgs();
        MatcherAssert.assertThat(
            "an arg of the form `head.m1.m2` must carry its full chain — 2 links here",
            args.get(0).chain(),
            Matchers.hasSize(2)
        );
    }

    @Test
    void consumesEntireChainOnArg() {
        final Tokens tokens = new Tokens(
            "foo bar.baz", new Span("foo bar.baz", 1)
        );
        tokens.readName();
        tokens.readArgs();
        MatcherAssert.assertThat(
            "after reading args, no `.method` tail must be left dangling in the body",
            tokens.tail(),
            Matchers.equalTo("")
        );
    }

    @Test
    void readsChainOnHorizontalBytesArg() {
        final Tokens tokens = new Tokens(
            "foo 00-01.as-i64", new Span("foo 00-01.as-i64", 1)
        );
        tokens.readName();
        final List<Value> args = tokens.readArgs();
        MatcherAssert.assertThat(
            "a BYTES arg of the form `head.m1` must carry its chain — 1 link here",
            args.get(0).chain(),
            Matchers.hasSize(1)
        );
    }

    @Test
    void consumesEntireChainOnBytesArg() {
        final Tokens tokens = new Tokens(
            "foo 00-01.as-i64", new Span("foo 00-01.as-i64", 1)
        );
        tokens.readName();
        tokens.readArgs();
        MatcherAssert.assertThat(
            "after reading a BYTES arg, its `.method` tail must not be left dangling",
            tokens.tail(),
            Matchers.equalTo("")
        );
    }

    @Test
    void stopsArgsBeforeSuffixMarker() {
        final Tokens tokens = new Tokens("foo a > x", new Span("foo a > x", 1));
        tokens.readName();
        tokens.readArgs();
        MatcherAssert.assertThat(
            "readArgs must leave the cursor at the space preceding the suffix so Suffix.parse can read it",
            tokens.tail(),
            Matchers.equalTo(" > x")
        );
    }

    @Test
    void reportsEndOfBody() {
        final Tokens tokens = new Tokens("foo", new Span("foo", 1));
        tokens.readName();
        MatcherAssert.assertThat(
            "atEnd must report true once every character has been consumed",
            tokens.atEnd(),
            Matchers.is(true)
        );
    }

    @Test
    void readsFloatLiteral() {
        MatcherAssert.assertThat(
            "a `42.5` source must produce a FLOAT value (digit-dot-digit triggers float path)",
            new Tokens("42.5", new Span("42.5", 1)).readNumber().kind(),
            Matchers.equalTo(Value.Kind.FLOAT)
        );
    }

    @Test
    void readsFloatWithExponent() {
        MatcherAssert.assertThat(
            "an exponent on a float must be absorbed into the FLOAT raw text",
            new Tokens("1.7e25", new Span("1.7e25", 1)).readNumber().raw(),
            Matchers.equalTo("1.7e25")
        );
    }

    @Test
    void readsNegativeFloat() {
        MatcherAssert.assertThat(
            "a signed FLOAT must round-trip the sign in raw()",
            new Tokens("-2.4E3", new Span("-2.4E3", 1)).readNumber().raw(),
            Matchers.equalTo("-2.4E3")
        );
    }

    @Test
    void leavesIntegerWhenDotFollowedByName() {
        MatcherAssert.assertThat(
            "`42.as-bytes` must read as INT followed by a chain, not as a FLOAT",
            new Tokens("42.as-bytes", new Span("42.as-bytes", 1)).readNumber().kind(),
            Matchers.equalTo(Value.Kind.INTEGER)
        );
    }

    @Test
    void readsStringLiteral() {
        MatcherAssert.assertThat(
            "a quoted string must read as STRING with raw() including the quotes",
            new Tokens("\"hello\"", new Span("\"hello\"", 1)).readString().raw(),
            Matchers.equalTo("\"hello\"")
        );
    }

    @Test
    void absorbsEscapesInString() {
        MatcherAssert.assertThat(
            "escape sequences must be absorbed into the STRING raw — emitter handles decoding",
            new Tokens("\"a\\nb\"", new Span("\"a\\nb\"", 1)).readString().raw(),
            Matchers.equalTo("\"a\\nb\"")
        );
    }

    @Test
    void rejectsUnterminatedString() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new Tokens("\"hello", new Span("\"hello", 1)).readString(),
            "a string without a closing quote must be rejected"
        );
    }

    @Test
    void rejectsStringSpanningLine() {
        final String body = "\"a".concat(String.valueOf((char) 10)).concat("b\"");
        Assertions.assertThrows(
            ParseError.class,
            () -> new Tokens(body, new Span(body, 1)).readString(),
            "a string containing a raw newline must be rejected per R-9.7.1"
        );
    }

    @Test
    void readsRootIdentifier() {
        MatcherAssert.assertThat(
            "`Q` must read as a ROOT value carrying the source char",
            new Tokens("Q", new Span("Q", 1)).readRoot().raw(),
            Matchers.equalTo("Q")
        );
    }

    @Test
    void readsPhiAsRoot() {
        MatcherAssert.assertThat(
            "`@` must read as a ROOT value (PHI)",
            new Tokens("@", new Span("@", 1)).readRoot().kind(),
            Matchers.equalTo(Value.Kind.ROOT)
        );
    }

    @Test
    void readsRhoAsRoot() {
        MatcherAssert.assertThat(
            "`^` must read as a ROOT value (RHO)",
            new Tokens("^", new Span("^", 1)).readRoot().raw(),
            Matchers.equalTo("^")
        );
    }

    @Test
    void readsXiAsRoot() {
        MatcherAssert.assertThat(
            "`$` must read as a ROOT value (XI)",
            new Tokens("$", new Span("$", 1)).readRoot().raw(),
            Matchers.equalTo("$")
        );
    }

    @Test
    void readsEmptyBytes() {
        MatcherAssert.assertThat(
            "`--` must read as BYTES with raw `--`",
            new Tokens("--", new Span("--", 1)).readBytes().raw(),
            Matchers.equalTo("--")
        );
    }

    @Test
    void readsSingleByteBytes() {
        MatcherAssert.assertThat(
            "`FF-` must read as a single-byte BYTES literal",
            new Tokens("FF-", new Span("FF-", 1)).readBytes().raw(),
            Matchers.equalTo("FF-")
        );
    }

    @Test
    void readsMultiByteBytes() {
        MatcherAssert.assertThat(
            "`CA-FE-BE-BE` must read as a multi-byte BYTES literal",
            new Tokens("CA-FE-BE-BE", new Span("CA-FE-BE-BE", 1)).readBytes().raw(),
            Matchers.equalTo("CA-FE-BE-BE")
        );
    }

    @Test
    void rejectsLowercaseHexAsBytesStart() {
        final String body = "c4-5";
        MatcherAssert.assertThat(
            "lowercase hex must not start a BYTES literal — `c4-5` is a NAME",
            new Tokens(body, new Span(body, 1)).readValue().kind(),
            Matchers.equalTo(Value.Kind.IDENTIFIER)
        );
    }

    @Test
    void readsLowercaseDashedIdentifierAsName() {
        final String body = "k7-9";
        MatcherAssert.assertThat(
            "`k7-9` must be parsed as a single NAME, not split at the dash",
            new Tokens(body, new Span(body, 1)).readValue().raw(),
            Matchers.equalTo(body)
        );
    }

    @Test
    void mapsAtSignAsMethodNameToPhi() {
        MatcherAssert.assertThat(
            "the `@` token used as a method name must surface as `φ` per R-3.5.2",
            new Tokens("@", new Span("foo.@", 1)).readMethodName().raw(),
            Matchers.equalTo("φ")
        );
    }

    @Test
    void mapsCaretAsMethodNameToRho() {
        MatcherAssert.assertThat(
            "the `^` token used as a method name must surface as `ρ` per R-3.5.2",
            new Tokens("^", new Span("foo.^", 1)).readMethodName().raw(),
            Matchers.equalTo("ρ")
        );
    }

    @Test
    void readsHexLiteralWithLowercase() {
        MatcherAssert.assertThat(
            "the `0x...` HEX literal must accept lowercase digits per R-9.8.3",
            new Tokens("0xabcd", new Span("0xabcd", 1)).readNumber().raw(),
            Matchers.equalTo("0xabcd")
        );
    }

    @Test
    void producesBytesKind() {
        MatcherAssert.assertThat(
            "readBytes must produce a BYTES-kinded Value",
            new Tokens("AA-BB", new Span("AA-BB", 1)).readBytes().kind(),
            Matchers.equalTo(Value.Kind.BYTES)
        );
    }

    @Test
    void readsHexLiteral() {
        MatcherAssert.assertThat(
            "a `0x` prefix must read as HEX with raw text preserving the prefix",
            new Tokens("0xFF", new Span("0xFF", 1)).readNumber().raw(),
            Matchers.equalTo("0xFF")
        );
    }

    @Test
    void readsHexCaseInsensitively() {
        MatcherAssert.assertThat(
            "HEX digits may be mixed case per R-9.8.3",
            new Tokens("0xaBcD", new Span("0xaBcD", 1)).readNumber().kind(),
            Matchers.equalTo(Value.Kind.HEX)
        );
    }

    @Test
    void rejectsHexWithNoDigits() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new Tokens("0x", new Span("0x", 1)).readHex(),
            "a `0x` with no following hex digits must be rejected"
        );
    }

    @Test
    void readsArgumentWithBinding() {
        final Tokens tokens = new Tokens("foo a:y > x", new Span("foo a:y > x", 1));
        tokens.readName();
        MatcherAssert.assertThat(
            "an arg followed by `:label` must record the binding tag on the Value",
            tokens.readArgs().get(0).binding(),
            Matchers.equalTo("y")
        );
    }

    @Test
    void readsArgumentWithNumericBinding() {
        final Tokens tokens = new Tokens("foo a:0 > x", new Span("foo a:0 > x", 1));
        tokens.readName();
        MatcherAssert.assertThat(
            "an arg followed by `:N` must record the digit string on the Value",
            tokens.readArgs().get(0).binding(),
            Matchers.equalTo("0")
        );
    }

    @Test
    void readsParenGroup() {
        MatcherAssert.assertThat(
            "readGroup must round-trip the bracketed text including parentheses",
            new Tokens("(foo bar)", new Span("(foo bar)", 1)).readGroup().raw(),
            Matchers.equalTo("(foo bar)")
        );
    }

    @Test
    void readsNestedParenGroup() {
        MatcherAssert.assertThat(
            "readGroup must handle nested `()` correctly",
            new Tokens("(foo (a b) c)", new Span("(foo (a b) c)", 1)).readGroup().raw(),
            Matchers.equalTo("(foo (a b) c)")
        );
    }

    @Test
    void rejectsUnterminatedParenGroup() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new Tokens("(foo bar", new Span("(foo bar", 1)).readGroup(),
            "a paren group missing its closing `)` must be rejected"
        );
    }

    @Test
    void rejectsRedundantParensAroundStringLiteralWithSpaces() {
        Assertions.assertThrows(
                ParseError.class,
                () -> new Tokens(
                        "(\"hello world\")", new Span("(\"hello world\")", 1)
                ).readGroup(),
                "a space inside a quoted string must not defeat the redundant-parens check"
        );
    }

    @Test
    void rejectsRedundantParensAroundStringLiteralWithParens() {
        Assertions.assertThrows(
                ParseError.class,
                () -> new Tokens(
                        "(\"a(b)c\")", new Span("(\"a(b)c\")", 1)
                ).readGroup(),
                "parens inside a quoted string must not defeat the redundant-parens check"
        );
    }

    @Test
    void rejectsRedundantParensAroundStringLiteralWithBrackets() {
        Assertions.assertThrows(
                ParseError.class,
                () -> new Tokens(
                        "(\"a[b]c\")", new Span("(\"a[b]c\")", 1)
                ).readGroup(),
                "brackets inside a quoted string must not defeat the redundant-parens check"
        );
    }

    @Test
    void rejectsRedundantParensAroundStringLiteralWithEscapedQuote() {
        final String body = "(\"a\\\"b\")";
        Assertions.assertThrows(
                ParseError.class,
                () -> new Tokens(body, new Span(body, 1)).readGroup(),
                "an escaped quote inside the string must not toggle quote state"
        );
    }

    @Test
    void acceptsParenGroupWithStringFollowedByAnotherArg() {
        final String body = "(\"hello\" b)";
        MatcherAssert.assertThat(
                "a real multi-token group containing a string arg must not be flagged",
                new Tokens(body, new Span(body, 1)).readGroup().raw(),
                Matchers.equalTo(body)
        );
    }

    @Test
    void detectsSuffixAhead() {
        final Tokens tokens = new Tokens("foo > x", new Span("foo > x", 1));
        tokens.readName();
        tokens.seek(tokens.cursor() + 1);
        MatcherAssert.assertThat(
            "suffixAhead must report true when the cursor sits on a `>` marker",
            tokens.suffixAhead(),
            Matchers.is(true)
        );
    }
}
