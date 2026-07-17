/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link LnApplication}.
 *
 * <p>First iteration: bare-identifier heads with optional name
 * suffixes. Chains, horizontal args, paren groups, star tuples, and
 * data literals are tested as their support lands in subsequent
 * iterations. *
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.TooManyMethods")
final class LnApplicationTest {

    @Test
    void pushesHeadKindForBareIdentifier() {
        final Stack stack = new Stack();
        new LnApplication(new Span("foo", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a bare identifier must push a HEAD level on the stack",
            stack.top().kind(),
            Matchers.equalTo(Kind.HEAD)
        );
    }

    @Test
    void leavesHeadOpenForDeeperChildren() {
        final Stack stack = new Stack();
        new LnApplication(new Span("foo > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a head line must remain OPEN so deeper-indent children can promote it to vapplication",
            stack.top().openness(),
            Matchers.equalTo(Openness.OPEN)
        );
    }

    @Test
    void marksLevelNamedWhenSuffixPresent() {
        final Stack stack = new Stack();
        new LnApplication(new Span("foo > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "the pushed level must reflect the suffix's named flag",
            stack.top().named(),
            Matchers.is(true)
        );
    }

    @Test
    void leavesLevelUnnamedWithoutSuffix() {
        final Stack stack = new Stack();
        new LnApplication(new Span("foo", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a head line without a suffix cannot mark the level as named",
            stack.top().named(),
            Matchers.is(false)
        );
    }

    @Test
    void emitsObjectWithBase() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a named head must emit <o name='x' base='foo' line='1' pos='0'>",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='x' and @base='foo' and @line='1' and @pos='0']"
            )
        );
    }

    @Test
    void emitsConstWhenBangPresent() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo > x!", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a `> x!` suffix must attach @const='' to the emitted <o>",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@base='foo' and @const='']")
        );
    }

    @Test
    void emitsAutoNameForDoubleArrow() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo >>", 7))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a `>>` head suffix must emit the cactus auto-name format",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[starts-with(@name,'a🌵') and @base='foo']")
        );
    }

    @Test
    void omitsNameAttributeForBareIdentifier() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a suffix-less head cannot carry a @name attribute on its <o>",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@base='foo' and not(@name)]")
        );
    }

    @Test
    void marksFirstObjectEmitted() {
        final Globals globals = new Globals();
        new LnApplication(new Span("foo > x", 1))
            .into(new Stack(), globals, new Emit());
        MatcherAssert.assertThat(
            "a head line must flip firstObjectEmitted so later metas are rejected",
            globals.firstObjectEmitted(),
            Matchers.is(true)
        );
    }

    @Test
    void pushesHmethodKindForChainedHead() {
        final Stack stack = new Stack();
        new LnApplication(new Span("foo.bar > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a head with `.method` continuation must push HMETHOD instead of HEAD",
            stack.top().kind(),
            Matchers.equalTo(Kind.HMETHOD)
        );
    }

    @Test
    void emitsHeadAsFlatSiblingBeforeChainLink() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo.bar > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "the head of a chain must appear as a flat sibling, not nested under the chain link",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[1][@base='foo' and not(@method)]")
        );
    }

    @Test
    void emitsLastChainLinkWithName() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo.bar > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "the line's name suffix must attach to the last chain link, not the head",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[2][@name='x' and @base='.bar' and @method='']"
            )
        );
    }

    @Test
    void recordsDotColumnForChainLinkPos() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo.bar > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "the chain link's @pos must point at the dot column per R-9.1.3, not at the method name",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@base='.bar' and @pos='3']")
        );
    }

    @Test
    void recordsDotColumnForFragileChainLinkPos() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo?.bar > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a fragile `?.` chain link's @pos must point at the `.`, not at the `?`,"
                .concat(" matching the standalone-continuation path in LnMethod"),
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@base='.bar' and @pos='4']")
        );
    }

    @Test
    void emitsTwoLevelChainAsThreeSiblings() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo.bar.baz > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "`foo.bar.baz` must emit three flat siblings — head plus two method links",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object[count(o)=3]")
        );
    }

    @Test
    void carriesNoNameOnIntermediateChainLink() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo.bar.baz > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "only the last link of a chain may carry the line's name suffix",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@base='.bar' and @method='' and not(@name)]"
            )
        );
    }

    @Test
    void pushesHapplicationKindForHorizontalArgs() {
        final Stack stack = new Stack();
        new LnApplication(new Span("foo a b > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a head with horizontal args must push HAPPLICATION, not HEAD/HMETHOD",
            stack.top().kind(),
            Matchers.equalTo(Kind.HAPPLICATION)
        );
    }

    @Test
    void marksHapplicationAsHorizontallyCompleted() {
        final Stack stack = new Stack();
        new LnApplication(new Span("foo a b > x", 1))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a happlication cannot receive deeper-indent children so it pushes HORIZONTAL_COMPLETED",
            stack.top().openness(),
            Matchers.equalTo(Openness.HORIZONTAL_COMPLETED)
        );
    }

    @Test
    void emitsHorizontalArgsAsChildren() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo a b > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "horizontal args must appear as children inside the head's <o>, not as siblings",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@base='foo']/o[@base='a']",
                "/object/o[@base='foo']/o[@base='b']"
            )
        );
    }

    @Test
    void emitsArgsInOrder() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo a b c > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "horizontal args must emit in source order",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@base='foo']/o[1][@base='a']",
                "/object/o[@base='foo']/o[2][@base='b']",
                "/object/o[@base='foo']/o[3][@base='c']"
            )
        );
    }

    @Test
    void attachesArgsToLastLink() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo.bar a b > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "for chained heads, args must live inside the last chain link per R-9.0.3.1",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@base='.bar']/o[@base='a']",
                "/object/o[@base='.bar']/o[@base='b']"
            )
        );
    }

    @Test
    void recordsArgPosition() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo a b > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "each arg's @pos must point at its starting column in the source line",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o/o[@base='a' and @pos='4']")
        );
    }

    @Test
    void emitsIntegerArgAsNumberWrapper() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo 42 > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "an INT arg must wrap in <o base='Φ.number'> with a <o base='Φ.bytes'> child carrying the hex",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@base='foo']/o[@base='Φ.number']",
                "/object/o[@base='foo']/o[@base='Φ.number']/o[@base='Φ.bytes']"
            )
        );
    }

    @Test
    void encodesIntegerAsBytes() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo 42 > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "an INT arg's <o base='Φ.bytes'> must carry the IEEE-754 hex of the value",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o/o[@base='Φ.number']/o[@base='Φ.bytes']/o[text()='40-45-00-00-00-00-00-00']"
            )
        );
    }

    @Test
    void acceptsSignedIntegerArg() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo -3 > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a signed INT arg must parse with the sign preserved in the encoded value",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o/o[@base='Φ.number']/o[@base='Φ.bytes']/o[text()='C0-08-00-00-00-00-00-00']"
            )
        );
    }

    @Test
    void rejectsIntegerArgWithLeadingZero() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnApplication(new Span("foo 07", 1))
                .into(new Stack(), new Globals(), new Emit()),
            "an INT arg with a leading zero must be rejected per R-9.8.1"
        );
    }

    @Test
    void emitsStarHead() {
        final Emit emit = new Emit();
        new LnApplication(new Span("* > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a `*` head must emit <o base='Φ.tuple' star=''> per §9.4.2",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='x' and @base='Φ.tuple' and @star='']")
        );
    }

    @Test
    void emitsIntegerHead() {
        final Emit emit = new Emit();
        new LnApplication(new Span("42 > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "an INT head must emit the <o base='Φ.number'> wrapper with bytes child",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='x' and @base='Φ.number']",
                "/object/o[@name='x']/o[@base='Φ.bytes']"
            )
        );
    }

    @Test
    void emitsFloatHead() {
        final Emit emit = new Emit();
        new LnApplication(new Span("3.14 > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a FLOAT head must emit <o base='Φ.number'> with the bytes child",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='x' and @base='Φ.number']",
                "/object/o[@name='x']/o[@base='Φ.bytes']"
            )
        );
    }

    @Test
    void emitsStringHead() {
        final Emit emit = new Emit();
        new LnApplication(new Span("\"hello\" > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a STRING head must emit <o base='Φ.string'> with the bytes child",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='x' and @base='Φ.string']",
                "/object/o[@name='x']/o[@base='Φ.bytes']/o[text()='68-65-6C-6C-6F']"
            )
        );
    }

    @Test
    void emitsStringArg() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo \"hi\" > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a STRING arg must wrap in Φ.string with bytes carrying UTF-8 hex",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@base='foo']/o[@base='Φ.string']")
        );
    }

    @Test
    void rejectsStringWithInvalidUnicodeEscape() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnApplication(new Span("\"\\uZZZZ\" > x", 1))
                .into(new Stack(), new Globals(), new Emit()),
            "a string with a non-hex \\u escape must be rejected, not crash"
        );
    }

    @Test
    void emitsRootIdentifierAsHead() {
        final Emit emit = new Emit();
        new LnApplication(new Span("Q > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "`Q` head must emit <o base='Φ'> per §9.3",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='x' and @base='Φ']")
        );
    }

    @Test
    void emitsXiAsHead() {
        final Emit emit = new Emit();
        new LnApplication(new Span("$ > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "`$` head must emit <o base='ξ'> per §9.3",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='x' and @base='ξ']")
        );
    }

    @Test
    void chainsAfterRootHead() {
        final Emit emit = new Emit();
        new LnApplication(new Span("Q.foo > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a chain after a ROOT head must emit `Φ` plus `.foo` as flat siblings",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[1][@base='Φ']",
                "/object/o[2][@base='.foo' and @method='' and @name='x']"
            )
        );
    }

    @Test
    void emitsParenGroupWithChain() {
        final Emit emit = new Emit();
        new LnApplication(new Span("(scanner stdin).nextInt > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a paren group head plus chain must emit the group as one sibling and the chain link as another",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[1][@base='scanner']/o[@base='stdin']",
                "/object/o[2][@name='x' and @base='.nextInt' and @method='']"
            )
        );
    }

    @Test
    void emitsParenGroupAsArg() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo (a b) > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a paren group in arg position must emit as a single nested arg child",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@base='foo']/o[@base='a']/o[@base='b']")
        );
    }

    @Test
    void rejectsUnclosedInlinePhiBracketInGroupArg() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnApplication(new Span("x (a > [b)", 1))
                .into(new Stack(), new Globals(), new Emit()),
            "a `> [` inline-phi marker with no closing `]` inside a paren-group arg must be"
                .concat(" rejected with a ParseError, not an unchecked exception")
        );
    }

    @Test
    void emitsAsAttributeForLabelBinding() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo a:y b:z > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "an arg with `:label` binding must attach @as='<label>' to its <o>",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@base='foo']/o[@base='a' and @as='y']",
                "/object/o[@base='foo']/o[@base='b' and @as='z']"
            )
        );
    }

    @Test
    void emitsAlphaAsAttributeForNumericBinding() {
        final Emit emit = new Emit();
        new LnApplication(new Span("foo a:0 b:1 > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "an arg with `:N` binding must attach @as='αN' per R-3.12.2",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@base='foo']/o[@base='a' and @as='α0']",
                "/object/o[@base='foo']/o[@base='b' and @as='α1']"
            )
        );
    }

    @Test
    void emitsHexHead() {
        final Emit emit = new Emit();
        new LnApplication(new Span("0x1F > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a HEX head must emit Φ.number with bytes derived from the long parsed value",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='x' and @base='Φ.number']",
                "/object/o[@name='x']/o[@base='Φ.bytes']/o[text()='40-3F-00-00-00-00-00-00']"
            )
        );
    }

    @Test
    void rejectsOversizedHexHead() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new LnApplication(new Span("0x10000000000000000 > x", 1))
                .into(new Stack(), new Globals(), new Emit()),
            "a HEX literal wider than a signed 64-bit long must raise a positioned"
                .concat(" ParseError instead of an uncaught NumberFormatException")
        );
    }

    @Test
    void rejectsOverPreciseFloatHead() {
        MatcherAssert.assertThat(
            "a FLOAT literal with dead trailing digits must name the offender and its Double.toString form",
            Assertions.assertThrows(
                ParseError.class,
                () -> new LnApplication(new Span("2.7182818284590452354 > e", 1))
                    .into(new Stack(), new Globals(), new Emit())
            ).getMessage(),
            Matchers.equalTo(
                "2.7182818284590452354 is over-precise, write 2.718281828459045 instead"
            )
        );
    }

    @Test
    void rejectsOverPreciseIntegerHead() {
        MatcherAssert.assertThat(
            "an INTEGER past the exact double integer range must suggest the rounded spelling",
            Assertions.assertThrows(
                ParseError.class,
                () -> new LnApplication(new Span("9007199254740993 > x", 1))
                    .into(new Stack(), new Globals(), new Emit())
            ).getMessage(),
            Matchers.equalTo(
                "9007199254740993 is over-precise, write 9007199254740992 instead"
            )
        );
    }

    @Test
    void acceptsCanonicalFloatHead() {
        final Emit emit = new Emit();
        new LnApplication(new Span("0.1 > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "0.1 is already Double.toString's shortest form and must parse",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='x' and @base='Φ.number']")
        );
    }

    @Test
    void emitsBytesHead() {
        final Emit emit = new Emit();
        new LnApplication(new Span("CA-FE > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a BYTES head must emit <o base='Φ.bytes'> with the raw hex string as content",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='x' and @base='Φ.bytes']/o[text()='CA-FE']"
            )
        );
    }

    @Test
    void emitsEmptyBytes() {
        final Emit emit = new Emit();
        new LnApplication(new Span("-- > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "an empty BYTES literal `--` must emit text='--'",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='x' and @base='Φ.bytes']/o[text()='--']")
        );
    }

    @Test
    void emitsSingleByteBytes() {
        final Emit emit = new Emit();
        new LnApplication(new Span("FF- > x", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a single-byte BYTES `FF-` must emit text='FF-'",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='x' and @base='Φ.bytes']/o[text()='FF-']")
        );
    }

    @Test
    void wrapsBytesHexInNestedChild() {
        final Emit emit = new Emit();
        new LnApplication(new Span("AA-BB > y", 1))
            .into(new Stack(), new Globals(), emit);
        emit.close();
        MatcherAssert.assertThat(
            "a BYTES literal must put the hex text in a nested unnamed <o> child, not on the @base='Φ.bytes' wrapper itself",
            LnApplicationTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='y' and @base='Φ.bytes']/o[not(@base) and not(@name) and text()='AA-BB']"
            )
        );
    }

    @Test
    void replacesTopOnSameIndentSibling() {
        final Stack stack = new Stack();
        new LnApplication(new Span("alpha > a", 1))
            .into(stack, new Globals(), new Emit());
        new LnApplication(new Span("beta > b", 2))
            .into(stack, new Globals(), new Emit());
        MatcherAssert.assertThat(
            "a same-indent sibling head must replace the top, leaving depth 1",
            stack.depth(),
            Matchers.equalTo(1)
        );
    }

    /**
     * Render the emit's directives under a fresh {@code <object/>}.
     * @param emit The emit
     * @return XMIR
     */
    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
