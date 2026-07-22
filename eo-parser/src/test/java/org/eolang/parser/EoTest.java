/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Eo}.
 *
 * <p>End-to-end tests of the walker — feed EO source text, assert the
 * shape of the emitted XMIR. Tests focus on the line shapes implemented
 * so far (blank, comment, meta) plus the cross-cutting concerns
 * (recovery, indent validation, EOF checks).</p>
 *
 * @since 0.1
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.AvoidDuplicateLiterals", "PMD.UnnecessaryLocalRule"})
final class EoTest {

    @Test
    void parsesEmptySourceWithNoOutput() {
        MatcherAssert.assertThat(
            "an empty source must produce a bare <object/> with no metas, comments, or errors",
            EoTest.render(),
            XhtmlMatchers.hasXPaths(
                "/object",
                "/object[not(metas)]",
                "/object[not(errors)]"
            )
        );
    }

    @Test
    void parsesSingleMeta() {
        MatcherAssert.assertThat(
            "a single-line +foo program must produce one meta and zero errors",
            EoTest.render("+foo"),
            XhtmlMatchers.hasXPaths(
                "/object/metas/meta/head[text()='foo']",
                "/object[not(errors)]"
            )
        );
    }

    @Test
    void parsesMultipleMetas() {
        MatcherAssert.assertThat(
            "consecutive metas at indent 0 must accumulate under one <metas>",
            EoTest.render("+alpha", "+beta", "+gamma"),
            XhtmlMatchers.hasXPath("/object/metas[count(meta)=3]")
        );
    }

    @Test
    void flushesTopCommentBlock() {
        MatcherAssert.assertThat(
            "a comment block on top of the file must flush into /object/comments",
            EoTest.render("# top doc", "", "[] > foo"),
            XhtmlMatchers.hasXPaths(
                "/object/comments/comment[contains(text(),'top doc')]",
                "/object[not(errors)]"
            )
        );
    }

    @Test
    void rejectsCommentAfterObject() {
        MatcherAssert.assertThat(
            "a comment after an object cannot be accepted — only the top block is allowed",
            EoTest.render("[] > foo", "# late", "  bar > @"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'comment is allowed only on top of the file, before metas')]"
            )
        );
    }

    @Test
    void rejectsTopCommentWithoutBlankBelow() {
        MatcherAssert.assertThat(
            "a top comment block not separated from the object by a blank line cannot be accepted",
            EoTest.render("# top doc", "[] > foo"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'a blank line must separate the top comment block from the rest of the file')]"
            )
        );
    }

    @Test
    void reportsOddIndentError() {
        MatcherAssert.assertThat(
            "a line whose indent is an odd number of spaces must surface the odd-indent error",
            EoTest.render(" +foo"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'unexpected odd indent')]"
            )
        );
    }

    @Test
    void reportsTabInLeadingWhitespace() {
        MatcherAssert.assertThat(
            "a tab inside leading whitespace must be reported with the canonical message",
            EoTest.render("\tfoo"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'tab character in leading whitespace')]"
            )
        );
    }

    @Test
    void recoversFromBadLineAndContinues() {
        final String rendered = EoTest.render("+ok-one", "  +bad-indent", "+ok-two");
        MatcherAssert.assertThat(
            "after an error the walker must continue and parse subsequent valid lines",
            rendered,
            XhtmlMatchers.hasXPaths(
                "/object/metas/meta[head='ok-one']",
                "/object/metas/meta[head='ok-two']"
            )
        );
    }

    @Test
    void parsesWithUnixAndWindowsLineEndings() {
        final String carriage = String.valueOf((char) 13);
        MatcherAssert.assertThat(
            "the walker must accept CRLF the same as LF (R-2.1.2)",
            EoTest.render("+alpha".concat(carriage), "+beta".concat(carriage)),
            XhtmlMatchers.hasXPath("/object/metas[count(meta)=2]")
        );
    }

    @Test
    void reportsExcessiveTrailingBlanks() {
        MatcherAssert.assertThat(
            "more than one trailing blank line at EOF must be reported per R-6.5.6",
            EoTest.render("+foo", "", ""),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'more than one trailing blank line')]"
            )
        );
    }

    @Test
    void parsesTopLevelFormation() {
        MatcherAssert.assertThat(
            "a single `[] > foo` line must emit one named <o> under the program root",
            EoTest.render("[] > foo"),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='foo' and @line='1' and @pos='0']",
                "/object[not(errors)]"
            )
        );
    }

    @Test
    void parsesNestedFormations() {
        MatcherAssert.assertThat(
            "a nested `[] > inner` under `[] > outer` must emit one <o> inside the other",
            EoTest.render("[] > outer", "  [] > inner"),
            XhtmlMatchers.hasXPath("/object/o[@name='outer']/o[@name='inner']")
        );
    }

    @Test
    void parsesFormationWithVoidParams() {
        MatcherAssert.assertThat(
            "a `[a b]` declaration must emit two void params inside the formation",
            EoTest.render("[a b] > foo"),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='foo']/o[@name='a' and @base='∅']",
                "/object/o[@name='foo']/o[@name='b' and @base='∅']"
            )
        );
    }

    @Test
    void parsesVerticalVoidAttribute() {
        MatcherAssert.assertThat(
            "a `? > x` body line must emit a void param inside the formation",
            EoTest.render("[] > foo", "  ? > x"),
            XhtmlMatchers.hasXPath("/object/o[@name='foo']/o[@name='x' and @base='∅']")
        );
    }

    @Test
    void rejectsVoidWithMethodDispatch() {
        MatcherAssert.assertThat(
            "a `?.method` dispatch on the void marker must be rejected",
            EoTest.render("[] > foo", "  ?.read > x"),
            XhtmlMatchers.hasXPath("/object/errors/error")
        );
    }

    @Test
    void rejectsVoidAsArgument() {
        MatcherAssert.assertThat(
            "the void marker `?` must not be accepted as a horizontal argument",
            EoTest.render("[] > foo", "  bar ? baz > x"),
            XhtmlMatchers.hasXPath("/object/errors/error")
        );
    }

    @Test
    void rejectsVoidInReversedArgument() {
        MatcherAssert.assertThat(
            "the void marker `?` must not be accepted as a reversed-dispatch argument",
            EoTest.render("[] > foo", "  bar. ? q > m"),
            XhtmlMatchers.hasXPath("/object/errors/error")
        );
    }

    @Test
    void parsesAtomVoidWithFormaList() {
        MatcherAssert.assertThat(
            "a void in an atom must carry its `/{…}` argument list as a raw @args attribute",
            EoTest.render("[] > fopen /file", "  ? > not-found /{string io.file-error}"),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='fopen']/o[@name='not-found' and @base='∅' and @args='string io.file-error']"
            )
        );
    }

    @Test
    void rejectsFormaListOutsideAtom() {
        MatcherAssert.assertThat(
            "a `/{…}` forma-list on a void outside an atom must be rejected",
            EoTest.render("[] > foo", "  ? > x /{string}"),
            XhtmlMatchers.hasXPath("/object/errors/error")
        );
    }

    @Test
    void parsesGenericAtomReturnType() {
        MatcherAssert.assertThat(
            "an atom with a generic return must keep the type variable verbatim on its lambda marker",
            EoTest.render("[] > recovered /A"),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='recovered']/o[@name='λ' and @atom='A']"
            )
        );
    }

    @Test
    void parsesVoidWithGenericType() {
        MatcherAssert.assertThat(
            "a void in an atom must carry its bare `/type` as a raw @type attribute",
            EoTest.render("[] > recovered /A", "  ? > value /A?"),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='recovered']/o[@name='value' and @base='∅' and @type='A?']"
            )
        );
    }

    @Test
    void rejectsGenericTypeOutsideRange() {
        MatcherAssert.assertThat(
            "a type variable outside A-F must be rejected",
            EoTest.render("[] > foo /G"),
            XhtmlMatchers.hasXPath("/object/errors/error")
        );
    }

    @Test
    void rejectsVoidBelowRegularAttribute() {
        MatcherAssert.assertThat(
            "a void declared after a regular attribute must be rejected",
            EoTest.render("[] > foo", "  6 > six", "  ? > x", "  5 > five", "  ? > y"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'void attribute must be declared above')]"
            )
        );
    }

    @Test
    void parsesAtomDeclaration() {
        MatcherAssert.assertThat(
            "a `/sig` suffix must emit the λ marker inside the formation",
            EoTest.render("[a] > foo /number"),
            XhtmlMatchers.hasXPath("/object/o[@name='foo']/o[@name='λ' and @atom='number']")
        );
    }

    @Test
    void rejectsTopLevelFormationWithoutName() {
        MatcherAssert.assertThat(
            "a top-level formation lacking a name suffix must trigger the naming check",
            EoTest.render("[]"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'object inside formation must have a name')]"
            )
        );
    }

    @Test
    void allowsSameIndentSiblingFormations() {
        MatcherAssert.assertThat(
            "two top-level formations must emit as two sibling <o>s under the program root",
            EoTest.render("[] > first", "[] > second"),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='first']",
                "/object/o[@name='second']",
                "/object[count(o)=2]"
            )
        );
    }

    @Test
    void parsesApplicationInsideFormation() {
        MatcherAssert.assertThat(
            "an identifier head inside a formation must emit as a child <o>",
            EoTest.render("[] > main", "  bar > x"),
            XhtmlMatchers.hasXPath("/object/o[@name='main']/o[@name='x' and @base='bar']")
        );
    }

    @Test
    void parsesBottomTermInsideFormation() {
        MatcherAssert.assertThat(
            "a bare T term inside a formation must emit the bottom object as @base='⊥'",
            EoTest.render("[] > main", "  T > x"),
            XhtmlMatchers.hasXPath("/object/o[@name='main']/o[@name='x' and @base='⊥']")
        );
    }

    @Test
    void parsesFragileHmethodInsideFormation() {
        MatcherAssert.assertThat(
            "a `?.` fragile dispatch must emit the method link with @fragile=''",
            EoTest.render("[] > main", "  foo?.bar > x"),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='main']/o[@base='.bar' and @method='' and @fragile='']"
            )
        );
    }

    @Test
    void parsesHmethodInsideFormation() {
        MatcherAssert.assertThat(
            "a dotted-chain head inside a formation must emit flat siblings",
            EoTest.render("[] > main", "  foo.bar > x"),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='main']/o[@base='foo']",
                "/object/o[@name='main']/o[@base='.bar' and @method='']"
            )
        );
    }

    @Test
    void parsesFragileVmethodContinuationLine() {
        MatcherAssert.assertThat(
            "a `?.read` continuation line must emit the link with @fragile=''",
            EoTest.render("[] > main", "  foo > x", "  .bar", "  ?.read > y"),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='main']/o[@base='.read' and @method='' and @fragile='']"
            )
        );
    }

    @Test
    void parsesFragileReversedDispatch() {
        MatcherAssert.assertThat(
            "a `name?.` reversed dispatch must emit the link with @fragile='' and no @method",
            EoTest.render("[] > main", "  mul?. x y > z"),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='main']/o[@name='z' and @base='.mul' and @fragile='' and not(@method)]"
            )
        );
    }

    @Test
    void keepsVoidLineDistinctFromFragileMethodLine() {
        MatcherAssert.assertThat(
            "a `? > name` line stays a void even though `?.` starts a method line",
            EoTest.render("[] > foo", "  ? > x"),
            XhtmlMatchers.hasXPath("/object/o[@name='foo']/o[@name='x' and @base='∅']")
        );
    }

    @Test
    void parsesHapplicationInsideFormation() {
        MatcherAssert.assertThat(
            "a head with horizontal args inside a formation must nest the args inside the head",
            EoTest.render("[] > main", "  foo a b > x"),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='main']/o[@base='foo']/o[@base='a']",
                "/object/o[@name='main']/o[@base='foo']/o[@base='b']"
            )
        );
    }

    @Test
    void parsesIntegerInsideFormation() {
        MatcherAssert.assertThat(
            "an INT literal as a child of a formation must emit the Φ.number wrapper",
            EoTest.render("[] > main", "  42 > x"),
            XhtmlMatchers.hasXPath("/object/o[@name='main']/o[@name='x' and @base='Φ.number']")
        );
    }

    @Test
    void promotesHeadToVapplicationWhenChildrenLand() {
        MatcherAssert.assertThat(
            "a head followed by deeper-indent children must end up containing them",
            EoTest.render("[] > main", "  foo > x", "    a", "    b"),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='main']/o[@name='x' and @base='foo']/o[@base='a']",
                "/object/o[@name='main']/o[@name='x' and @base='foo']/o[@base='b']"
            )
        );
    }

    @Test
    void rejectsDeeperChildrenUnderHapplication() {
        MatcherAssert.assertThat(
            "a deeper-indent line under a happlication must be rejected per R-6.1.1",
            EoTest.render("[] > main", "  foo a b > x", "    deep"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'previous expression is closed for children')]"
            )
        );
    }

    @Test
    void parsesReversedDispatchVertical() {
        MatcherAssert.assertThat(
            "a vertical reversed dispatch must emit <o base='.if'> wrapping its children, without @method (it opens a new chain, not continues one)",
            EoTest.render("[] > main", "  if. > @", "    cond", "    then", "    other"),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='main']/o[@name='φ' and @base='.if' and not(@method)]",
                "/object/o[@name='main']/o[@base='.if']/o[@base='cond']"
            )
        );
    }

    @Test
    void parsesReversedDispatchHorizontal() {
        MatcherAssert.assertThat(
            "a horizontal reversed dispatch must emit args as children in source order",
            EoTest.render("[] > main", "  if. cond then else > x"),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='main']/o[@name='x' and @base='.if']",
                "/object/o[@name='main']/o[@base='.if']/o[@base='cond']",
                "/object/o[@name='main']/o[@base='.if']/o[@base='then']",
                "/object/o[@name='main']/o[@base='.if']/o[@base='else']"
            )
        );
    }

    @Test
    void reportsMissingReceiverForBareReversed() {
        MatcherAssert.assertThat(
            "a bare-reversed line with no deeper receiver must surface R-5.3.2",
            EoTest.render("[] > main", "  if. > @"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'reversed dispatch missing receiver')]"
            )
        );
    }

    @Test
    void parsesVerticalMethodChainNamedOnLastLink() {
        MatcherAssert.assertThat(
            "a `head` followed by `.method > x` continuations must name the last link",
            EoTest.render("[] > main", "  tmpdir", "  .tmpfile", "  .open > out"),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='main']/o[@base='tmpdir']",
                "/object/o[@name='main']/o[@base='.tmpfile' and not(@name)]",
                "/object/o[@name='main']/o[@name='out' and @base='.open']"
            )
        );
    }

    @Test
    void parsesCompactTupleWithNDirectChildren() {
        MatcherAssert.assertThat(
            "with *N, the first N children must stay direct and the rest wrap in Φ.tuple",
            EoTest.render(
                "[] > main",
                "  sprintf *1 > xyz",
                "    \"Hello\"",
                "    \"world\""
            ),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='main']/o[@name='xyz' and @base='sprintf']/o[1][@base='Φ.string']",
                "/object/o[@name='main']/o[@name='xyz']/o[2][@base='Φ.tuple' and @star='']"
            )
        );
    }

    @Test
    void parsesCompactTupleWithZeroN() {
        MatcherAssert.assertThat(
            "with *0 every child must land inside the Φ.tuple wrapper",
            EoTest.render(
                "[] > main",
                "  seq * > xyz",
                "    foo",
                "    bar"
            ),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='main']/o[@name='xyz']/o[@base='Φ.tuple' and @star='']/o[@base='foo']"
            )
        );
    }

    @Test
    void rejectsCompactTupleWithFewerThanNChildren() {
        MatcherAssert.assertThat(
            "a *N requiring N children with only some present must surface R-5.3.3",
            EoTest.render(
                "[] > main",
                "  sprintf *3 > xyz",
                "    foo"
            ),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'compact tuple requires at least')]"
            )
        );
    }

    @Test
    void parsesTripleQuotedTextBlock() {
        MatcherAssert.assertThat(
            "a triple-quoted block must produce a Φ.string with bytes carrying the body",
            EoTest.render(
                "[] > main",
                "  \"\"\"",
                "  hello",
                "  world",
                "  \"\"\" > greeting"
            ),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='main']/o[@name='greeting' and @base='Φ.string']"
            )
        );
    }

    @Test
    void stripsOpenerIndentFromTextBlockBody() {
        MatcherAssert.assertThat(
            "the body of a text block must have the opener indent stripped before joining",
            EoTest.render(
                "[] > main",
                "  \"\"\"",
                "  hi",
                "  \"\"\" > x"
            ),
            XhtmlMatchers.hasXPath(
                "/object//o[@name='x']/o[@base='Φ.bytes']/o[text()='68-69']"
            )
        );
    }

    @Test
    void emitsCompactTupleWithExtraChildrenAsTupleWrap() {
        MatcherAssert.assertThat(
            "a compact-tuple head with N=1 and 2 vertical children must place the first child directly and wrap the rest in Φ.tuple",
            EoTest.render(
                "[] > app",
                "  foo *1 > @",
                "    x",
                "    y"
            ),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='app']/o[@name='φ']/o[@base='Φ.tuple']/o[@base='y']"
            )
        );
    }

    @Test
    void rejectsMethodContinuationAfterVmethodWithHargs() {
        MatcherAssert.assertThat(
            "vmethod-with-hargs is horizontally-completed — a following `.method` must surface R-5.2.3(b)",
            EoTest.render(
                "[] > main",
                "  obj > x",
                "  .method 1",
                "  .other > y"
            ),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'method continuation not allowed after horizontal application')]"
            )
        );
    }

    @Test
    void acceptsVmethodWithHargsAsTerminator() {
        MatcherAssert.assertThat(
            "vmethod-with-hargs is legal as the terminating link of a chain (no `.method` follows it)",
            EoTest.render(
                "[] > main",
                "  obj > x",
                "  .method y"
            ),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void acceptsVmethodContinuationAfterVapp() {
        MatcherAssert.assertThat(
            "after a vapplication (parent already has vertical children), a `.method` (no hargs) continuation extends the chain",
            EoTest.render(
                "[] > main",
                "  obj > x",
                "    child",
                "  .method > y"
            ),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void reportsTextBlockBodyLineWithNegativeIndent() {
        MatcherAssert.assertThat(
            "a body line indented less than the opener must surface an error per R-3.11.2",
            EoTest.render(
                "[] > main",
                "  \"\"\"",
                "foo",
                "  \"\"\" > neg"
            ),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'text block body line indented less than opener')]"
            )
        );
    }

    @Test
    void reportsUnclosedTextBlockAtEof() {
        MatcherAssert.assertThat(
            "a text block opened without a closer must surface unclosed-text-block at EOF",
            EoTest.render(
                "[] > main",
                "  \"\"\"",
                "  unfinished"
            ),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'unclosed text block')]"
            )
        );
    }

    @Test
    void acceptsAllUnboundHorizontalArgs() {
        MatcherAssert.assertThat(
            "all-unbound is a valid mode per R-6.6.2 and must parse without errors",
            EoTest.render("[] > main", "  foo a b c > x"),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void acceptsAllBoundHorizontalArgs() {
        MatcherAssert.assertThat(
            "all-bound is a valid mode per R-6.6.2 and must parse without errors",
            EoTest.render("[] > main", "  foo a:x b:y > z"),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void reportsMixedBindings() {
        MatcherAssert.assertThat(
            "mixed bound and unbound args in the same group must surface R-6.6.2",
            EoTest.render("[] > main", "  foo a:x b > z"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'argument bindings must be all-or-nothing')]"
            )
        );
    }

    @Test
    void acceptsReversedReceiverBareAndArgsBound() {
        MatcherAssert.assertThat(
            "the reversed receiver is exempt; the remaining args may be all-bound per R-6.6.2/R-6.6.3",
            EoTest.render("[] > main", "  if. cond then:a else:b > z"),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void rejectsBoundReversedReceiver() {
        MatcherAssert.assertThat(
            "a receiver carrying `:x` must surface R-6.6.3",
            EoTest.render("[] > main", "  if. cond:x then else > z"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'reversed-dispatch receiver cannot carry a binding')]"
            )
        );
    }

    @Test
    void rejectsMixedBindingsAmongReversedArgs() {
        MatcherAssert.assertThat(
            "the rule still applies to args after the receiver in a reversed dispatch",
            EoTest.render("[] > main", "  if. cond then:a else > z"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'argument bindings must be all-or-nothing')]"
            )
        );
    }

    @Test
    void emitsAsForVerticalBindingOnIdentifier() {
        MatcherAssert.assertThat(
            "a vertical-arg line ending with `:label` must emit @as on the line's outermost <o>",
            EoTest.render("foo > main", "  bar:tag"),
            XhtmlMatchers.hasXPath("/object/o[@name='main']/o[@base='bar' and @as='tag']")
        );
    }

    @Test
    void emitsAsForVerticalBindingWithSuffix() {
        MatcherAssert.assertThat(
            "the binding may combine with `> name`, both attaching to the same <o>",
            EoTest.render("foo > main", "  bar:tag > out"),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='main']/o[@name='out' and @base='bar' and @as='tag']"
            )
        );
    }

    @Test
    void emitsNumericVerticalBindingAsAlpha() {
        MatcherAssert.assertThat(
            "a numeric `:N` binding must become @as='αN' (R-3.12.2)",
            EoTest.render("foo > main", "  bar:0"),
            XhtmlMatchers.hasXPath("/object/o[@name='main']/o[@base='bar' and @as='α0']")
        );
    }

    @Test
    void rejectsMixedVerticalBindings() {
        MatcherAssert.assertThat(
            "vertical args under a vapplication must follow R-6.6.2 — mixing bound and unbound is rejected",
            EoTest.render("[] > main", "  foo > app", "    a:x", "    b"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'argument bindings must be all-or-nothing')]"
            )
        );
    }

    @Test
    void acceptsAllBoundVerticalArgs() {
        MatcherAssert.assertThat(
            "vertical args may all carry bindings — the uniform pattern is valid",
            EoTest.render("[] > main", "  foo > app", "    a:x", "    b:y"),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void rejectsBindingOnFormationChild() {
        MatcherAssert.assertThat(
            "a plain child of a formation cannot carry a binding per R-3.12.3",
            EoTest.render("[] > main", "  bar:tag > x"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'binding allowed only in argument position')]"
            )
        );
    }

    @Test
    void rejectsBindingOnVerticalReceiver() {
        MatcherAssert.assertThat(
            "the receiver of a vertical reversed dispatch cannot carry a binding either",
            EoTest.render("foo > main", "  if.", "    cond:x", "    then", "    other"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'reversed-dispatch receiver cannot carry a binding')]"
            )
        );
    }

    @Test
    void acceptsInlineVoidsOnReversedDispatch() {
        MatcherAssert.assertThat(
            "a trailing-dot object with an inline void suffix must parse with no errors and desugar to the same φ-as-reversed-dispatch shape as its two-line form",
            EoTest.render(
                "[x] > obj",
                "  if. > [t] >> rec",
                "    t.eq 0",
                "    1",
                "    2",
                "  rec x > @"
            ),
            XhtmlMatchers.hasXPaths(
                "/object[not(errors)]",
                "/object/o[@name='obj']/o[@local='rec']/o[@name='t' and @base='∅']",
                "/object/o[@name='obj']/o[@local='rec']/o[@name='φ' and @base='.if' and not(@method)]"
            )
        );
    }

    @Test
    void mergesMultiLineBytesAcrossTwoLines() {
        MatcherAssert.assertThat(
            "two BYTES lines with trailing dash on the first must merge into one Φ.bytes token",
            EoTest.render("foo > main", "  CA-FE-", "  BE-BE"),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='main']/o[@base='Φ.bytes']/o[text()='CA-FE-BE-BE']"
            )
        );
    }

    @Test
    void mergesMultiLineBytesAcrossThreeLines() {
        MatcherAssert.assertThat(
            "three-line BYTES continuation must concatenate all chunks into one token",
            EoTest.render("foo > main", "  CA-FE-", "  BE-BE-", "  AB-CD"),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='main']/o[@base='Φ.bytes']/o[text()='CA-FE-BE-BE-AB-CD']"
            )
        );
    }

    @Test
    void leavesSingleLineBytesAlone() {
        MatcherAssert.assertThat(
            "a single-line BYTES literal without trailing dash must NOT consume the next line",
            EoTest.render("[] > main", "  CA-FE > x", "  bar > y"),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='main']/o[@name='x' and @base='Φ.bytes']",
                "/object/o[@name='main']/o[@name='y' and @base='bar']"
            )
        );
    }

    @Test
    void parsesAbsEoStyleProgram() {
        MatcherAssert.assertThat(
            "an abs.eo-style program must parse cleanly with no /object/errors element",
            EoTest.render(
                "[num] > abs",
                "  if. > @",
                "    value",
                "    value",
                "    value",
                "  number > value"
            ),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void attachesMetasBeforeFormation() {
        MatcherAssert.assertThat(
            "a meta header before a formation must remain at /object/metas and not nest inside the <o>",
            EoTest.render("+alpha", "", "[] > foo"),
            XhtmlMatchers.hasXPaths(
                "/object/metas/meta/head[text()='alpha']",
                "/object/o[@name='foo']"
            )
        );
    }

    @Test
    void parsesEmptyProgramWithSingleTrailingBlank() {
        MatcherAssert.assertThat(
            "a single trailing blank line is legal — no error must surface",
            EoTest.render("+foo"),
            Matchers.not(
                XhtmlMatchers.hasXPath(
                    "/object/errors/error[contains(text(),'trailing blank')]"
                )
            )
        );
    }

    /**
     * Run the EO source through the walker and render the XMIR under a
     * fresh {@code <object/>} root. The supplied rows are joined with
     * a literal newline (no platform-dependent separator) and a
     * trailing newline is appended — matching what the parser expects.
     * @param rows The EO program lines (no terminators)
     * @return Rendered XMIR
     */
    private static String render(final String... rows) {
        final StringBuilder source = new StringBuilder(rows.length * 16);
        for (final String row : rows) {
            source.append(row).append((char) 10);
        }
        return new Xembler(
            new Directives().add("object").append(
                new Eo(source.toString()).directives()
            )
        ).xmlQuietly();
    }
}
