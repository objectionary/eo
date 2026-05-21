/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Emit}.
 * @since 0.1
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnnecessaryLocalRule", "PMD.AvoidDuplicateLiterals"})
final class EmitTest {

    @Test
    void emitsMetaWithHeadAndParts() {
        final Emit emit = new Emit();
        emit.meta(1, "alias", Collections.singletonList("org.example.foo"));
        MatcherAssert.assertThat(
            "a meta directive must produce <metas><meta line=N><head>...<tail>...<part>...",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/metas/meta[@line='1']",
                "/object/metas/meta/head[text()='alias']",
                "/object/metas/meta/tail[text()='org.example.foo']",
                "/object/metas/meta/part[text()='org.example.foo']"
            )
        );
    }

    @Test
    void emitsMetaWithoutParts() {
        final Emit emit = new Emit();
        emit.meta(2, "foo", Collections.emptyList());
        MatcherAssert.assertThat(
            "a parameterless meta must produce an empty <tail> and no <part> children",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/metas/meta[@line='2']/head[text()='foo']",
                "/object/metas/meta[not(part)]"
            )
        );
    }

    @Test
    void groupsMultipleMetasUnderOneMetasElement() {
        final Emit emit = new Emit();
        emit.meta(1, "alpha", Collections.emptyList());
        emit.meta(2, "beta", Collections.emptyList());
        MatcherAssert.assertThat(
            "consecutive meta directives must share one <metas> container",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object[count(metas)=1 and count(metas/meta)=2]")
        );
    }

    @Test
    void emitsCommentWithSingleLine() {
        final Emit emit = new Emit();
        emit.comment(Collections.singletonList(new Span("# hello", 7)), 8);
        MatcherAssert.assertThat(
            "a single-line comment must produce <comments><comment line='8'>hello</comment>, where the line is that of the attached named object",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/comments/comment[@line='8']",
                "/object/comments/comment[text()='hello']"
            )
        );
    }

    @Test
    void emitsCommentJoiningMultipleLines() {
        final Emit emit = new Emit();
        emit.comment(
            Arrays.asList(
                new Span("# first", 3),
                new Span("# second", 4)
            ),
            5
        );
        MatcherAssert.assertThat(
            "a multi-line comment must join the bodies with a newline",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/comments/comment[contains(text(),'first') and contains(text(),'second')]"
            )
        );
    }

    @Test
    void recordsTargetLineOnCommentAttribute() {
        final Emit emit = new Emit();
        emit.comment(
            Arrays.asList(
                new Span("# first", 3),
                new Span("# second", 4),
                new Span("# third", 5)
            ),
            6
        );
        MatcherAssert.assertThat(
            "a comment's @line attribute must point at the line of the named object it attaches to",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object/comments/comment[@line='6']")
        );
    }

    @Test
    void leavesNoOutputForEmptyCommentBlock() {
        final Emit emit = new Emit();
        emit.comment(Collections.emptyList(), 1);
        MatcherAssert.assertThat(
            "an empty comment block cannot produce any directives",
            EmitTest.render(emit),
            Matchers.not(XhtmlMatchers.hasXPath("/object/comments"))
        );
    }

    @Test
    void emitsErrorWithLineAndPos() {
        final Emit emit = new Emit();
        emit.error(5, 3, "unexpected odd indent");
        MatcherAssert.assertThat(
            "an error must record line, pos, severity=error, and the canonical text",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPaths(
                "/object/errors/error[@line='5' and @pos='3' and @severity='error']",
                "/object/errors/error[contains(text(),'[5:3]') and contains(text(),'unexpected odd indent')]"
            )
        );
    }

    @Test
    void returnsZeroSavepointForFreshEmit() {
        MatcherAssert.assertThat(
            "the initial savepoint of an empty Emit must be 0",
            new Emit().savepoint(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void growsSavepointAfterEmission() {
        final Emit emit = new Emit();
        final int before = emit.savepoint();
        emit.meta(1, "x", Collections.emptyList());
        MatcherAssert.assertThat(
            "after emitting, the savepoint must advance past the pre-emit position",
            emit.savepoint(),
            Matchers.greaterThan(before)
        );
    }

    @Test
    void dropsDirectivesAfterTokenOnRollback() {
        final Emit emit = new Emit();
        final int token = emit.savepoint();
        emit.error(1, 0, "boom");
        emit.rollback(token);
        MatcherAssert.assertThat(
            "rollback must drop the directives appended after the savepoint",
            EmitTest.render(emit),
            Matchers.not(XhtmlMatchers.hasXPath("/object/errors"))
        );
    }

    @Test
    void keepsDirectivesBeforeTokenOnRollback() {
        final Emit emit = new Emit();
        emit.meta(1, "keep", Collections.emptyList());
        final int token = emit.savepoint();
        emit.error(2, 0, "boom");
        emit.rollback(token);
        MatcherAssert.assertThat(
            "rollback must preserve directives appended before the savepoint",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object/metas/meta/head[text()='keep']")
        );
    }

    @Test
    void opensObjectWithNameAndPosition() {
        final Emit emit = new Emit();
        emit.object("foo", null, 3, 0);
        emit.close();
        MatcherAssert.assertThat(
            "object() must add an <o> at the current cursor with @name, @line, @pos",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='foo' and @line='3' and @pos='0']")
        );
    }

    @Test
    void opensObjectWithBase() {
        final Emit emit = new Emit();
        emit.object("hello", "Φ.string", 2, 4);
        emit.close();
        MatcherAssert.assertThat(
            "object() must emit @base when supplied",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='hello' and @base='Φ.string']")
        );
    }

    @Test
    void omitsNameAttributeWhenNull() {
        final Emit emit = new Emit();
        emit.object(null, "bar", 1, 0);
        emit.close();
        MatcherAssert.assertThat(
            "passing a null name must omit @name on the emitted <o>",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@base='bar' and not(@name)]")
        );
    }

    @Test
    void nestsChildObjectInsideOpenParent() {
        final Emit emit = new Emit();
        emit.object("outer", null, 1, 0);
        emit.object("inner", null, 2, 2);
        emit.close();
        emit.close();
        MatcherAssert.assertThat(
            "a child object() between two close() calls must appear nested inside its parent",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='outer']/o[@name='inner']")
        );
    }

    @Test
    void emitsVoidParam() {
        final Emit emit = new Emit();
        emit.object("foo", null, 1, 0);
        emit.voidParam("x", 1, 1);
        emit.close();
        MatcherAssert.assertThat(
            "voidParam must add <o name='x' base='∅'/> inside the parent",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='foo']/o[@name='x' and @base='∅']"
            )
        );
    }

    @Test
    void emitsAtomMarker() {
        final Emit emit = new Emit();
        emit.object("foo", null, 1, 0);
        emit.atomMarker("number", 1, 5);
        emit.close();
        MatcherAssert.assertThat(
            "atomMarker must add <o name='λ' atom='<sig>'/> inside the parent atom",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='foo']/o[@name='λ' and @atom='number']"
            )
        );
    }

    @Test
    void marksObjectAsMethodLink() {
        final Emit emit = new Emit();
        emit.object("x", ".bar", 1, 3);
        emit.method();
        emit.close();
        MatcherAssert.assertThat(
            "method() must attach @method='' to the most recently opened <o>",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='x' and @base='.bar' and @method='']")
        );
    }

    @Test
    void marksObjectWithAsAttribute() {
        final Emit emit = new Emit();
        emit.object(null, "foo", 1, 0);
        emit.slot("label");
        emit.close();
        MatcherAssert.assertThat(
            "as() must attach @as='label' for the inline-binding marker",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@base='foo' and @as='label']")
        );
    }

    @Test
    void marksObjectAsStar() {
        final Emit emit = new Emit();
        emit.object(null, "Φ.tuple", 1, 0);
        emit.star();
        emit.close();
        MatcherAssert.assertThat(
            "star() must attach @star='' to the most recently opened <o>",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@base='Φ.tuple' and @star='']")
        );
    }

    @Test
    void setsTextContent() {
        final Emit emit = new Emit();
        emit.object(null, "Φ.bytes", 1, 0);
        emit.set("CA-FE-BE");
        emit.close();
        MatcherAssert.assertThat(
            "set() must inject the given text as content of the current <o>",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@base='Φ.bytes' and text()='CA-FE-BE']")
        );
    }

    @Test
    void marksObjectAsConst() {
        final Emit emit = new Emit();
        emit.object("x", "bar", 1, 0);
        emit.constant();
        emit.close();
        MatcherAssert.assertThat(
            "constant() must attach @const='' to the most recently opened <o>",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='x' and @const='']")
        );
    }

    @Test
    void preservesCursorAcrossSidePanelEmissions() {
        final Emit emit = new Emit();
        emit.object("outer", null, 1, 0);
        emit.error(99, 7, "boom");
        emit.object("inner", null, 2, 2);
        emit.close();
        emit.close();
        MatcherAssert.assertThat(
            "an error emitted between object() and child object() cannot disturb the nesting",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object/o[@name='outer']/o[@name='inner']")
        );
    }

    @Test
    void retainsSidePanelErrorAlongsideTree() {
        final Emit emit = new Emit();
        emit.object("outer", null, 1, 0);
        emit.error(5, 0, "boom");
        emit.close();
        MatcherAssert.assertThat(
            "a side-panel error emitted mid-tree must still land under /object/errors",
            EmitTest.render(emit),
            XhtmlMatchers.hasXPath("/object/errors/error[contains(text(),'boom')]")
        );
    }

    /**
     * Run the emit's directives through Xembler against a fresh
     * {@code <object/>} root so XPath assertions see the same shape the
     * full parser would produce.
     * @param emit The emit
     * @return Rendered XMIR document as a string
     */
    private static String render(final Emit emit) {
        return new Xembler(
            new Directives().add("object").append(emit.directives())
        ).xmlQuietly();
    }
}
