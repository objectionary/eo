/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for the top comment block handling of {@link Eo}.
 *
 * @since 0.1
 */
final class EoCommentTest {

    @Test
    void flushesTopCommentBlock() {
        MatcherAssert.assertThat(
            "a comment block on top of the file must flush into /object/comments",
            EoCommentTest.render("# top doc", "", "[] > foo"),
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
            EoCommentTest.render("[] > foo", "# late", "  bar > @"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'comment is allowed only on top of the file, before metas')]"
            )
        );
    }

    @Test
    void rejectsTopCommentWithoutBlankBelow() {
        MatcherAssert.assertThat(
            "a top comment block not separated from the object by a blank line cannot be accepted, must not linger in the xmir, and must be reported once",
            EoCommentTest.render("# top doc", "[] > foo"),
            XhtmlMatchers.hasXPaths(
                "/object[not(comments)]",
                "/object[count(errors/error[contains(text(),'a blank line must separate the top comment block from the rest of the file')])=1]"
            )
        );
    }

    @Test
    void keepsTopCommentWhenTheSealingLineFails() {
        MatcherAssert.assertThat(
            "a top comment block flushed by a line that then fails must survive that line's rollback",
            EoCommentTest.render("# top doc", "", "  [x] > foo"),
            XhtmlMatchers.hasXPaths(
                "/object/comments/comment[contains(text(),'top doc')]",
                "/object[count(errors/error)=1]"
            )
        );
    }

    @Test
    void keepsAcceptingCommentsAfterTheSealingLineFails() {
        MatcherAssert.assertThat(
            "a line failing after it sealed the header zone cannot turn the next comment line into an error",
            EoCommentTest.render("  [x] > foo", "# top doc", "", "[] > bar"),
            XhtmlMatchers.hasXPaths(
                "/object/comments/comment[contains(text(),'top doc')]",
                "/object[count(errors/error)=1]"
            )
        );
    }

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
