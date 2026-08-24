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
 * Test case for {@link Eo}, focused on the argument-binding rule (R-6.6.2/R-6.6.3):
 * a group of arguments must be all bound or all unbound, and a reversed-dispatch
 * receiver may never carry a binding.
 *
 * @since 0.1
 */
final class EoArgumentBindingsTest {

    @Test
    void acceptsAllUnboundHorizontalArgs() {
        MatcherAssert.assertThat(
            "all-unbound is a valid mode per R-6.6.2 and must parse without errors",
            EoArgumentBindingsTest.render("[] > main", "  foo a b c > x"),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void acceptsAllBoundHorizontalArgs() {
        MatcherAssert.assertThat(
            "all-bound is a valid mode per R-6.6.2 and must parse without errors",
            EoArgumentBindingsTest.render("[] > main", "  foo a:x b:y > z"),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void reportsMixedBindings() {
        MatcherAssert.assertThat(
            "mixed bound and unbound args in the same group must surface R-6.6.2",
            EoArgumentBindingsTest.render("[] > main", "  foo a:x b > z"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'argument bindings must be all-or-nothing')]"
            )
        );
    }

    @Test
    void acceptsReversedReceiverBareAndArgsBound() {
        MatcherAssert.assertThat(
            "the reversed receiver is exempt; the remaining args may be all-bound per R-6.6.2/R-6.6.3",
            EoArgumentBindingsTest.render("[] > main", "  if. cond then:a else:b > z"),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void rejectsBoundReversedReceiver() {
        MatcherAssert.assertThat(
            "a receiver carrying `:x` must surface R-6.6.3",
            EoArgumentBindingsTest.render("[] > main", "  if. cond:x then else > z"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'reversed-dispatch receiver cannot carry a binding')]"
            )
        );
    }

    @Test
    void rejectsMixedBindingsAmongReversedArgs() {
        MatcherAssert.assertThat(
            "the rule still applies to args after the receiver in a reversed dispatch",
            EoArgumentBindingsTest.render("[] > main", "  if. cond then:a else > z"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'argument bindings must be all-or-nothing')]"
            )
        );
    }

    @Test
    void emitsAsForVerticalBindingOnIdentifier() {
        MatcherAssert.assertThat(
            "a vertical-arg line ending with `:label` must emit @as on the line's outermost <o>",
            EoArgumentBindingsTest.render("foo > main", "  bar:tag"),
            XhtmlMatchers.hasXPath("/object/o[@name='main']/o[@base='bar' and @as='tag']")
        );
    }

    @Test
    void emitsAsForVerticalBindingWithSuffix() {
        MatcherAssert.assertThat(
            "the binding may combine with `> name`, both attaching to the same <o>",
            EoArgumentBindingsTest.render("foo > main", "  bar:tag > out"),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='main']/o[@name='out' and @base='bar' and @as='tag']"
            )
        );
    }

    @Test
    void emitsNumericVerticalBindingAsAlpha() {
        MatcherAssert.assertThat(
            "a numeric `:N` binding must become @as='αN' (R-3.12.2)",
            EoArgumentBindingsTest.render("foo > main", "  bar:0"),
            XhtmlMatchers.hasXPath("/object/o[@name='main']/o[@base='bar' and @as='α0']")
        );
    }

    @Test
    void rejectsMixedVerticalBindings() {
        MatcherAssert.assertThat(
            "vertical args under a vapplication must follow R-6.6.2 — mixing bound and unbound is rejected",
            EoArgumentBindingsTest.render("[] > main", "  foo > app", "    a:x", "    b"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'argument bindings must be all-or-nothing')]"
            )
        );
    }

    @Test
    void acceptsAllBoundVerticalArgs() {
        MatcherAssert.assertThat(
            "vertical args may all carry bindings — the uniform pattern is valid",
            EoArgumentBindingsTest.render("[] > main", "  foo > app", "    a:x", "    b:y"),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void rejectsMixedBindingsUnderChainedVerticalMethod() {
        MatcherAssert.assertThat(
            "args of the last link of a same-indent method chain must follow R-6.6.2 too",
            EoArgumentBindingsTest.render(
                "[] > main", "  foo > app", "  .bar", "  .baz", "    a", "    b:y"
            ),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'argument bindings must be all-or-nothing')]"
            )
        );
    }

    @Test
    void acceptsLoneBoundArgUnderVerticalMethod() {
        MatcherAssert.assertThat(
            "a single arg under a vmethod is a uniform group, so its binding is valid",
            EoArgumentBindingsTest.render("[] > main", "  foo > app", "  .bar", "    a:x"),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void rejectsBindingOnFormationChild() {
        MatcherAssert.assertThat(
            "a plain child of a formation cannot carry a binding per R-3.12.3",
            EoArgumentBindingsTest.render("[] > main", "  bar:tag > x"),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'binding allowed only in argument position')]"
            )
        );
    }

    @Test
    void rejectsBindingOnVerticalReceiver() {
        MatcherAssert.assertThat(
            "the receiver of a vertical reversed dispatch cannot carry a binding either",
            EoArgumentBindingsTest.render(
                "foo > main", "  if.", "    cond:x", "    then", "    other"
            ),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'reversed-dispatch receiver cannot carry a binding')]"
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
