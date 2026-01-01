/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.ImpossibleModificationException;
import org.xembly.Xembler;

/**
 * Tests for {@link OnDefault}.
 *
 * @since 0.56.1
 */
final class ObjectNameTest {

    @Test
    void retrievesSimpleName() throws ImpossibleModificationException {
        final String expected = "foo";
        final String retrieved = new OnDefault(
            new XMLDocument(
                new Xembler(
                    new Directives().add("object").add("o").attr("name", expected)
                ).xml()
            )
        ).get();
        MatcherAssert.assertThat(
            String.format(
                "Retrieved name '%s' does not match with expected: '%s'",
                retrieved, expected
            ),
            retrieved,
            Matchers.equalTo(expected)
        );
    }

    @Test
    void retrievesPackagedName() throws ImpossibleModificationException {
        final String expected = "org.eolang.f.foo";
        final String retrieved = new OnDefault(
            new XMLDocument(
                new Xembler(
                    new Directives().add("object")
                        .add("o").attr("name", "foo")
                        .up()
                        .add("metas")
                        .add("meta")
                        .add("head")
                        .set("package")
                        .up()
                        .add("tail")
                        .set("org.eolang.f")
                ).xml()
            )
        ).get();
        MatcherAssert.assertThat(
            String.format(
                "Retrieved name '%s' does not match with expected: '%s'",
                retrieved, expected
            ),
            retrieved,
            Matchers.equalTo(expected)
        );
    }

    @Test
    void throwsExceptionWhenNameIsMissing() {
        Assertions.assertThrows(
            Exception.class,
            () -> new OnDefault(
                new XMLDocument(
                    new Xembler(
                        new Directives().add("object")
                            .add("metas")
                            .add("meta")
                            .add("head")
                            .set("package")
                            .up()
                            .add("tail")
                            .set("org.eolang.fail")
                    ).xml()
                )
            ).get(),
            "The exception is not thrown, thought XMIR is broken"
        );
    }

    @Test
    void doesNotThrowExceptionWhenNameIsPresentButPackageIsMissing() {
        Assertions.assertDoesNotThrow(
            () -> new OnDefault(
                new XMLDocument(
                    new Xembler(new Directives().add("object").add("o").attr("name", "foo")).xml()
                )
            ).get(),
            "The exception was thrown, but it should not"
        );
    }
}
