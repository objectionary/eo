/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link OnDefault}.
 * @since 0.60
 */
final class OnDefaultTest {

    @Test
    void returnsObjectNameWhenOnlyONamePresent() {
        MatcherAssert.assertThat(
            "We should get object name 'hello'",
            new OnDefault(
                new Xnav(new XMLDocument("<object><o name='hello'/></object>").inner())
            ).get(),
            Matchers.equalTo("hello")
        );
    }

    @Test
    void joinsPackageWithObjectNameWhenPackageMetaPresent() {
        MatcherAssert.assertThat(
            "We should get 'org.example.world' when package meta is present",
            new OnDefault(
                new Xnav(
                    new XMLDocument(
                        String.join(
                            String.format("%n"),
                            "<object>",
                            "    <o name='world'/>",
                            "    <metas>",
                            "      <meta>",
                            "        <head>package</head>",
                            "        <tail>org.example</tail>",
                            "      </meta>",
                            "    </metas>",
                            "</object>"
                        )
                    ).inner()
                )
            ).get(),
            Matchers.equalTo("org.example.world")
        );
    }

    @Test
    void usesClassNameWhenONameMissing() {
        MatcherAssert.assertThat(
            "We should get class name 'Clazz' when o name is missing",
            new OnDefault(
                new Xnav(
                    new XMLDocument(
                        "<object><class name='Clazz'/></object>"
                    ).inner()
                )
            ).get(),
            Matchers.equalTo("Clazz")
        );
    }

    @Test
    void failsWhenMoreThanOneClassNamePresent() {
        MatcherAssert.assertThat(
            "We should fail when more than one '/object/class/@name' is present in XMIR",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new OnDefault(
                    new Xnav(
                        new XMLDocument(
                            "<object><class name='A'/><class name='B'/></object>"
                        ).inner()
                    )
                ).get(),
                "We should fail fast on multiple class names"
            ).getMessage(),
            Matchers.containsString("found 2")
        );
    }

    @Test
    void throwsWhenNeitherONameNorClassNamePresent() {
        MatcherAssert.assertThat(
            "We should fail when neither '/object/o/@name' nor '/object/class/@name' is present in XMIR",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new OnDefault(
                    new Xnav(new XMLDocument("<object></object>").inner())
                ).get(),
                "We should fail fast when no object name can be derived"
            ).getMessage(),
            Matchers.containsString("found 0")
        );
    }

    @Test
    void ignoresNonPackageMetaAndReturnsObjectName() {
        MatcherAssert.assertThat(
            "We should get object name 'o' when non-package meta is present",
            new OnDefault(
                new Xnav(
                    new XMLDocument(
                        String.join(
                            String.format("%n"),
                            "<object>",
                            "    <o name='o'/>",
                            "    <metas>",
                            "      <meta>",
                            "        <head>author</head>",
                            "        <tail>me</tail>",
                            "      </meta>",
                            "    </metas>",
                            "</object>"
                        )
                    ).inner()
                )
            ).get(),
            Matchers.equalTo("o")
        );
    }

    @Test
    void doesNotUsePackageMetaWithoutTail() {
        MatcherAssert.assertThat(
            "We should get object name 'o' when package meta has no tail",
            new OnDefault(
                new Xnav(
                    new XMLDocument(
                        String.join(
                            String.format("%n"),
                            "<object>",
                            "    <o name='x'/>",
                            "    <metas>",
                            "      <meta>",
                            "        <head>package</head>",
                            "      </meta>",
                            "    </metas>",
                            "</object>"
                        )
                    ).inner()
                )
            ).get(),
            Matchers.equalTo("x")
        );
    }
}
