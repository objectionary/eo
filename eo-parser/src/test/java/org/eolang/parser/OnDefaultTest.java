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

/**
 * Test cases for {@link OnDefault}.
 *
 * @since 0.60
 */
final class OnDefaultTest {

    @Test
    void returnsObjectNameWhenOnlyONamePresent() {
        MatcherAssert.assertThat(
            "We should get object name 'hello'",
            new OnDefault(new XMLDocument("<object><o name='hello'/></object>")).get(),
            Matchers.equalTo("hello")
        );
    }

    @Test
    void joinsPackageWithObjectNameWhenPackageMetaPresent() {
        MatcherAssert.assertThat(
            "We should get 'org.example.world' when package meta is present",
            new OnDefault(
                new XMLDocument(
                    String.join(
                        "\n",
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
                new XMLDocument(
                    "<object><class name='Clazz'/></object>"
                )
            ).get(),
            Matchers.equalTo("Clazz")
        );
    }

    @Test
    @SuppressWarnings("PMD.UnitTestContainsTooManyAsserts")
    void throwsWhenNeitherONameNorClassNamePresent() {
        MatcherAssert.assertThat(
            "Expecting exception when neither o name nor class name present",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new OnDefault(new XMLDocument("<object></object>")).get(),
                "We should throw IllegalStateException"
            ).getMessage(),
            Matchers.equalTo(
                "XMIR should have either '/object/o/@name' or '/object/class/@name' attribute"
            )
        );
    }

    @Test
    void ignoresNonPackageMetaAndReturnsObjectName() {
        MatcherAssert.assertThat(
            "We should get object name 'o' when non-package meta is present",
            new OnDefault(
                new XMLDocument(
                    String.join(
                        "\n",
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
                new XMLDocument(
                    String.join(
                        "\n",
                        "<object>",
                        "    <o name='x'/>",
                        "    <metas>",
                        "      <meta>",
                        "        <head>package</head>",
                        "      </meta>",
                        "    </metas>",
                        "</object>"
                    )
                )
            ).get(),
            Matchers.equalTo("x")
        );
    }
}
