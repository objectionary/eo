/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.util.stream.Collectors;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link MjSafe}.
 *
 * @since 0.1
 */
final class MjSafeTest {

    @Test
    void failsBuildWhenUnrollExitErrorIsTrue() {
        final MjSafeTest.Failing mojo = new MjSafeTest.Failing();
        mojo.unroll = true;
        Assertions.assertThrows(
            MojoFailureException.class,
            mojo::execute,
            "Build must fail when the wrapped Mojo throws and unroll is true"
        );
    }

    @Test
    void failsBuildWhenUnrollExitErrorIsFalse() {
        final MjSafeTest.Failing mojo = new MjSafeTest.Failing();
        mojo.unroll = false;
        Assertions.assertThrows(
            MojoFailureException.class,
            mojo::execute,
            "Build must still fail when the wrapped Mojo throws, even if unroll is false"
        );
    }

    @Test
    void computesHashFromTagInjectedAfterConstruction() {
        final MjSafeTest.Failing mojo = new MjSafeTest.Failing();
        final String hex = "abcdefabcdefabcdefabcdefabcdefabcdefabcd";
        mojo.tag = hex;
        Assertions.assertEquals(
            hex.substring(0, 7),
            mojo.hash.value(),
            "hash must be computed from the tag Maven injects after construction, not from the constructor-time default"
        );
    }

    @Test
    void namesEveryParameterThatOnlyOneGoalDeclaresAfterThatGoal() throws Exception {
        final XML plugin = new XMLDocument(
            new TextOf(new ResourceOf("META-INF/maven/plugin.xml")).asString()
        );
        final String mojos = "//mojo[starts-with(implementation, 'org.eolang.maven.Mj')]";
        MatcherAssert.assertThat(
            "a parameter that only one goal declares must be named '<goal>.<field>' and read from '${eo.<goal>.<field>}', but these are not",
            plugin.nodes(String.format("%s/parameters/parameter", mojos)).stream().filter(
                param -> plugin.nodes(
                    String.format(
                        "%s/parameters/parameter[name='%s' and string(alias)='%s']",
                        mojos,
                        param.xpath("name/text()").get(0),
                        String.join("", param.xpath("alias/text()"))
                    )
                ).size() == 1
            ).filter(
                param -> param.nodes(
                    "self::*[alias=concat(ancestor::mojo/goal, '.', name)]"
                ).isEmpty()
                    || !param.nodes(
                        String.format(
                            "ancestor::mojo/configuration/*[name()='%s' and text()!=concat('${eo.', ancestor::mojo/goal, '.%1$s}')]",
                            param.xpath("name/text()").get(0)
                        )
                    ).isEmpty()
            ).map(
                param -> String.format(
                    "%s:%s",
                    param.xpath("ancestor::mojo/goal/text()").get(0),
                    param.xpath("name/text()").get(0)
                )
            ).collect(Collectors.toList()),
            Matchers.empty()
        );
    }

    /**
     * The mojo whose exec() always fails.
     *
     * @since 0.1
     */
    @Mojo(name = "failing", defaultPhase = LifecyclePhase.VALIDATE)
    private static final class Failing extends MjSafe {

        @Override
        public void exec() throws IOException {
            throw new IOException("intentional failure for MjSafeTest");
        }
    }
}
