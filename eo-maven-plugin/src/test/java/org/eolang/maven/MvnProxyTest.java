/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link MvnProxy}.
 * @since 0.73.4
 */
final class MvnProxyTest {

    @Test
    void excludesAHostListedInNonProxyHosts() {
        final org.apache.maven.settings.Proxy origin = new org.apache.maven.settings.Proxy();
        origin.setHost("prox.eolang.org");
        origin.setPort(8080);
        origin.setNonProxyHosts("localhost|*.internal.example.com");
        MatcherAssert.assertThat(
            "A host matching a nonProxyHosts pattern must be excluded",
            new MvnProxy(origin).excludes("build.internal.example.com"),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotExcludeAHostAbsentFromNonProxyHosts() {
        final org.apache.maven.settings.Proxy origin = new org.apache.maven.settings.Proxy();
        origin.setHost("prox.eolang.org");
        origin.setPort(8080);
        origin.setNonProxyHosts("localhost");
        MatcherAssert.assertThat(
            "A host not matching any nonProxyHosts pattern must not be excluded",
            new MvnProxy(origin).excludes("raw.githubusercontent.com"),
            Matchers.is(false)
        );
    }
}
