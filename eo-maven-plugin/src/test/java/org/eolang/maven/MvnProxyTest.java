/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.apache.maven.settings.Proxy;
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
        final Proxy origin = new Proxy();
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
        final Proxy origin = new Proxy();
        origin.setHost("prox.eolang.org");
        origin.setPort(8080);
        origin.setNonProxyHosts("localhost");
        MatcherAssert.assertThat(
            "A host not matching any nonProxyHosts pattern must not be excluded",
            new MvnProxy(origin).excludes("raw.githubusercontent.com"),
            Matchers.is(false)
        );
    }

    @Test
    void excludesAHostSpelledInCapitals() {
        final Proxy origin = new Proxy();
        origin.setHost("prox.eolang.org");
        origin.setPort(8080);
        origin.setNonProxyHosts("*.internal.example.com");
        MatcherAssert.assertThat(
            "A host name is case-insensitive, so a capitalised one matching a nonProxyHosts pattern cannot be sent through the proxy",
            new MvnProxy(origin).excludes("BUILD.Internal.EXAMPLE.com"),
            Matchers.is(true)
        );
    }

    @Test
    void answersTheChallengeWithTheCredentialsOfTheSettings() {
        final Proxy origin = new Proxy();
        origin.setHost("prox.eolang.org");
        origin.setPort(3128);
        origin.setUsername("jeff");
        origin.setPassword("secret");
        MatcherAssert.assertThat(
            "a proxy that wants a name and a password must have something to answer with",
            new MvnProxy(origin).credentials().isPresent(),
            Matchers.is(true)
        );
    }

    @Test
    void saysNothingWhenTheSettingsCarryNoCredentials() {
        final Proxy origin = new Proxy();
        origin.setHost("prox.eolang.org");
        origin.setPort(3128);
        MatcherAssert.assertThat(
            "a proxy that wants nothing must be left alone, but it was given an authenticator",
            new MvnProxy(origin).credentials().isPresent(),
            Matchers.is(false)
        );
    }
}
