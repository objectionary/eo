/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Optional;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link OnClasspath}.
 * @since 0.62
 */
final class OnClasspathTest {

    @Test
    void findsExistingClass() {
        MatcherAssert.assertThat(
            "A class that exists must be reported as present, but it wasn't",
            OnClasspath.has("org.eolang.PhDefault"),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotFindMissingClass() {
        MatcherAssert.assertThat(
            "A class that is absent must be reported as missing, but it wasn't",
            OnClasspath.has("org.eolang.ThereIsDefinitelyNoSuchClass"),
            Matchers.is(false)
        );
    }

    @Test
    @SuppressWarnings("PMD.AvoidAccessibilityAlteration")
    void returnsCachedAnswerInsteadOfProbingClasspathAgain() throws ReflectiveOperationException {
        final String cls = "org.eolang.OnClasspathTest$CacheProbe";
        OnClasspath.has(cls);
        final Field field = OnClasspath.class.getDeclaredField("CACHE");
        field.setAccessible(true);
        @SuppressWarnings("unchecked")
        final Map<String, Boolean> cache = (Map<String, Boolean>) field.get(null);
        cache.put(cls, false);
        try {
            MatcherAssert.assertThat(
                "The cached answer must be returned instead of probing the classpath again",
                OnClasspath.has(cls),
                Matchers.is(false)
            );
        } finally {
            cache.put(cls, true);
        }
    }

    @Test
    void findsNestedClassOnClasspath() {
        MatcherAssert.assertThat(
            "A nested class that exists must be reported as present, but it wasn't",
            OnClasspath.has("org.eolang.OnClasspathTest$Marking"),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotRunClassInitializerWhileProbing() {
        System.clearProperty("eo.onclasspath.marked");
        OnClasspath.has("org.eolang.OnClasspathTest$Marking");
        MatcherAssert.assertThat(
            "Probing a class must not run its initializer, but it did",
            Optional.ofNullable(System.getProperty("eo.onclasspath.marked")).isPresent(),
            Matchers.is(false)
        );
    }

    /**
     * A class whose initializer marks a system property, so that a test can tell
     * whether merely probing for the class was enough to initialize it.
     *
     * <p>The mark lands outside the class on purpose: reading a field of this very
     * class would itself initialize it, which is the thing being detected.</p>
     *
     * @since 0.74.0
     */
    final class Marking {

        static {
            System.setProperty("eo.onclasspath.marked", "true");
        }
    }

    /**
     * A class used only to give the caching test a cache entry of its own, so
     * tampering with it cannot leak into other tests.
     * @since 0.74.0
     */
    final class CacheProbe {
    }
}
