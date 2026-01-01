/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.security.SecureRandom;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Random;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * List of dependencies.
 * @since 0.54
 */
interface Dependencies extends Iterable<Dep> {
    /**
     * Fake dependencies.
     * @since 0.54.0
     */
    final class Fake implements Dependencies {

        /**
         * Dependencies.
         */
        private final Collection<Dep> dependencies;

        /**
         * Ctor.
         */
        Fake() {
            this(
                Dependencies.Fake.randDep(),
                Dependencies.Fake.randDep(),
                Dependencies.Fake.randDep()
            );
        }

        /**
         * Ctor.
         * @param size Number of fake dependencies.
         */
        Fake(final int size) {
            this(
                Stream.generate(Dependencies.Fake::randDep)
                    .limit(size)
                    .collect(Collectors.toList())
            );
        }

        /**
         * Ctor.
         * @param deps Dependencies.
         */
        Fake(final Dep... deps) {
            this(Arrays.asList(deps));
        }

        /**
         * Ctor.
         * @param deps Dependencies.
         */
        private Fake(final Collection<Dep> deps) {
            this.dependencies = deps;
        }

        @Override
        public Iterator<Dep> iterator() {
            return this.dependencies.iterator();
        }

        /**
         * Create a random dependency with specified scope.
         * @param scope Scope.
         * @return Dependency.
         */
        static Dep randDep(final String scope) {
            return Dependencies.Fake.dep(
                UUID.randomUUID().toString(),
                UUID.randomUUID().toString(),
                String.valueOf(new SecureRandom().nextInt(Integer.MAX_VALUE)),
                scope
            );
        }

        /**
         * Create a eo-runtime dependency.
         * @return Dependency.
         */
        static Dep runtimeDep() {
            return Dependencies.Fake.dep(
                "org.eolang",
                "eo-runtime",
                "0.30.0",
                "compiled"
            );
        }

        /**
         * Create a random dependency.
         * @return Dependency.
         */
        private static Dep randDep() {
            final Random rand = new SecureRandom();
            final String scope = new String[] {
                "test", "compiled", "runtime",
            }[rand.nextInt(3)];
            return Dependencies.Fake.dep(
                UUID.randomUUID().toString(),
                UUID.randomUUID().toString(),
                String.valueOf(rand.nextInt(Integer.MAX_VALUE)),
                scope
            );
        }

        /**
         * Create a dependency.
         * @param group Group ID.
         * @param artifact Artifact ID.
         * @param version Version.
         * @param scope Scope.
         * @return Dependency.
         * @checkstyle ParameterNumberCheck (5 lines)
         */
        @SuppressWarnings("PMD.UseObjectForClearerAPI")
        private static Dep dep(
            final String group,
            final String artifact,
            final String version,
            final String scope
        ) {
            return new Dep()
                .withGroupId(group)
                .withArtifactId(artifact)
                .withVersion(version)
                .withScope(scope);
        }
    }
}
