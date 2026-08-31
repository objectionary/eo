/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.stream.Stream;
import org.cactoos.BiProc;

/**
 * Placed Java generated code.
 * @since 0.56.7
 */
final class JavaPlaced implements BiProc<Xnav, Boolean> {

    /**
     * The footprint.
     */
    private final Footprint footprint;

    /**
     * The target path.
     */
    private final Path target;

    /**
     * Generated sources dir.
     */
    private final Path generated;

    /**
     * Ctor.
     * @param ftprnt The footprint
     * @param tgt The target path
     * @param gen Generated sources dir
     */
    JavaPlaced(final Footprint ftprnt, final Path tgt, final Path gen) {
        this.footprint = ftprnt;
        this.target = tgt;
        this.generated = gen;
    }

    @Override
    public void exec(final Xnav clazz, final Boolean tests) throws IOException {
        if (clazz.element("java").text().isPresent()) {
            this.footprint.apply(Paths.get(""), this.target);
        }
        if (tests && JavaPlaced.testsPresent(clazz)) {
            this.placeJavaTests(clazz);
        } else {
            this.removeJavaTests(clazz);
        }
    }

    private void placeJavaTests(final Xnav clazz) throws IOException {
        final String[] jparts = clazz.attribute("java-name").text().get().split("\\.");
        final Path tests = this.generated.getParent().resolve(
            "generated-test-sources"
        );
        final Path base = Arrays.stream(jparts, 0, jparts.length - 1).reduce(
            tests,
            Path::resolve,
            Path::resolve
            );
        final String origin = String.format("Test%s.java", jparts[jparts.length - 1]);
        final Path resolved = base.resolve(origin);
        final Path resulted;
        final String content;
        if (
            Files.exists(
                tests.getParent().getParent().resolve("src").resolve("test")
                    .resolve("java")
                    .resolve(tests.relativize(resolved))
            )
        ) {
            final String atomized = String.format(
                "TestAtom%s.java", jparts[jparts.length - 1]
            );
            resulted = base.resolve(atomized);
            content = clazz.element("tests").text().get().replace(
                origin.replace(".java", ""), atomized.replace(".java", "")
            );
        } else {
            resulted = resolved;
            content = clazz.element("tests").text().get();
        }
        new Saved(content, resulted).value();
        this.removeJavaTests(clazz, resulted);
    }

    private void removeJavaTests(final Xnav clazz) throws IOException {
        this.removeJavaTests(clazz, null);
    }

    private void removeJavaTests(final Xnav clazz, final Path retained) throws IOException {
        final String[] parts = clazz.attribute("java-name").text().get().split("\\.");
        final Path base = Arrays.stream(parts, 0, parts.length - 1).reduce(
            this.generated.getParent().resolve("generated-test-sources"),
            Path::resolve,
            Path::resolve
        );
        final String name = parts[parts.length - 1];
        for (final String mark : new String[]{"Test", "TestAtom"}) {
            final Path test = base.resolve(String.format("%s%s.java", mark, name));
            if (!test.equals(retained)) {
                Files.deleteIfExists(test);
            }
        }
    }

    private static boolean testsPresent(final Xnav clazz) {
        return clazz.element("tests").text().map(
            s -> Stream.of(
                "@Test", "@ParameterizedTest", "@RepeatedTest", "@TestFactory", "@TestTemplate"
            ).anyMatch(s::contains)
        ).orElse(false);
    }
}
