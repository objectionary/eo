/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.cactoos.set.SetOf;
import org.eolang.cache.Saved;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.aggregator.ArgumentsAccessor;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test cases for {@link Unspiling}.
 *
 * @since 0.61.0
 */
final class UnspilingTest {

    @ParameterizedTest
    @CsvSource({
        "EOapp.java, EOmine$1.class, , EOmine.class;EOmine$1.class",
        "EOapp.java, EOmine$1$2.class, , EOmine.class;EOmine$1$2.class",
        "EOapp.java, EOmine$Nested.class, , EOmine.class;EOmine$Nested.class",
        "EOapp.java, EOmine$EOΦinner.class, , EOmine.class;EOmine$EOΦinner.class",
        "EOapp.java, EOapplication$1.class, , EOmine.class;EOapplication$1.class",
        "EOapp.java, EOapp$1.class, , EOmine.class",
        "EOapp.java, EOapp$1$2$3.class, , EOmine.class",
        "EOapp.java, EOapp$EOmine.class, , EOmine.class;EOapp$EOmine.class",
        "EOapp.java, EOapp$EOmine$1.class, , EOmine.class;EOapp$EOmine$1.class",
        "EOapp.java, EOapp$EOmine$EOΦinner.class, , EOmine.class;EOapp$EOmine$EOΦinner.class",
        "EOapp.java, EOapp$EOΦinner.class, , EOmine.class",
        "EOapp.java, EOapp$1.class, **/*$1.class, EOmine.class;EOapp$1.class",
        "EOapp.java, EOapp$1.class, **/EOapp.class, EOmine.class;EOapp.class",
        "EOapp.java, EOapp$1.class, **/EOapp*, EOmine.class;EOapp.class;EOapp$1.class",
        ", EOmine$1.class, , EOapp.class;EOmine.class;EOmine$1.class",
        "'', EOmine$EOΦinner.class, , EOapp.class;EOmine.class;EOmine$EOΦinner.class",
        "EOapp.txt, EOapp$1.class, , EOapp.class;EOmine.class;EOapp$1.class"
    })
    void deletesOnlyClassesOwnedByGeneratedSources(
        final ArgumentsAccessor row, @TempDir final Path temp
    ) throws IOException {
        final Path generated = temp.resolve("generated");
        final Path classes = temp.resolve("classes");
        final String pkg = "EOorg/EOeolang/";
        final String source = row.getString(0);
        if (source != null) {
            Files.createDirectories(generated);
            if (!source.isEmpty()) {
                new Saved("generated", generated.resolve(pkg + source)).value();
            }
        }
        for (final String name : Set.of("EOapp.class", "EOmine.class", row.getString(1))) {
            new Saved("binary", classes.resolve(pkg + name)).value();
        }
        new Unspiling(
            generated, classes,
            Optional.ofNullable(row.getString(2)).stream().collect(Collectors.toSet())
        ).exec();
        Assertions.assertEquals(
            new SetOf<>(row.getString(3).split(";")),
            new WkDefault(classes).stream()
                .map(path -> path.getFileName().toString()).collect(Collectors.toSet()),
            "Only generated classes outside keepBinaries should be deleted"
        );
    }

    @Test
    void skipsWhenNoClassesExist(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new Unspiling(
                temp.resolve("generated"),
                temp.resolve("classes"),
                new SetOf<>()
            ).exec(),
            "Unspiling must skip gracefully when the classes directory is empty"
        );
    }
}
