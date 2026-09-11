/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Arrays;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Phino}.
 *
 * <p>The tests that run the real binary hold only when it is installed
 * and of the pinned version, which is what CI arranges; a machine
 * without it skips them, the same way the goal that uses this class
 * skips its work.</p>
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class PhinoTest {

    @Test
    void readsPin(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the pinned version must come from the phino-version.txt resource, but it didnt",
            new Phino("phino", 7, temp).pin(),
            Matchers.matchesPattern("\\d+\\.\\d+\\.\\d+")
        );
    }

    @Test
    void distrustsAbsentBinary(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "an executable that is not there cannot be suitable, but it was",
            new Phino(temp.resolve("no-such-phino").toString(), 7, temp).suitable(),
            Matchers.is(false)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void distrustsWrongVersion(@Mktmp final Path temp) throws Exception {
        final Path fake = temp.resolve("phino");
        Files.write(
            fake,
            String.format("#!/bin/sh%necho 9.9.9%n").getBytes(StandardCharsets.UTF_8)
        );
        Files.setPosixFilePermissions(
            fake, PosixFilePermissions.fromString("rwxr-xr-x")
        );
        MatcherAssert.assertThat(
            "an executable of another version cannot be suitable, but it was",
            new Phino(fake.toString(), 7, temp).suitable(),
            Matchers.is(false)
        );
    }

    @Test
    void mergesXmirDocumentsIntoOneExpression(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 100, temp);
        Assumptions.assumeTrue(phino.suitable());
        final Path first = temp.resolve("a.xmir");
        Files.write(
            first,
            "<object><metas><meta><head>package</head><tail>foo</tail></meta></metas><o name=\"a\"><o base=\"Φ.bytes\" name=\"φ\"><o as=\"α0\">2A-</o></o></o></object>"
                .getBytes(StandardCharsets.UTF_8)
        );
        final Path second = temp.resolve("b.xmir");
        Files.write(
            second,
            "<object><metas><meta><head>package</head><tail>foo</tail></meta></metas><o name=\"b\"><o name=\"x\"/><o base=\"ξ.ρ.a\" name=\"φ\"/></o></object>"
                .getBytes(StandardCharsets.UTF_8)
        );
        final Path world = temp.resolve("world.phi");
        phino.merged(Arrays.asList(first, second), world);
        MatcherAssert.assertThat(
            "both documents must land under the same package of the universe, but they didnt",
            Files.readString(world, StandardCharsets.UTF_8).replaceAll("\\s+", " "),
            Matchers.allOf(Matchers.containsString("a ↦"), Matchers.containsString("b ↦"))
        );
    }

    @Test
    void morphsInsideOneObjectToXmir(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 100, temp);
        Assumptions.assumeTrue(phino.suitable());
        final Path world = temp.resolve("world.phi");
        Files.write(
            world,
            "⟦ foo ↦ ⟦ gap ↦ ⟦ x ↦ ∅, φ ↦ ξ.x ⟧, λ ⤍ Package ⟧ ⟧".getBytes(StandardCharsets.UTF_8)
        );
        final Path registry = temp.resolve("atoms.json");
        Files.write(registry, "{}".getBytes(StandardCharsets.UTF_8));
        MatcherAssert.assertThat(
            "the residual of the object must come back as an XMIR object, but it didnt",
            phino.morphed(world, "Φ.foo.gap", registry),
            Matchers.allOf(
                Matchers.containsString("<object "),
                Matchers.containsString("base=\"ξ.x\"")
            )
        );
    }

    @Test
    void refusesFailingRun(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 100, temp);
        Assumptions.assumeTrue(phino.suitable());
        final Path world = temp.resolve("world.phi");
        Files.write(world, "this is not phi".getBytes(StandardCharsets.UTF_8));
        final Path registry = temp.resolve("atoms.json");
        Files.write(registry, "{}".getBytes(StandardCharsets.UTF_8));
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> phino.morphed(world, "Φ.foo", registry),
            "a run the binary rejects must fail loudly, but it didnt"
        );
    }
}
