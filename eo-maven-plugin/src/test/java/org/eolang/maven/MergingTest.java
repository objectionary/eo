/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link Merging}.
 *
 * @since 0.68.0
 */
final class MergingTest {

    @Test
    void writesTheMergedXmirOnlyWhenItsContentChanges(@TempDir final Path temp) throws Exception {
        final Path pkg = temp.resolve("pkg.xmir");
        Files.write(
            pkg,
            new EoSyntax(String.format("[] > foo%n  true ++> works"))
                .parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        final Path member = temp.resolve("member.xmir");
        Files.write(
            member,
            new EoSyntax(String.format("[] > bar%n  true --> works"))
                .parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        final Path target = new Place("foo").make(
            new Subdir(temp, "merge").path(), MjAssemble.XMIR
        );
        this.merge(pkg, member, temp);
        final FileTime before = Files.getLastModifiedTime(target);
        Thread.sleep(1_100L);
        this.merge(pkg, member, temp);
        MatcherAssert.assertThat(
            "Merged XMIR should not be rewritten when its content hasn't changed",
            Files.getLastModifiedTime(target),
            Matchers.equalTo(before)
        );
    }

    private void merge(
        final Path pkg, final Path member, final Path base
    ) throws IOException {
        final TjsForeign tojos = new TjsForeign();
        tojos.add("foo").withXmir(pkg);
        tojos.add("foo.bar").withXmir(member);
        new Merging(tojos, base).exec();
    }
}
