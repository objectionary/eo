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
            new EoSyntax("[] > foo").parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        final Path member = temp.resolve("member.xmir");
        Files.write(
            member,
            new EoSyntax("[] > bar").parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        final Path merge = temp.resolve("4-merge");
        final Path target = new Place("foo").make(merge, MjAssemble.XMIR);
        this.merge(pkg, member, merge);
        final FileTime before = Files.getLastModifiedTime(target);
        Thread.sleep(1_100L);
        this.merge(pkg, member, merge);
        MatcherAssert.assertThat(
            "Merged XMIR should not be rewritten when its content hasn't changed",
            Files.getLastModifiedTime(target),
            Matchers.equalTo(before)
        );
    }

    private void merge(
        final Path pkg, final Path member, final Path merge
    ) throws IOException {
        final TjsForeign tojos = new TjsForeign();
        tojos.add("foo").withXmir(pkg);
        tojos.add("foo.bar").withXmir(member);
        new Merging(tojos, merge).exec();
    }
}
