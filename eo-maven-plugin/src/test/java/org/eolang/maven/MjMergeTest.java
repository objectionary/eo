/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link MjMerge}.
 * @since 0.68.0
 */
@ExtendWith(MktmpResolver.class)
final class MjMergeTest {

    @Test
    void putsAMemberIntoTheObjectOfItsPackage(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the member must arrive as an attribute of the object named by its package",
            new XMLDocument(
                MjMergeTest.merged(temp, "foo").targetPath().resolve("4-merge/foo.xmir")
            ),
            XhtmlMatchers.hasXPath("/object/o[@name='foo']/o[@name='bar']")
        );
    }

    @Test
    void keepsTheAttributesTheObjectAlreadyHad(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a member arrives after the voids, so the order of the arguments cannot shift",
            new XMLDocument(
                MjMergeTest.merged(temp, "foo").targetPath().resolve("4-merge/foo.xmir")
            ),
            XhtmlMatchers.hasXPath(
                "/object/o[@name='foo']/o[@name='n']/following-sibling::o[@name='bar']"
            )
        );
    }

    @Test
    void pointsTheObjectAtTheMergedXmir(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the object must be transpiled from the merged XMIR and not from the parsed one",
            MjMergeTest.merged(temp, "foo").foreignTojos().find("foo").xmir().toString(),
            Matchers.endsWith(Paths.get("4-merge/foo.xmir").toString())
        );
    }

    @Test
    void takesTheMergedMemberAwayFromTranspiling(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a member that now lives inside its package object must not be transpiled apart",
            MjMergeTest.merged(temp, "foo")
                .execute(MjTranspile.class)
                .result()
                .keySet(),
            Matchers.not(Matchers.hasItem(Matchers.containsString("EObar.java")))
        );
    }

    @Test
    void keepsTranspilingTheObjectTheMembersWentInto(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the object that now holds its members must still be compiled",
            MjMergeTest.merged(temp, "foo")
                .execute(MjTranspile.class)
                .result()
                .keySet(),
            Matchers.hasItem(Matchers.containsString("EOfoo.java"))
        );
    }

    @Test
    void leavesEveryPackageAloneByDefault(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "no package is merged until one is named, so nothing may be written",
            Files.exists(MjMergeTest.merged(temp).targetPath().resolve("4-merge")),
            Matchers.is(false)
        );
    }

    @Test
    void refusesToMergeAMemberOverAnExistingAttribute(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a name held by both the object and its member must stop the build and name both",
            MjMergeTest.root(
                Assertions.assertThrows(
                    IllegalStateException.class,
                    () -> new FakeMaven(temp).withProgram(
                        MjMergeTest.program("[n] > foo", "  42 > bar", "  n > @"),
                        "foo",
                        "foo.eo"
                    ).withProgram(
                        MjMergeTest.program("+package foo", "", "[] > bar", "  42 > @"),
                        "foo.bar",
                        "foo/bar.eo"
                    ).with("mergedPackages", Collections.singletonList("foo"))
                        .execute(new FakeMaven.Merge())
                )
            ),
            Matchers.stringContainsInOrder("bar", "foo")
        );
    }

    @Test
    void refusesToMergeAPackageWithoutAnObject(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a package named for merging with no object of its own must not pass silently",
            MjMergeTest.root(
                Assertions.assertThrows(
                    IllegalStateException.class,
                    () -> new FakeMaven(temp).withProgram(
                        MjMergeTest.program("+package foo", "", "[] > bar", "  42 > @"),
                        "foo.bar",
                        "foo/bar.eo"
                    ).with("mergedPackages", Collections.singletonList("foo"))
                        .execute(new FakeMaven.Merge())
                )
            ),
            Matchers.containsString("foo")
        );
    }

    /**
     * A workspace holding the object {@code foo} with a void and the member
     * {@code foo.bar}, taken through parsing and merging.
     * @param temp The temporary directory
     * @param packages The packages to merge
     * @return The workspace, after the merge
     * @throws Exception If the pipeline fails
     */
    private static FakeMaven merged(final Path temp, final String... packages) throws Exception {
        return new FakeMaven(temp).withProgram(
            MjMergeTest.program("[n] > foo", "  n > @"),
            "foo",
            "foo.eo"
        ).withProgram(
            MjMergeTest.program("+package foo", "", "[] > bar", "  42 > @"),
            "foo.bar",
            "foo/bar.eo"
        ).with("mergedPackages", Arrays.asList(packages))
            .execute(new FakeMaven.Merge());
    }

    /**
     * The message of the deepest cause, which is where a mojo failure keeps
     * what actually went wrong.
     * @param thrown What the mojo threw
     * @return The message
     */
    private static String root(final Throwable thrown) {
        Throwable cause = thrown;
        while (cause.getCause() != null) {
            cause = cause.getCause();
        }
        return cause.getMessage();
    }

    /**
     * The lines of an EO program as one text.
     * @param lines The lines
     * @return The program
     */
    private static String program(final String... lines) {
        return String.join(System.lineSeparator(), lines);
    }
}
