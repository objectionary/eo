/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link MjMerge}.
 *
 * <p>What the merge does to a program is described by the packs in
 * {@code merge-packs}: each one carries the files of a program, the packages to
 * merge, and the XPaths the merged XMIR must satisfy, plus the text the
 * generated Java must hold, both of the object and of its tests, so the
 * behaviour is stated as the program a human would write rather than as XMIR
 * by hand. What is left here are the mechanics
 * no EO source can express: where the merged XMIR is pointed to from, what
 * stops being compiled apart, and what the mojo refuses to do at all.</p>
 *
 * @since 0.68.0
 */
@ExtendWith(MktmpResolver.class)
final class MjMergeTest {

    /**
     * Temp directory, injected into every test instance, since a parameterized
     * test cannot also take one as an argument.
     */
    @Mktmp
    private Path dir;

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/merge-packs/", glob = "**.yaml")
    void mergesTheProgramOfAPack(final String yaml) throws IOException {
        final Xtory pack = new XtSticky(new XtYaml(yaml));
        MatcherAssert.assertThat(
            "every XPath and every text of the pack must match what the merge wrote, but some didnt",
            MjMergeTest.unmatched(pack, MjMergeTest.spliced(pack, this.dir)),
            Matchers.empty()
        );
    }

    @Test
    void pointsTheObjectAtTheMergedXmir(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the object must be transpiled from the merged XMIR and not from the parsed one",
            MjMergeTest.merged(temp).foreignTojos().find("foo").xmir().toString(),
            Matchers.endsWith(Paths.get("4-merge/foo.xmir").toString())
        );
    }

    @Test
    void takesTheMergedMemberAwayFromTranspiling(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a member that now lives inside its package object must not be transpiled apart",
            MjMergeTest.merged(temp)
                .execute(MjTranspile.class)
                .result()
                .keySet(),
            Matchers.not(Matchers.hasItem(Matchers.containsString("EObar.java")))
        );
    }

    @Test
    void leavesAMergedMemberAloneWhenMergedAgain(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a member already inside its object must not arrive there a second time, since MjTranspile merges too",
            new XMLDocument(
                MjMergeTest.merged(temp)
                    .execute(MjMerge.class)
                    .foreignTojos()
                    .find("foo")
                    .xmir()
            ).nodes("/object/o[@name='foo']/o[@name='bar']").size(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void leavesAPackageWithoutAnObjectAlone(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a package no object is named after has nothing to merge into, so nothing may be written",
            Files.exists(
                new FakeMaven(temp).withProgram(
                    MjMergeTest.program("+package foo", "", "[] > bar", "  42 > @"),
                    "foo.bar",
                    "foo/bar.eo"
                ).execute(new PpMerge()).targetPath().resolve(Merging.DIR)
            ),
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
                    ).execute(new PpMerge())
                )
            ),
            Matchers.stringContainsInOrder("bar", "foo")
        );
    }

    @Test
    void refusesToLiftOneNameOutOfTwoMembers(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "two members declaring one name for their tests must stop the build and name it",
            MjMergeTest.root(
                Assertions.assertThrows(
                    IllegalStateException.class,
                    () -> new FakeMaven(temp).withProgram(
                        MjMergeTest.program("[n] > foo", "  n > @"),
                        "foo",
                        "foo.eo"
                    ).withProgram(
                        MjMergeTest.program(
                            "+package foo", "", "[] > bar", "  42 > @", "  1.eq 1 ++> can-be-one"
                        ),
                        "foo.bar",
                        "foo/bar.eo"
                    ).withProgram(
                        MjMergeTest.program(
                            "+package foo", "", "[] > baz", "  42 > @", "  1.eq 1 ++> can-be-one"
                        ),
                        "foo.baz",
                        "foo/baz.eo"
                    ).execute(new PpMerge())
                )
            ),
            Matchers.stringContainsInOrder("can-be-one", "foo.baz", "foo")
        );
    }

    private static FakeMaven spliced(final Xtory pack, final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        for (final Map.Entry<?, ?> source : MjMergeTest.sources(pack).entrySet()) {
            final String name = source.getKey().toString();
            maven.withProgram(source.getValue().toString(), MjMergeTest.identifier(name), name);
        }
        maven.execute(new PpMerge());
        if (!MjMergeTest.asked(pack, "java").isEmpty()
            || !MjMergeTest.asked(pack, "tests").isEmpty()) {
            maven.execute(MjTranspile.class);
        }
        return maven;
    }

    private static Collection<String> unmatched(final Xtory pack, final FakeMaven maven)
        throws IOException {
        final Collection<String> failed = new ArrayList<>(0);
        for (final Object key : pack.map().keySet()) {
            if (!Arrays.asList("eo", "xmir", "java", "tests", "absent").contains(key)) {
                failed.add(String.format("unknown key: %s", key));
            }
        }
        failed.addAll(MjMergeTest.unmerged(pack, maven.targetPath().resolve(Merging.DIR)));
        failed.addAll(MjMergeTest.untranspiled(pack, maven));
        return failed;
    }

    private static Collection<String> unmerged(final Xtory pack, final Path base)
        throws IOException {
        final Collection<String> failed = new ArrayList<>(0);
        for (final Map.Entry<?, ?> entry : MjMergeTest.asked(pack, "xmir").entrySet()) {
            final String name = entry.getKey().toString();
            final Path file = base.resolve(name.replace(".eo", ".xmir"));
            if (Files.exists(file)) {
                failed.addAll(
                    MjMergeTest.absent(new XMLDocument(file), name, (List<?>) entry.getValue())
                );
            } else {
                failed.add(String.format("no merged XMIR for %s", name));
            }
        }
        for (final Object name : MjMergeTest.listed(pack, "absent")) {
            if (Files.exists(base.resolve(name.toString().replace(".eo", ".xmir")))) {
                failed.add(String.format("%s was merged while it should not be", name));
            }
        }
        return failed;
    }

    private static Collection<String> untranspiled(final Xtory pack, final FakeMaven maven)
        throws IOException {
        final Collection<String> failed = new ArrayList<>(0);
        for (final String kind : Arrays.asList("java", "tests")) {
            for (final Map.Entry<?, ?> entry : MjMergeTest.asked(pack, kind).entrySet()) {
                final String name = entry.getKey().toString();
                final Path file = MjMergeTest.generated(maven, name, kind);
                if (Files.exists(file)) {
                    failed.addAll(
                        MjMergeTest.missing(
                            Files.readString(file), name, (List<?>) entry.getValue()
                        )
                    );
                } else {
                    failed.add(String.format("nothing generated for %s under %s", name, kind));
                }
            }
        }
        return failed;
    }

    private static Path generated(final FakeMaven maven, final String name, final String kind)
        throws IOException {
        final Path base;
        final String mark;
        if ("tests".equals(kind)) {
            base = maven.generatedPath().getParent().resolve("generated-test-sources");
            mark = "Test";
        } else {
            base = maven.generatedPath();
            mark = "";
        }
        return base.resolve("org").resolve("eolang").resolve(
            String.format(
                "%sEO%s.java", mark, name.replace(".eo", "").replace("/", "$EO")
            )
        );
    }

    private static Map<?, ?> asked(final Xtory pack, final String key) {
        return (Map<?, ?>) pack.map().getOrDefault(key, Collections.emptyMap());
    }

    private static List<?> listed(final Xtory pack, final String key) {
        return (List<?>) pack.map().getOrDefault(key, Collections.emptyList());
    }

    private static Collection<String> missing(
        final String java, final String about, final List<?> texts
    ) {
        final Collection<String> failed = new ArrayList<>(0);
        for (final Object text : texts) {
            if (!java.contains(text.toString())) {
                failed.add(String.format("%s: %s", about, text));
            }
        }
        return failed;
    }

    private static Map<?, ?> sources(final Xtory pack) {
        return (Map<?, ?>) pack.map().get("eo");
    }

    private static String identifier(final String name) {
        return name.replace(".eo", "").replace('/', '.');
    }

    private static Collection<String> absent(
        final XML document, final String about, final List<?> xpaths
    ) {
        final Collection<String> failed = new ArrayList<>(0);
        for (final Object xpath : xpaths) {
            if (document.nodes(xpath.toString()).isEmpty()) {
                failed.add(String.format("%s: %s", about, xpath));
            }
        }
        return failed;
    }

    private static FakeMaven merged(final Path temp) throws IOException {
        return new FakeMaven(temp).withProgram(
            MjMergeTest.program("[n] > foo", "  n > @"),
            "foo",
            "foo.eo"
        ).withProgram(
            MjMergeTest.program("+package foo", "", "[] > bar", "  42 > @"),
            "foo.bar",
            "foo/bar.eo"
        ).execute(new PpMerge());
    }

    private static String root(final Throwable thrown) {
        Throwable cause = thrown;
        while (cause.getCause() != null) {
            cause = cause.getCause();
        }
        return cause.getMessage();
    }

    private static String program(final String... lines) {
        return String.join(System.lineSeparator(), lines);
    }
}
