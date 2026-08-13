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
 * generated Java must hold, so the behaviour is stated as the program a human
 * would write rather than as XMIR by hand. What is left here are the mechanics
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
    void leavesEveryPackageAloneByDefault(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "no package is merged until one is named, so nothing may be written",
            Files.exists(MjMergeTest.merged(temp).targetPath().resolve(Merging.DIR)),
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
     * The program of a pack, parsed and merged, and transpiled as well when the
     * pack asks anything of the generated Java.
     * @param pack The pack
     * @param temp The temporary directory
     * @return The workspace, after the merge
     * @throws IOException If the pipeline fails
     */
    private static FakeMaven spliced(final Xtory pack, final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        for (final Map.Entry<?, ?> source : MjMergeTest.sources(pack).entrySet()) {
            final String name = source.getKey().toString();
            maven.withProgram(source.getValue().toString(), MjMergeTest.identifier(name), name);
        }
        maven.with("mergedPackages", pack.map().get("merged"))
            .execute(new FakeMaven.Merge());
        if (!MjMergeTest.asked(pack, "java").isEmpty()) {
            maven.execute(MjTranspile.class);
        }
        return maven;
    }

    /**
     * Everything the pack asks for that is not there, each named by the
     * document it was asked of, plus any key of the pack this runner does not
     * know, since a key nobody reads would switch its assertions off in
     * silence.
     * @param pack The pack
     * @param maven The workspace the merge has just written into
     * @return What failed, empty when the pack is satisfied
     * @throws IOException If a document cannot be read
     */
    private static Collection<String> unmatched(final Xtory pack, final FakeMaven maven)
        throws IOException {
        final Collection<String> failed = new ArrayList<>(0);
        for (final Object key : pack.map().keySet()) {
            if (!Arrays.asList("eo", "merged", "xmir", "java", "absent").contains(key)) {
                failed.add(String.format("unknown key: %s", key));
            }
        }
        failed.addAll(MjMergeTest.unmerged(pack, maven.targetPath().resolve(Merging.DIR)));
        failed.addAll(MjMergeTest.untranspiled(pack, maven));
        return failed;
    }

    /**
     * The XPaths the pack asks of the merged XMIR that match nothing, together
     * with the files it says must not be merged at all and were.
     * @param pack The pack
     * @param base The directory the merged XMIR was written to
     * @return What failed
     * @throws IOException If a document cannot be read
     */
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

    /**
     * The texts the pack asks of the generated Java that are not in it, which is
     * where a member that used to be an object of its own has to show up as an
     * attribute of the class of its package.
     * @param pack The pack
     * @param maven The workspace the merge has just written into
     * @return What failed
     * @throws IOException If a file cannot be read
     */
    private static Collection<String> untranspiled(final Xtory pack, final FakeMaven maven)
        throws IOException {
        final Collection<String> failed = new ArrayList<>(0);
        for (final Map.Entry<?, ?> entry : MjMergeTest.asked(pack, "java").entrySet()) {
            final String name = entry.getKey().toString();
            final Path file = MjMergeTest.generated(maven, name);
            if (Files.exists(file)) {
                failed.addAll(
                    MjMergeTest.missing(Files.readString(file), name, (List<?>) entry.getValue())
                );
            } else {
                failed.add(String.format("no Java generated for %s", name));
            }
        }
        return failed;
    }

    /**
     * The Java class a file of the program is transpiled into.
     * @param maven The workspace
     * @param name The name of the file
     * @return The path to the class
     * @throws IOException If the workspace cannot be read
     */
    private static Path generated(final FakeMaven maven, final String name) throws IOException {
        return maven.generatedPath().resolve("org").resolve("eolang").resolve(
            String.format("EO%s.java", name.replace(".eo", "").replace("/", "$EO"))
        );
    }

    /**
     * What the pack asks of one kind of document, by the name of the file.
     * @param pack The pack
     * @param key The key of the pack
     * @return The demands, empty when the pack asks nothing
     */
    private static Map<?, ?> asked(final Xtory pack, final String key) {
        return (Map<?, ?>) pack.map().getOrDefault(key, Collections.emptyMap());
    }

    /**
     * What the pack lists under one of its keys.
     * @param pack The pack
     * @param key The key of the pack
     * @return The list, empty when the pack lists nothing
     */
    private static List<?> listed(final Xtory pack, final String key) {
        return (List<?>) pack.map().getOrDefault(key, Collections.emptyList());
    }

    /**
     * The texts that are nowhere in the given one.
     * @param java The text of the generated class
     * @param about What the class is, for the message
     * @param texts The texts
     * @return The texts that failed
     */
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

    /**
     * The files of the program the pack describes.
     * @param pack The pack
     * @return The sources, by their names
     */
    private static Map<?, ?> sources(final Xtory pack) {
        return (Map<?, ?>) pack.map().get("eo");
    }

    /**
     * The name a file is known by in the tojos, which is what its path says
     * with the separators read as dots, exactly as {@link MjRegister} reads it.
     * @param name The name of the file
     * @return The identifier
     */
    private static String identifier(final String name) {
        return name.replace(".eo", "").replace('/', '.');
    }

    /**
     * The XPaths that match nothing in the given document.
     * @param document The document
     * @param about What the document is, for the message
     * @param xpaths The XPaths
     * @return The XPaths that failed
     */
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

    /**
     * A workspace holding the object {@code foo} with a void and the member
     * {@code foo.bar}, taken through parsing and merging.
     * @param temp The temporary directory
     * @param packages The packages to merge
     * @return The workspace, after the merge
     * @throws IOException If the pipeline fails
     */
    private static FakeMaven merged(final Path temp, final String... packages)
        throws IOException {
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
