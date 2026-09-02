/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The Java files that a transpiled XMIR is written out as.
 *
 * <p>One XMIR carries a class per object, and every one of them becomes
 * a Java file under the generated sources directory, taken from the
 * global cache when the tojo is at a released hash and put there when
 * it is not.</p>
 *
 * @since 0.74
 */
final class JavaFiles {

    /**
     * Java extension.
     */
    private static final String JAVA = "java";

    /**
     * Generated sources directory.
     */
    private final Path generated;

    /**
     * Java files generated during the current transpilation.
     *
     * <p>The collection is shared by parallel {@link #total(boolean, Path,
     * String, boolean, GlobalCache)} calls. It is reconciled only after all XMIRs have
     * been processed.</p>
     */
    private final Collection<Path> fresh;

    /**
     * Where a class of the current run would have gone, written or not.
     *
     * <p>A class this run decided to skip, such as the implementation of an
     * atom, names a directory all the same, and a file an earlier run left
     * there is stale exactly as one beside a file we did write (#7763).</p>
     */
    private final Collection<Path> touched;

    /**
     * Ctor.
     * @param dir Generated sources directory
     */
    JavaFiles(final Path dir) {
        this.generated = dir;
        this.fresh = new ConcurrentLinkedQueue<>();
        this.touched = new ConcurrentLinkedQueue<>();
    }

    /**
     * Generate java files and count them.
     * @param rewrite Rewrite .java files even if they exist
     * @param target Full target path to XMIR after transpilation optimizations
     * @param hsh Tojo hash
     * @param tests Whether to generate test sources for this tojo
     * @param cache The cache of this XMIR, keyed by the objects it holds
     * @return Amount of generated .java files
     * @throws IOException If fails to save files
     * @checkstyle ParameterNumberCheck (6 lines)
     */
    int total(
        final boolean rewrite,
        final Path target,
        final String hsh,
        final boolean tests,
        final GlobalCache cache
    ) throws IOException {
        final AtomicInteger saved = new AtomicInteger(0);
        if (Files.exists(target)) {
            final Xnav object = new Xnav(target).element("object");
            final Collection<Xnav> classes = object.elements(Filter.withName("class"))
                .collect(Collectors.toList());
            final boolean atom = object.path("o/o[@name='λ']").findAny().isPresent();
            for (final Xnav clazz : classes) {
                final String jname = clazz.attribute("java-name").text().get();
                final Path tgt = new Place(jname).make(this.generated, JavaFiles.JAVA);
                this.touched.add(tgt);
                if (!atom || jname.endsWith("Test")) {
                    this.fresh.add(tgt);
                    final Footprint java = new FpJavaGenerated(
                        clazz, new FileGenerationReport(saved, tgt, target)
                    );
                    new JavaPlaced(
                        new FpIfReleased(
                            hsh,
                            cache.kept(
                                this.generated.relativize(tgt),
                                () -> hsh,
                                new RewritePolicy(rewrite, tgt),
                                java
                            ),
                            java
                        ),
                        tgt,
                        this.generated
                    ).exec(clazz, tests);
                }
            }
            Logger.debug(
                this,
                "Generated %d Java files from %[file]s",
                saved.get(), target
            );
        }
        return saved.get();
    }

    /**
     * Delete generated Java files absent from the current XMIR collection.
     *
     * <p>Only considers files inside a directory this run's own {@link
     * #fresh} output actually landed in, never the whole {@link #generated}
     * tree: {@code generated-sources} is the standard Maven convention
     * directory, and other generators (antlr4, protobuf, jaxb2, the
     * annotation-processor output of maven-compiler-plugin) can write their
     * own {@code .java} files there too, outside any directory eo itself
     * touched this run.</p>
     *
     * @throws IOException If fails to inspect or remove a generated file
     */
    void removeStale() throws IOException {
        if (Files.exists(this.generated)) {
            final Set<Path> expected = new HashSet<>(this.fresh);
            final Collection<Path> dirs = this.directories();
            try (Stream<Path> walk = Files.walk(this.generated)) {
                for (final Path file : walk.filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(JavaFiles.JAVA)).collect(
                        Collectors.toList()
                    )) {
                    if (dirs.contains(file.getParent())
                        && !expected.contains(file)
                        && !"package-info.java".equals(file.getFileName().toString())) {
                        Files.delete(file);
                        Logger.debug(this, "Deleted stale generated Java file %[file]s", file);
                    }
                }
            }
        }
    }

    /**
     * The directories this run's own output landed in, every parent up
     * to and including the generated sources root.
     *
     * <p>A class this run decided to skip names a directory all the
     * same, so the set covers what a transpile owns, and nothing another
     * generator wrote into the same convention directory.</p>
     *
     * @return The directories
     */
    Collection<Path> directories() {
        final Set<Path> dirs = new HashSet<>();
        for (final Path file : this.touched) {
            for (Path dir = file.getParent(); dir != null && dir.startsWith(this.generated);
                dir = dir.getParent()) {
                dirs.add(dir);
            }
        }
        return dirs;
    }
}
