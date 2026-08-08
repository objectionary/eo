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
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * The Java files that a transpiled XMIR is written out as.
 * <p>
 *     One XMIR carries a class per object, and every one of them becomes
 *     a Java file under the generated sources directory, taken from the
 *     global cache when the tojo is at a released hash and put there when
 *     it is not.
 * </p>
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
     * Cache directory for transpiled sources, with the cache-key version
     * segment already resolved into it.
     */
    private final Path cache;

    /**
     * Whether caching is enabled.
     */
    private final boolean enabled;

    /**
     * Ctor.
     * @param dir Generated sources directory
     * @param cached Cache directory for this transpile version
     * @param caching Whether caching is enabled
     */
    JavaFiles(final Path dir, final Path cached, final boolean caching) {
        this.generated = dir;
        this.cache = cached;
        this.enabled = caching;
    }

    /**
     * Generate java files and count them.
     * @param rewrite Rewrite .java files even if they exist
     * @param target Full target path to XMIR after transpilation optimizations
     * @param hsh Tojo hash
     * @param tests Whether to generate test sources for this tojo
     * @return Amount of generated .java files
     * @throws IOException If fails to save files
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    int total(
        final boolean rewrite,
        final Path target,
        final String hsh,
        final boolean tests
    ) throws IOException {
        final AtomicInteger saved = new AtomicInteger(0);
        if (Files.exists(target)) {
            final Xnav object = new Xnav(target).element("object");
            final Collection<Xnav> classes = object.elements(Filter.withName("class"))
                .collect(Collectors.toList());
            final boolean atom = object.path("o/o[@name='λ']").findAny().isPresent();
            for (final Xnav clazz : classes) {
                final String jname = clazz.attribute("java-name").text().get();
                if (!atom || jname.endsWith("Test")) {
                    final Path tgt = new Place(jname).make(
                        this.generated, JavaFiles.JAVA
                    );
                    final Footprint java = new FpJavaGenerated(
                        clazz, new FileGenerationReport(saved, tgt, target)
                    );
                    new JavaPlaced(
                        new FpIfReleased(
                            hsh,
                            new FpAppliedWithCache(
                                java,
                                this.cached(hsh, jname),
                                new RewritePolicy(rewrite, tgt),
                                this.enabled
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
     * Cached path supplier for a generated Java file.
     * @param hsh Hash
     * @param jname Java class name
     * @return Supplier of cached path
     */
    private Supplier<Path> cached(final String hsh, final String jname) {
        final Path tail = this.generated.relativize(
            new Place(jname).make(
                this.generated, JavaFiles.JAVA
            )
        );
        return () -> this.cache.resolve(hsh).resolve(tail);
    }
}
