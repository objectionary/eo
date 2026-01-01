/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import org.cactoos.io.InputOf;
import org.cactoos.text.Joined;

/**
 * Footprint of Java generated files as input.
 * @since 0.56.7
 */
final class FpJavaGenerated extends FpEnvelope {
    /**
     * Ctor.
     * @param clazz Transpiled Java class
     * @param generated Generated path
     * @param target Target path
     */
    FpJavaGenerated(final Xnav clazz, final Path generated, final Path target) {
        this(clazz, new FileGenerationReport(new AtomicInteger(), generated, target));
    }

    /**
     * Ctor.
     * @param clazz Transpiled Java class
     * @param generated Generated
     */
    FpJavaGenerated(
        final Xnav clazz, final FileGenerationReport generated
    ) {
        super(
            new FpGenerated(
                src -> {
                    generated.incrementAndLog();
                    return new InputOf(
                        new Joined(
                            "",
                            clazz.elements(Filter.withName("java")).map(
                                java -> java.text().orElse("")
                            ).collect(Collectors.toList())
                        )
                    );
                }
            )
        );
    }
}
