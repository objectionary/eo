/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
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
     */
    FpJavaGenerated(final AtomicInteger saved, final Xnav clazz, final Path generated, final Path target) {
        super(
            new FpGenerated(
                src -> {
                    saved.incrementAndGet();
                    Logger.debug(
                        FpJavaGenerated.class,
                        "Generated %[file]s (%[size]s) file from %[file]s (%[size]s)",
                        generated, generated.toFile().length(),
                        target, target.toFile().length()
                    );
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
