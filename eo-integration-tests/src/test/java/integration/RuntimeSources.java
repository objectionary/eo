/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.Proc;

/**
 * Local {@code .eo} sources of the eo-runtime, planted into a fake reactor.
 *
 * <p>An integration test must build only against the {@code .eo} files that
 * live in this repository, never against the incompatible ones served by the
 * remote objectionary. This object copies every local runtime source into the
 * {@code src/main/eo} of a {@link Farea} sandbox, so that the plugin, running
 * offline, resolves all objects locally.</p>
 *
 * @since 0.57
 */
final class RuntimeSources implements Proc<Farea> {

    /**
     * Directory with local {@code .eo} sources of the eo-runtime.
     */
    private final Path dir;

    /**
     * Ctor, taking the sources of the {@code eo-runtime} of this reactor.
     *
     * <p>The module the tests run in sits next to {@code eo-runtime}, so the
     * sources are found one directory up. Every test that compiles the
     * runtime reaches for them the same way, which is why the path is
     * spelled here and not at the call sites.</p>
     */
    RuntimeSources() {
        this(
            Paths.get(System.getProperty("basedir", System.getProperty("user.dir")))
                .getParent()
                .resolve("eo-runtime")
                .resolve("src")
                .resolve("main")
                .resolve("eo")
        );
    }

    /**
     * Ctor.
     *
     * @param home Directory with local {@code .eo} sources
     */
    RuntimeSources(final Path home) {
        this.dir = home;
    }

    @Override
    public void exec(final Farea farea) throws IOException {
        if (!Files.isDirectory(this.dir)) {
            throw new IOException(
                String.format("Directory with runtime sources not found at '%s'", this.dir)
            );
        }
        final List<Path> sources;
        try (Stream<Path> paths = Files.walk(this.dir)) {
            sources = paths
                .filter(Files::isRegularFile)
                .filter(path -> path.getFileName().toString().endsWith(".eo"))
                .collect(Collectors.toList());
        }
        for (final Path source : sources) {
            farea.files()
                .file(String.format("src/main/eo/%s", this.dir.relativize(source)))
                .save(source);
        }
    }
}
