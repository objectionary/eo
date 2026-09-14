/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.tojos.Mono;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javax.json.Json;
import javax.json.spi.JsonProvider;
import org.cactoos.Text;
import org.eolang.atoms.Engine;

/**
 * The {@code atoms.json} registry of one run, and the launcher it points
 * at.
 *
 * <p>It takes the directory of the run and the paths of the symbol table,
 * the boxes and the count of trips. It answers the path of a JSON file
 * phino reads, written next to a one-line shell script that starts
 * {@link Engine} in a JVM of its own with those paths in its environment.
 * One entry serves every λ the engine knows.</p>
 *
 * @since 0.77.0
 */
final class Registry {

    /**
     * Where the registry and the launcher go.
     */
    private final Path dir;

    /**
     * The table of symbols of the run.
     */
    private final Path symbols;

    /**
     * The table of boxes of the build.
     */
    private final Path boxes;

    /**
     * The file the trips over the wire are counted in.
     */
    private final Path wire;

    /**
     * Ctor.
     *
     * @param home Where the registry and the launcher go
     * @param table The table of symbols of the run
     * @param planted The table of boxes of the build
     * @param counted The file the trips over the wire are counted in
     */
    Registry(final Path home, final Path table, final Path planted,
        final Path counted) {
        this.dir = home;
        this.symbols = table;
        this.boxes = planted;
        this.wire = counted;
    }

    /**
     * Write both files.
     *
     * @return The registry file
     * @throws IOException If they cannot be written
     */
    Path saved() throws IOException {
        final Path launcher = this.dir.resolve("engine");
        Files.write(
            launcher,
            String.format(
                "#!/bin/sh%nSYMBOLS='%s' BOXES='%s' TRIPS='%s' exec '%s' -cp '%s' %s%n",
                this.symbols.toAbsolutePath(),
                this.boxes.toAbsolutePath(),
                this.wire.toAbsolutePath(),
                Paths.get(System.getProperty("java.home"), "bin", "java"),
                Registry.classpath(),
                Engine.class.getName()
            ).getBytes(StandardCharsets.UTF_8)
        );
        Files.setPosixFilePermissions(launcher, PosixFilePermissions.fromString("rwxr-xr-x"));
        final Path out = this.dir.resolve("atoms.json");
        Files.write(
            out,
            Json.createObjectBuilder().add(
                Registry.served(),
                Json.createObjectBuilder()
                    .add("rt", "exec")
                    .add("path", launcher.toAbsolutePath().toString())
                    .add("serve", true)
            ).build().toString().getBytes(StandardCharsets.UTF_8)
        );
        return out;
    }

    private static String served() {
        final List<String> names = Op.table().stream()
            .map(row -> row[0])
            .collect(Collectors.toList());
        names.add("L_dataized");
        names.add("L_fork");
        names.add("L_box_\\d+");
        return String.join("|", names);
    }

    private static String classpath() {
        final List<String> out = new ArrayList<>(5);
        for (final Class<?> type
            : new Class<?>[] {
                Engine.class, Json.class, JsonProvider.provider().getClass(), Text.class,
                Mono.class,
            }) {
            try {
                out.add(
                    Paths.get(
                        type.getProtectionDomain().getCodeSource().getLocation().toURI()
                    ).toString()
                );
            } catch (final URISyntaxException ex) {
                throw new IllegalStateException(
                    String.format("The code source of %s is not a path", type.getName()), ex
                );
            }
        }
        return out.stream().distinct().collect(
            Collectors.joining(System.getProperty("path.separator"))
        );
    }
}
