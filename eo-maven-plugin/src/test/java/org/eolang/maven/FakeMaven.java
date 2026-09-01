/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.tojos.TjSmart;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.concurrent.NotThreadSafe;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugin.testing.stubs.MavenProjectStub;
import org.cactoos.scalar.ScalarOf;
import org.cactoos.scalar.Synced;
import org.cactoos.set.SetOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * Fake maven workspace that executes Mojos in order to test
 * their behaviour and results.
 * NOT thread-safe.
 * @since 0.28.12
 */
@SuppressWarnings({
    "JTCOP.RuleAllTestsHaveProductionClass",
    "JTCOP.RuleCorrectTestName"
})
@NotThreadSafe
final class FakeMaven {

    /**
     * Test workspace where we place all programs, files, compilation results, etc.
     */
    private final Path workspace;

    /**
     * Mojos params.
     */
    private final Map<String, Object> params;

    /**
     * Current program number.
     * We can save several programs in workspace and each program has it's own number
     * started from 0.
     */
    private final AtomicInteger current;

    /**
     * Use default attributes if they are not set.
     */
    private final boolean defaults;

    /**
     * Constructor.
     * @param workspace Test temporary directory
     */
    FakeMaven(final Path workspace) {
        this(workspace, true);
    }

    /**
     * Constructor.
     * @param workspace Test temporary directory
     * @param defaults Use default attributes if they are not set
     */
    private FakeMaven(final Path workspace, final boolean defaults) {
        this(
            workspace,
            new HashMap<>(),
            new AtomicInteger(0),
            defaults
        );
    }

    /**
     * The main constructor.
     * @param workspace Test temporary directory
     * @param params Mojos params
     * @param current Current program number
     * @param defaults Use default attributes if they are not set
     */
    private FakeMaven(
        final Path workspace,
        final Map<String, Object> params,
        final AtomicInteger current,
        final boolean defaults
    ) {
        this.workspace = workspace;
        this.params = params;
        this.current = current;
        this.defaults = defaults;
    }

    /**
     * Sets parameter for execution.
     * @param param Parameter name
     * @param value Parameter value
     * @return The same maven instance
     */
    FakeMaven with(final String param, final Object value) {
        final Set<String> known = new MojoFields().all();
        if (!known.isEmpty() && !known.contains(param)) {
            throw new IllegalArgumentException(
                String.format(
                    "No mojo of the plugin declares a parameter named '%s', so setting it would change nothing",
                    param
                )
            );
        }
        this.params.put(param, value);
        return this;
    }

    /**
     * Executes mojos in the workspace.
     * You can use utility classes to run predefined maven pipelines:
     * - {@link org.eolang.maven.PpParse} to parse eo code
     * - see other inner classes below.
     * @param mojo Several mojos to execute
     * @return Workspace after executing Mojo
     * @throws IOException If some problem with filesystem is happened.
     */
    FakeMaven execute(final Iterable<Class<? extends AbstractMojo>> mojo)
        throws IOException {
        for (final Class<? extends AbstractMojo> clazz : mojo) {
            this.execute(clazz);
        }
        return this;
    }

    /**
     * Tojo for eo-foreign.* file.
     * @return TjSmart of the current eo-foreign.file
     */
    TjSmart foreign() {
        return new TjSmart(
            Catalogs.INSTANCE.make(this.foreignPath())
        );
    }

    /**
     * Executes Mojo in the workspace.
     * @param mojo Mojo to execute
     * @param <T> Template for descendants of Mojo
     * @return Workspace after executing Mojo
     * @throws java.io.IOException If some problem with filesystem has happened.
     */
    <T extends AbstractMojo> FakeMaven execute(final Class<T> mojo) throws IOException {
        if (this.defaults) {
            this.params.putIfAbsent("targetDir", this.targetPath().toFile());
            this.params.putIfAbsent(
                "measures", this.targetPath().resolve("measures.csv").toFile()
            );
            this.params.putIfAbsent("foreign", this.foreignPath().toFile());
            this.params.putIfAbsent("foreignfmt", "csv");
            final MavenProjectStub stub = new MavenProjectStub();
            stub.setCompileSourceRoots(new ArrayList<>(0));
            this.params.putIfAbsent("project", stub);
            this.params.putIfAbsent("transpiledFormat", "csv");
            this.params.putIfAbsent("skipZeroVersions", true);
            this.params.putIfAbsent("cacheEnabled", true);
            this.params.putIfAbsent("discover", false);
            this.params.putIfAbsent("ignoreConflicts", false);
            this.params.putIfAbsent("central", new DummyCentral());
            this.params.putIfAbsent("centrally", false);
            this.params.putIfAbsent(
                "placed",
                this.workspace.resolve(Paths.get("placed.json")).toFile()
            );
            this.params.putIfAbsent("placedfmt", "json");
            this.params.putIfAbsent(
                "sourcesDir", this.workspace.resolve(".").toFile()
            );
            this.params.putIfAbsent(
                "cache", this.workspace.resolve("eo/cache/parsed").toFile()
            );
            this.params.putIfAbsent("generatedDir", this.generatedPath().toFile());
            this.params.putIfAbsent(
                "prepared", this.targetPath().resolve("6-pre-inference").toFile()
            );
            this.params.putIfAbsent(
                "tables", this.targetPath().resolve("6-inference").toFile()
            );
            this.params.putIfAbsent(
                "pages", this.targetPath().getParent().resolve("site/inference").toFile()
            );
            this.params.putIfAbsent("placedfmt", "csv");
            this.params.putIfAbsent("plugin", FakeMaven.pluginDescriptor());
            this.params.putIfAbsent(
                "objectionary",
                new Synced<>(new ScalarOf<>(Objectionary.Fake::new))
            );
            this.params.putIfAbsent("rewriteBinaries", true);
            this.params.putIfAbsent("offline", false);
            this.params.putIfAbsent("classesDir", this.classesPath().toFile());
            this.params.putIfAbsent("superclass", "PhDefault");
            this.params.putIfAbsent("attach", true);
            this.params.putIfAbsent("tests", true);
            this.params.putIfAbsent("strict", true);
            this.params.putIfAbsent("included", new SetOf<>("**.eo"));
        }
        final Moja<T> moja = new Moja<>(mojo);
        for (final Map.Entry<String, ?> entry : this.allowedParams(mojo).entrySet()) {
            moja.with(entry.getKey(), entry.getValue());
        }
        moja.execute();
        return this;
    }

    /**
     * Path to compilation target directory.
     * @return Path to target dir
     */
    Path targetPath() {
        return this.workspace.resolve("target");
    }

    /**
     * Path to generated directory.
     * @return Path to generated dir
     */
    Path generatedPath() {
        return this.targetPath().resolve("generated");
    }

    /**
     * Path to classes directory.
     * @return Path to classes directory
     */
    Path classesPath() {
        return this.targetPath().resolve("classes");
    }

    /**
     * Foreign tojos for eo-foreign.* file.
     * @return Foreign tojos
     */
    TjsForeign foreignTojos() {
        return new TjsForeign(
            () -> Catalogs.INSTANCE.make(this.foreignPath()),
            this::scope
        );
    }

    /**
     * Sets placed tojo attribute.
     * @param binary Binary as class file or jar
     * @return The same maven instance
     */
    FakeMaven withPlacedBinary(final Path binary) {
        this.placed().placeClass(binary, "", "test.jar");
        return this;
    }

    /**
     * Adds correct 'Hello world' program to workspace.
     * @return The same maven instance
     * @throws IOException If method can't save eo program to the workspace.
     */
    FakeMaven withHelloWorld() throws IOException {
        return this.withProgram(new HelloWorld().asString());
    }

    /**
     * Adds eo program to a workspace.
     * @param program Program as a raw string
     * @return The same maven instance
     * @throws IOException If method can't save eo program to the workspace.
     */
    FakeMaven withProgram(final String... program) throws IOException {
        return this.withProgram(
            String.join(System.lineSeparator(), program),
            FakeMaven.tojoId(this.current.get())
        );
    }

    /**
     * Adds eo program to a workspace.
     * @param path Path to the program
     * @return The same maven instance
     * @throws IOException If fails
     */
    FakeMaven withProgram(final Path path) throws IOException {
        return this.withProgram(new UncheckedText(new TextOf(path)).asString());
    }

    /**
     * Adds eo program to a workspace.
     * @param content EO program content
     * @param object Object name to save in tojos
     * @return The same maven instance
     * @throws IOException If method can't save eo program to the workspace.
     */
    FakeMaven withProgram(
        final String content, final String object
    ) throws IOException {
        return this.withProgram(
            content,
            object,
            String.format("foo/x/main%s.eo", FakeMaven.suffix(this.current.get()))
        );
    }

    /**
     * Adds eo program to a workspace.
     * @param content EO program content
     * @param object Object name to save in tojos
     * @param source Source file name
     * @return The same maven instance
     * @throws IOException If method can't save eo program to the workspace.
     */
    FakeMaven withProgram(
        final String content, final String object, final String source
    ) throws IOException {
        final Path src = this.workspace.resolve(source);
        new Saved(content, src).value();
        this.foreignTojos()
            .add(object)
            .withScope(this.scope())
            .withVersion("0.25.0")
            .withSource(src);
        this.current.incrementAndGet();
        return this;
    }

    /**
     * Specify hash for all foreign tojos.
     * @param hash Commit hash
     * @return The same maven instance
     */
    FakeMaven allTojosWithHash(final CommitHash hash) {
        this.foreignTojos().all().forEach(tojo -> tojo.withHash(hash));
        return this;
    }

    /**
     * Should we use defaults or not?
     * @return The same maven instance
     */
    FakeMaven withoutDefaults() {
        return new FakeMaven(this.workspace, this.params, this.current, false);
    }

    /**
     * Path to eo-foreign.* file after all changes.
     * @return Path to eo-foreign.* file
     */
    Path foreignPath() {
        return this.workspace.resolve("eo-foreign.csv");
    }

    /**
     * Tojo for placed.json file.
     * @return TjSmart of the current placed.json file
     */
    TjsPlaced placed() {
        return new TjsPlaced(this.workspace.resolve("placed.json"));
    }

    /**
     * Creates of the result map with all files and folders that was created
     * or compiled during mojo execution.
     * @return Map of "relative UNIX path" (key) - "absolute path" (value)
     * @throws IOException If some problem with filesystem have happened.
     */
    Map<String, Path> result() throws IOException {
        final Path root = this.workspace.resolve("");
        try (Stream<Path> stream = Files.walk(root)) {
            return stream.collect(
                Collectors.toMap(
                    p -> String.join(
                        "/",
                        root.relativize(p).toString().split(Pattern.quote(File.separator))
                    ),
                    Function.identity()
                )
            );
        }
    }

    /**
     * Retrieve the entry of the last program in the eo-foreign.csv file.
     * @return Tojo entry
     */
    TjForeign programTojo() {
        return this.foreignTojos().find(FakeMaven.tojoId(this.current.get() - 1));
    }

    /**
     * The version of eo-maven-plugin for tests.
     * @return Version
     */
    static String pluginVersion() {
        return "1.0-TEST";
    }

    /**
     * Suffix for the program name or path.
     * - main_1.eo
     * - foo.x.main100
     * - main.eo
     * @param index Number of the program
     * @return String suffix
     */
    static String suffix(final int index) {
        final String suffix;
        if (index == 0) {
            suffix = "";
        } else {
            suffix = String.format("-%d", index);
        }
        return suffix;
    }

    private Map<String, ?> allowedParams(final Class<? extends AbstractMojo> mojo) {
        final Map<String, Object> res = new HashMap<>();
        final Set<String> allowed = FakeMaven.mojoFields(mojo, new HashSet<>());
        for (final Map.Entry<String, Object> entry : this.params.entrySet()) {
            if (allowed.contains(entry.getKey())) {
                res.put(entry.getKey(), entry.getValue());
            }
        }
        return res;
    }

    private String scope() {
        return String.valueOf(this.params.getOrDefault("scope", "compile"));
    }

    private static String tojoId(final int id) {
        return String.format("foo.x.main%s", FakeMaven.suffix(id));
    }

    private static PluginDescriptor pluginDescriptor() {
        final PluginDescriptor descriptor = new PluginDescriptor();
        descriptor.setGroupId("org.eolang");
        descriptor.setArtifactId("eo-maven-plugin");
        descriptor.setVersion(FakeMaven.pluginVersion());
        return descriptor;
    }

    private static Set<String> mojoFields(final Class<?> mojo, final Set<String> fields) {
        final Set<String> res;
        if (mojo == null) {
            res = fields;
        } else {
            Stream.of(mojo.getDeclaredFields()).map(Field::getName).forEach(fields::add);
            res = mojoFields(mojo.getSuperclass(), fields);
        }
        return res;
    }
}
