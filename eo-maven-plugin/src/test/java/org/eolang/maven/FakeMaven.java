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
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.concurrent.NotThreadSafe;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugin.testing.stubs.MavenProjectStub;
import org.cactoos.Input;
import org.cactoos.scalar.ScalarOf;
import org.cactoos.scalar.Synced;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * Fake maven workspace that executes Mojos in order to test
 * their behaviour and results.
 * NOT thread-safe.
 * @since 0.28.12
 */
@SuppressWarnings({
    "PMD.TooManyMethods",
    "JTCOP.RuleAllTestsHaveProductionClass",
    "JTCOP.RuleCorrectTestName",
    "PMD.UnnecessaryLocalRule"
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
     *
     * @param workspace Test temporary directory.
     */
    FakeMaven(final Path workspace) {
        this(workspace, true);
    }

    /**
     * Constructor.
     *
     * @param workspace Test temporary directory.
     * @param defaults Use default attributes if they are not set.
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
     * @param workspace Test temporary directory.
     * @param params Mojos params.
     * @param current Current program number.
     * @param defaults Use default attributes if they are not set.
     * @checkstyle ParameterNumberCheck (10 lines)
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
     *
     * @param param Parameter name
     * @param value Parameter value
     * @return The same maven instance.
     */
    FakeMaven with(final String param, final Object value) {
        this.params.put(param, value);
        return this;
    }

    /**
     * Executes mojos in the workspace.
     * You can use utility classes to run predefined maven pipelines:
     *  - {@link org.eolang.maven.FakeMaven.Parse} to parse eo code
     *  - see other inner classes below.
     * @param mojo Several mojos to execute.
     * @return Workspace after executing Mojo.
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
     *
     * @return TjSmart of the current eo-foreign.file.
     */
    TjSmart foreign() {
        return new TjSmart(
            Catalogs.INSTANCE.make(this.foreignPath())
        );
    }

    /**
     * Executes Mojo in the workspace.
     *
     * @param mojo Mojo to execute.
     * @param <T> Template for descendants of Mojo.
     * @return Workspace after executing Mojo.
     * @throws java.io.IOException If some problem with filesystem has happened.
     * @checkstyle ExecutableStatementCountCheck (100 lines)
     * @checkstyle JavaNCSSCheck (100 lines)
     */
    <T extends AbstractMojo> FakeMaven execute(final Class<T> mojo) throws IOException {
        if (this.defaults) {
            final Path placed = Paths.get("placed.json");
            this.params.putIfAbsent("targetDir", this.targetPath().toFile());
            this.params.putIfAbsent(
                "xslMeasures", this.targetPath().resolve("measures.csv").toFile()
            );
            this.params.putIfAbsent("foreign", this.foreignPath().toFile());
            this.params.putIfAbsent("foreignFormat", "csv");
            this.params.putIfAbsent("project", new MavenProjectStub());
            this.params.putIfAbsent("transpiledFormat", "csv");
            this.params.putIfAbsent("skipZeroVersions", true);
            this.params.putIfAbsent("cacheEnabled", true);
            this.params.putIfAbsent("discoverSelf", false);
            this.params.putIfAbsent("ignoreVersionConflicts", false);
            this.params.putIfAbsent("ignoreTransitive", true);
            this.params.putIfAbsent("central", new DummyCentral());
            this.params.putIfAbsent("resolveInCentral", false);
            this.params.putIfAbsent("placed", this.workspace.resolve(placed).toFile());
            this.params.putIfAbsent("placedFormat", "json");
            this.params.putIfAbsent(
                "sourcesDir", this.workspace.resolve(".").toFile()
            );
            this.params.putIfAbsent(
                "cache", this.workspace.resolve("eo/cache/parsed").toFile()
            );
            this.params.putIfAbsent("generatedDir", this.generatedPath().toFile());
            this.params.putIfAbsent("placedFormat", "csv");
            this.params.putIfAbsent("plugin", FakeMaven.pluginDescriptor());
            this.params.putIfAbsent(
                "objectionary",
                new Synced<>(new ScalarOf<>(Objectionary.Fake::new))
            );
            this.params.putIfAbsent("rewriteBinaries", true);
            this.params.putIfAbsent("offline", false);
            this.params.putIfAbsent("classesDir", this.classesPath().toFile());
        }
        final Moja<T> moja = new Moja<>(mojo);
        for (final Map.Entry<String, ?> entry : this.allowedParams(mojo).entrySet()) {
            moja.with(entry.getKey(), entry.getValue());
        }
        moja.execute();
        return this;
    }

    /**
     * Adds eo program to a workspace.
     * @param input Program as an input.
     * @return The same maven instance.
     * @throws IOException If method can't save eo program to the workspace.
     */
    FakeMaven withProgram(final Input input) throws IOException {
        return this.withProgram(new UncheckedText(new TextOf(input)).asString());
    }

    /**
     * Path to compilation target directory.
     * @return Path to target dir.
     */
    Path targetPath() {
        return this.workspace.resolve("target");
    }

    /**
     * Path to generated directory.
     * @return Path to generated dir.
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
     * @return Foreign tojos.
     */
    TjsForeign foreignTojos() {
        return new TjsForeign(
            () -> Catalogs.INSTANCE.make(this.foreignPath()),
            this::scope
        );
    }

    /**
     * Sets placed tojo attribute.
     *
     * @param binary Binary as class file or jar.
     * @return The same maven instance.
     */
    FakeMaven withPlacedBinary(final Path binary) {
        this.placed().placeClass(binary, "", "test.jar");
        return this;
    }

    /**
     * Adds correct 'Hello world' program to workspace.
     * @return The same maven instance.
     * @throws IOException If method can't save eo program to the workspace.
     */
    FakeMaven withHelloWorld() throws IOException {
        return this.withProgram(
            "+alias stdout org.eolang.io.stdout",
            "+home https://www.eolang.org",
            "+package foo.x",
            "+version 0.0.0",
            "",
            "# No comments.",
            "[x] > main",
            "  (stdout \"Hello!\" x).print > @"
        );
    }

    /**
     * Adds eo program to a workspace.
     * @param program Program as a raw string.
     * @return The same maven instance.
     * @throws IOException If method can't save eo program to the workspace.
     */
    FakeMaven withProgram(final String... program) throws IOException {
        return this.withProgram(
            String.join("\n", program),
            FakeMaven.tojoId(this.current.get())
        );
    }

    /**
     * Adds eo program to a workspace.
     *
     * @param path Path to the program
     * @return The same maven instance
     * @throws IOException If fails
     */
    FakeMaven withProgram(final Path path) throws IOException {
        return this.withProgram(new UncheckedText(new TextOf(path)).asString());
    }

    /**
     * Adds eo program to a workspace.
     * @param content EO program content.
     * @param object Object name to save in tojos.
     * @return The same maven instance.
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
     * @param content EO program content.
     * @param object Object name to save in tojos.
     * @param source Source file name
     * @return The same maven instance.
     * @throws IOException If method can't save eo program to the workspace.
     */
    FakeMaven withProgram(
        final String content, final String object, final String source
    ) throws IOException {
        final Path src = this.workspace.resolve(source);
        new Saved(content, src).value();
        final String scope = this.scope();
        final String version = "0.25.0";
        this.foreignTojos()
            .add(object)
            .withScope(scope)
            .withVersion(version)
            .withSource(src);
        this.current.incrementAndGet();
        return this;
    }

    /**
     * Specify hash for all foreign tojos.
     * @param hash Commit hash
     * @return The same maven instance.
     */
    FakeMaven allTojosWithHash(final CommitHash hash) {
        this.foreignTojos().all().forEach(tojo -> tojo.withHash(hash));
        return this;
    }

    /**
     * Should we use defaults or not?
     * @return The same maven instance.
     */
    FakeMaven withoutDefaults() {
        return new FakeMaven(this.workspace, this.params, this.current, false);
    }

    /**
     * Path to eo-foreign.* file after all changes.
     * @return Path to eo-foreign.* file.
     */
    Path foreignPath() {
        return this.workspace.resolve("eo-foreign.csv");
    }

    /**
     * Tojo for placed.json file.
     *
     * @return TjSmart of the current placed.json file.
     */
    TjsPlaced placed() {
        return new TjsPlaced(this.workspace.resolve("placed.json"));
    }

    /**
     * Creates of the result map with all files and folders that was created
     * or compiled during mojo execution.
     *
     * @return Map of "relative UNIX path" (key) - "absolute path" (value).
     * @throws IOException If some problem with filesystem have happened.
     */
    Map<String, Path> result() throws IOException {
        final Path root = this.workspace.resolve("");
        return Files.walk(root).collect(
            Collectors.toMap(
                p -> String.join(
                    "/",
                    root.relativize(p).toString().split(Pattern.quote(File.separator))
                ),
                Function.identity()
            )
        );
    }

    /**
     * Retrieve the entry of the last program in the eo-foreign.csv file.
     * @return Tojo entry.
     */
    TjForeign programTojo() {
        return this.foreignTojos().find(FakeMaven.tojoId(this.current.get() - 1));
    }

    /**
     * The version of eo-maven-plugin for tests.
     * @return Version.
     */
    static String pluginVersion() {
        return "1.0-TEST";
    }

    /**
     * Suffix for the program name or path.
     * - main_1.eo
     * - foo.x.main100
     * - main.eo
     * @param index Number of the program.
     * @return String suffix.
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

    /**
     * Ensures the map of allowed params for the Mojo.
     *
     * @param mojo Mojo
     * @return Map of params that applicable to the Mojo
     */
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

    /**
     * Returns the current scope that was set.
     * @return The current scope.
     */
    private String scope() {
        return String.valueOf(this.params.getOrDefault("scope", "compile"));
    }

    /**
     * The id of the program in tojos file.
     * @param id Number of the program.
     * @return String id.
     */
    private static String tojoId(final int id) {
        return String.format("foo.x.main%s", FakeMaven.suffix(id));
    }

    /**
     * Plugin descriptor with test version.
     * @return Plugin descriptor.
     */
    private static PluginDescriptor pluginDescriptor() {
        final PluginDescriptor descriptor = new PluginDescriptor();
        descriptor.setGroupId("org.eolang");
        descriptor.setArtifactId("eo-maven-plugin");
        descriptor.setVersion(FakeMaven.pluginVersion());
        return descriptor;
    }

    /**
     * Looks for all declared fields for mojo and its parents.
     *
     * @param mojo Mojo or mojo parent.
     * @param fields Already collected fields.
     * @return All mojo and mojo parent fields.
     */
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

    /**
     * Parse full pipeline.
     *
     * @since 0.28.12
     */
    static final class Parse implements Iterable<Class<? extends AbstractMojo>> {

        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Collections.<Class<? extends AbstractMojo>>singletonList(
                MjParse.class
            ).iterator();
        }
    }

    /**
     * Check errors and warnings.
     *
     * @since 0.31.0
     */
    static final class Lint implements Iterable<Class<? extends AbstractMojo>> {
        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                MjParse.class,
                MjLint.class
            ).iterator();
        }
    }

    /**
     * Transpile full pipeline.
     *
     * @since 0.29.0
     */
    static final class Transpile implements Iterable<Class<? extends AbstractMojo>> {

        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                MjParse.class,
                MjLint.class,
                MjTranspile.class
            ).iterator();
        }
    }

    /**
     * Resolve all eo dependencies.
     *
     * @since 0.29.0
     */
    static final class Resolve implements Iterable<Class<? extends AbstractMojo>> {

        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                MjParse.class,
                MjResolve.class
            ).iterator();
        }
    }

    /**
     * Plan all eo dependencies full pipeline.
     *
     * @since 0.29.0
     */
    static final class Place implements Iterable<Class<? extends AbstractMojo>> {

        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                MjParse.class,
                MjResolve.class,
                MjPlace.class
            ).iterator();
        }
    }

    /**
     * Single register phase.
     * @since 0.1.0
     */
    static final class Register implements Iterable<Class<? extends AbstractMojo>> {
        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                MjRegister.class
            ).iterator();
        }
    }

    /**
     * Probe full pipeline.
     *
     * @since 0.29
     */
    static final class Probe implements Iterable<Class<? extends AbstractMojo>> {

        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                MjParse.class,
                MjProbe.class
            ).iterator();
        }
    }

    /**
     * Pull a full pipeline.
     *
     * @since 0.31
     */
    static final class Pull implements Iterable<Class<? extends AbstractMojo>> {

        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                MjParse.class,
                MjProbe.class,
                MjPull.class
            ).iterator();
        }
    }

    /**
     * Printing pipeline.
     *
     * @since 0.33.0
     */
    static final class Print implements Iterable<Class<? extends AbstractMojo>> {
        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                MjPrint.class
            ).iterator();
        }
    }

    /**
     * The class for emulating of Maven Central repository.
     * DummyCentral creates an empty dependency jar file under the path.
     *
     * @since 0.28.11
     */
    private static final class DummyCentral implements BiConsumer<Dependency, Path> {

        @Override
        public void accept(final Dependency dependency, final Path path) {
            try {
                Files.createDirectories(path);
                final String other = DummyCentral.className(dependency);
                Files.createFile(path.resolve(other));
            } catch (final IOException ex) {
                throw new IllegalStateException(
                    String.format("Can't save '%s' to '%s'", dependency, path),
                    ex
                );
            }
        }

        /**
         * Dependency class name.
         *
         * @param dependency Dependency
         * @return Class file name
         */
        private static String className(final Dependency dependency) {
            final List<String> parts = new ArrayList<>(3);
            if (dependency.getArtifactId() != null && !dependency.getArtifactId().isEmpty()) {
                parts.add(dependency.getArtifactId());
            }
            if (dependency.getVersion() != null && !dependency.getVersion().isEmpty()) {
                parts.add(dependency.getVersion());
            }
            if (dependency.getClassifier() != null && !dependency.getClassifier().isEmpty()) {
                parts.add(dependency.getClassifier());
            }
            return String.format("%s.class", String.join("-", parts));
        }
    }
}
