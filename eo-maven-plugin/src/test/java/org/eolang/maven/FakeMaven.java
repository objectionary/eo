/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.hash.CommitHashesMap;
import org.eolang.maven.name.ObjectName;
import org.eolang.maven.name.OnDefault;
import org.eolang.maven.objectionary.Objectionaries;
import org.eolang.maven.rust.Names;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.tojos.ForeignTojos;
import org.eolang.maven.tojos.PlacedTojos;
import org.eolang.maven.util.HmBase;
import org.eolang.maven.util.Home;

/**
 * Fake maven workspace that executes Mojos in order to test
 * their behaviour and results.
 * NOT thread-safe.
 * @since 0.28.12
 */
@SuppressWarnings({
    "PMD.TooManyMethods",
    "PMD.CouplingBetweenObjects",
    "JTCOP.RuleAllTestsHaveProductionClass",
    "JTCOP.RuleCorrectTestName"
})
@NotThreadSafe
public final class FakeMaven {

    /**
     * Test workspace where we place all programs, files, compilation results, etc.
     */
    private final Home workspace;

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
    public FakeMaven(final Path workspace) {
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
            new HmBase(workspace),
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
        final Home workspace,
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
    public FakeMaven with(final String param, final Object value) {
        this.params.put(param, value);
        return this;
    }

    /**
     * Executes mojos in the workspace.
     * You can use utility classes to run predefined maven pipelines:
     *  - {@link org.eolang.maven.FakeMaven.Parse} to parse eo code
     *  - {@link org.eolang.maven.FakeMaven.Optimize} to parse and optimize eo code
     *  - see other inner classes below.
     * @param mojo Several mojos to execute.
     * @return Workspace after executing Mojo.
     * @throws IOException If some problem with filesystem is happened.
     */
    public FakeMaven execute(final Iterable<Class<? extends AbstractMojo>> mojo)
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
    public TjSmart foreign() {
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
     */
    public <T extends AbstractMojo> FakeMaven execute(final Class<T> mojo) throws IOException {
        if (this.defaults) {
            final Path transpiled = Paths.get("transpiled");
            final Path placed = Paths.get("placed.json");
            this.workspace.save(new TextOf(""), transpiled);
            this.params.putIfAbsent("targetDir", this.targetPath().toFile());
            this.params.putIfAbsent("foreign", this.foreignPath().toFile());
            this.params.putIfAbsent("foreignFormat", "csv");
            this.params.putIfAbsent("project", new MavenProjectStub());
            this.params.putIfAbsent("transpiled", this.workspace.absolute(transpiled).toFile());
            this.params.putIfAbsent("transpiledFormat", "csv");
            this.params.putIfAbsent("skipZeroVersions", true);
            this.params.putIfAbsent("discoverSelf", false);
            this.params.putIfAbsent("withVersions", false);
            this.params.putIfAbsent("ignoreVersionConflicts", false);
            this.params.putIfAbsent("ignoreTransitive", true);
            this.params.putIfAbsent("central", new DummyCentral());
            this.params.putIfAbsent("placed", this.workspace.absolute(placed).toFile());
            this.params.putIfAbsent("placedFormat", "json");
            this.params.putIfAbsent(
                "sourcesDir",
                this.workspace.absolute(Paths.get(".")).toFile()
            );
            this.params.putIfAbsent(
                "outputDir",
                this.workspace.absolute(Paths.get("target").resolve("classes")).toFile()
            );
            this.params.putIfAbsent(
                "cache",
                this.workspace.absolute(Paths.get("eo")).resolve("cache/parsed")
            );
            this.params.putIfAbsent("generateSodgXmlFiles", true);
            this.params.putIfAbsent("generateXemblyFiles", true);
            this.params.putIfAbsent("generateGraphFiles", true);
            this.params.putIfAbsent("generateDotFiles", true);
            this.params.putIfAbsent("generateDotFiles", true);
            this.params.putIfAbsent("generatedDir", this.generatedPath().toFile());
            this.params.putIfAbsent("placedFormat", "csv");
            this.params.putIfAbsent("plugin", FakeMaven.pluginDescriptor());
            this.params.putIfAbsent("objectionaries", new Objectionaries.Fake());
            this.params.putIfAbsent("rewriteBinaries", true);
            this.params.putIfAbsent("offline", false);
            this.params.putIfAbsent("phiOptimize", false);
            this.params.putIfAbsent(
                "eoPortalDir",
                new File("../eo-runtime/src/main/rust/eo")
            );
            this.params.putIfAbsent("namesDir", this.generatedPath().resolve("names").toFile());
            this.params.putIfAbsent(
                "names",
                new Names(((File) this.params.get("namesDir")).toPath())
            );
            this.params.putIfAbsent("hashes", new CommitHashesMap.Fake());
            this.params.putIfAbsent(
                "phiInputDir",
                this.workspace.absolute(
                    Paths.get(String.format("target/%s", OptimizeMojo.DIR))
                ).toFile()
            );
            this.params.putIfAbsent(
                "phiOutputDir",
                this.workspace.absolute(Paths.get("target/phi")).toFile()
            );
            this.params.putIfAbsent(
                "unphiInputDir",
                this.workspace.absolute(Paths.get("target/phi")).toFile()
            );
            this.params.putIfAbsent(
                "unphiOutputDir",
                this.workspace.absolute(
                    Paths.get(String.format("target/%s", ParseMojo.DIR))
                ).toFile()
            );
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
    public FakeMaven withProgram(final Input input) throws IOException {
        return this.withProgram(new UncheckedText(new TextOf(input)).asString());
    }

    /**
     * Path to compilation target directory.
     * @return Path to target dir.
     */
    public Path targetPath() {
        return this.workspace.absolute(Paths.get("target"));
    }

    /**
     * Path to generated directory.
     * @return Path to generated dir.
     */
    public Path generatedPath() {
        return this.targetPath().resolve("generated");
    }

    /**
     * Foreign tojos for eo-foreign.* file.
     * @return Foreign tojos.
     */
    ForeignTojos foreignTojos() {
        return new ForeignTojos(
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
            "+package f\n",
            "# This is the default 64+ symbols comment in front of named abstract object.",
            "[x] > main",
            "  (stdout \"Hello!\" x).print > @"
        );
    }

    /**
     * Add the correct versioned 'Hello world' program to workspace.
     * @return The same maven instance.
     * @throws IOException If method can't save eo program to the workspace.
     */
    FakeMaven withVersionedHelloWorld() throws IOException {
        return this.withProgram(
            "+package f\n",
            "# This is the default 64+ symbols comment in front of named abstract object.",
            "[] > main",
            "  QQ.io.stdout|0.28.5 > @",
            "    \"Hello world\""
        );
    }

    /**
     * Add correct versioned program to workspace.
     * @return The same maven instance.
     * @throws IOException If method can't save eo program to the workspace.
     */
    FakeMaven withVersionedProgram() throws IOException {
        return this.withProgram(
            "+alias org.eolang.math.number",
            "+alias org.eolang.txt.text",
            "+home https://objectionary.home",
            "+package f",
            "+version 0.0.0\n",
            "# This is the default 64+ symbols comment in front of named abstract object.",
            "[args] > main",
            "  seq|0.28.4 > @",
            "    QQ.io.stdout|0.28.5",
            "      QQ.txt.sprintf|0.28.6",
            "        \"Number %d, text %s\"",
            "        number 2",
            "        text|0.28.7",
            "          \"text\"",
            "    nop"
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
            new OnDefault(FakeMaven.tojoId(this.current.get()))
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
    FakeMaven withProgram(final String content, final ObjectName object)
        throws IOException {
        final Path path = Paths.get(
            String.format("foo/x/main%s.eo", FakeMaven.suffix(this.current.get()))
        );
        this.workspace.save(content, path);
        final String scope = this.scope();
        final String version = "0.25.0";
        final Path source = this.workspace.absolute(path);
        this.foreignTojos()
            .add(object)
            .withScope(scope)
            .withVersion(version)
            .withSource(source);
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
        return this.workspace.absolute(Paths.get("eo-foreign.csv"));
    }

    /**
     * Tojo for placed.json file.
     *
     * @return TjSmart of the current placed.json file.
     */
    PlacedTojos placed() {
        return new PlacedTojos(this.workspace.absolute(Paths.get("placed.json")));
    }

    /**
     * Creates of the result map with all files and folders that was created
     * or compiled during mojo execution.
     *
     * @return Map of "relative UNIX path" (key) - "absolute path" (value).
     * @throws IOException If some problem with filesystem have happened.
     */
    Map<String, Path> result() throws IOException {
        final Path root = this.workspace.absolute(Paths.get(""));
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
    ForeignTojo programTojo() {
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
            suffix = String.format("_%d", index);
        }
        return suffix;
    }

    /**
     * Plugin descriptor with test version.
     * @return Plugin descriptor.
     */
    static PluginDescriptor pluginDescriptor() {
        final PluginDescriptor descriptor = new PluginDescriptor();
        descriptor.setGroupId("org.eolang");
        descriptor.setArtifactId("eo-maven-plugin");
        descriptor.setVersion(FakeMaven.pluginVersion());
        return descriptor;
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
                ParseMojo.class
            ).iterator();
        }
    }

    /**
     * Optimization full pipeline.
     *
     * @since 0.28.12
     */
    static final class Optimize implements Iterable<Class<? extends AbstractMojo>> {
        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                ParseMojo.class,
                OptimizeMojo.class
            ).iterator();
        }
    }

    /**
     * Check errors and warnings.
     *
     * @since 0.31.0
     */
    static final class Verify implements Iterable<Class<? extends AbstractMojo>> {
        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                ParseMojo.class,
                OptimizeMojo.class,
                ShakeMojo.class,
                VerifyMojo.class
            ).iterator();
        }
    }

    /**
     * Shake full pipeline.
     *
     * @since 0.35.0
     */
    static final class Shake implements Iterable<Class<? extends AbstractMojo>> {
        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                ParseMojo.class,
                OptimizeMojo.class,
                ShakeMojo.class
            ).iterator();
        }
    }

    /**
     * Latex full pipeline.
     *
     * @since 0.29.2
     */
    static final class Latex implements Iterable<Class<? extends AbstractMojo>> {

        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                ParseMojo.class,
                OptimizeMojo.class,
                ShakeMojo.class,
                LatexMojo.class
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
                ParseMojo.class,
                OptimizeMojo.class,
                ShakeMojo.class,
                VerifyMojo.class,
                TranspileMojo.class
            ).iterator();
        }
    }

    /**
     * Binarize full pipeline.
     *
     * @since 0.29.0
     */
    static final class Binarize implements Iterable<Class<? extends AbstractMojo>> {

        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                ParseMojo.class,
                OptimizeMojo.class,
                ShakeMojo.class,
                VerifyMojo.class,
                BinarizeMojo.class
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
                ParseMojo.class,
                OptimizeMojo.class,
                ShakeMojo.class,
                ResolveMojo.class
            ).iterator();
        }
    }

    /**
     * Translates EO program to Phi-calculus expression.
     *
     * @since 0.33.0
     */
    static final class Phi implements Iterable<Class<? extends AbstractMojo>> {
        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                ParseMojo.class,
                OptimizeMojo.class,
                PhiMojo.class
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
                ParseMojo.class,
                OptimizeMojo.class,
                ShakeMojo.class,
                ResolveMojo.class,
                PlaceMojo.class
            ).iterator();
        }
    }

    /**
     * Sodg full pipeline.
     *
     * @since 0.29.0
     */
    static final class Sodg implements Iterable<Class<? extends AbstractMojo>> {

        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                ParseMojo.class,
                OptimizeMojo.class,
                ShakeMojo.class,
                SodgMojo.class
            ).iterator();
        }
    }

    /**
     * Single register phase.
     * @since 1.0
     */
    static final class Register implements Iterable<Class<? extends AbstractMojo>> {
        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                RegisterMojo.class
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
                ParseMojo.class,
                OptimizeMojo.class,
                ShakeMojo.class,
                DiscoverMojo.class,
                ProbeMojo.class
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
                ParseMojo.class,
                OptimizeMojo.class,
                ShakeMojo.class,
                DiscoverMojo.class,
                ProbeMojo.class,
                PullMojo.class
            ).iterator();
        }
    }

    /**
     * Discovery pipeline.
     *
     * @since 0.31
     */
    static final class Discover implements Iterable<Class<? extends AbstractMojo>> {

        @Override
        public Iterator<Class<? extends AbstractMojo>> iterator() {
            return Arrays.<Class<? extends AbstractMojo>>asList(
                ParseMojo.class,
                OptimizeMojo.class,
                ShakeMojo.class,
                DiscoverMojo.class
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
                PrintMojo.class
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
        public void accept(
            final Dependency dependency,
            final Path path
        ) {
            try {
                Files.createDirectories(path);
                final String other = DummyCentral.jarName(dependency);
                Files.createFile(path.resolve(other));
            } catch (final IOException ex) {
                throw new IllegalStateException(
                    String.format("Can't save '%s' to '%s'", dependency, path),
                    ex
                );
            }
        }

        /**
         * Dependency jar name.
         *
         * @param dependency Dependency
         * @return Jar file name
         */
        private static String jarName(final Dependency dependency) {
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
            return String.format("%s.jar", String.join("-", parts));
        }
    }
}
