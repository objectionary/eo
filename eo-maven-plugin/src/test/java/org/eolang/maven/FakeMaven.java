/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
import com.yegor256.tojos.Tojo;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.maven.plugin.AbstractMojo;

/**
 * Fake maven workspace that executes Mojos in order to test
 * their behaviour and results.
 * @since 0.28.12
 */
@SuppressWarnings("PMD.TooManyMethods")
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
     * Attributes for eo.foreign.*.
     */
    private final Map<String, Object> attributes;

    /**
     * The main constructor.
     *
     * @param workspace Test temporary directory.
     */
    public FakeMaven(final Path workspace) {
        this.workspace = new Home(workspace);
        this.params = new HashMap<>();
        this.attributes = new HashMap<>();
    }

    /**
     * Adds eo program to a workspace.
     * @param program Program as a raw string.
     * @return The same maven instance.
     * @throws IOException If method can't save eo program to the workspace.
     */
    public FakeMaven withProgram(final String... program) throws IOException {
        return this.withProgram(
            Paths.get("foo/x/main.eo"), String.join("\n", program)
        );
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
     * Sets tojo attribute.
     *
     * @param attribute Tojo attribute.
     * @param value Attribute value.
     * @return The same maven instance.
     */
    public FakeMaven withTojoAttribute(final String attribute, final Object value) {
        this.attributes.put(attribute, value);
        return this;
    }

    /**
     * Executes Mojo in the workspace.
     *
     * @param mojo Mojo to execute.
     * @param <T> Template for descendants of Mojo.
     * @return Workspace after executing Mojo.
     * @throws java.io.IOException If some problem with filesystem have happened.
     */
    public <T extends AbstractMojo> Map<String, Path> execute(
        final Class<T> mojo
    ) throws IOException {
        final Tojo tojo = Catalogs.INSTANCE.make(this.foreignPath())
            .add("foo.x.main")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_VERSION, "0.25.0");
        for (final Map.Entry<String, Object> entry : this.attributes.entrySet()) {
            tojo.set(entry.getKey(), entry.getValue());
        }
        this.params.putIfAbsent("targetDir", this.targetPath().toFile());
        this.params.putIfAbsent("foreign", this.foreignPath().toFile());
        this.params.putIfAbsent("foreignFormat", "csv");
        final Moja<T> moja = new Moja<>(mojo);
        for (final Map.Entry<String, ?> entry : this.params.entrySet()) {
            moja.with(entry.getKey(), entry.getValue());
        }
        moja.execute();
        return this.result();
    }

    /**
     * Path to compilation target directory.
     * @return Path to target dir.
     */
    public Path targetPath() {
        return this.workspace.absolute(Paths.get("target"));
    }

    /**
     * Path to 'eo-foreign.csv' or 'eo-foreign.json' file after all changes.
     * @return Path to eo-foreign.* file.
     */
    public Path foreignPath() {
        return this.workspace.absolute(Paths.get("eo-foreign.csv"));
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
     * Adds eo program to a workspace.
     * @param path Relative path where to save EO program
     * @param content EO program content.
     * @return The same maven instance.
     * @throws IOException If method can't save eo program to the workspace.
     */
    private FakeMaven withProgram(final Path path, final String content) throws IOException {
        this.workspace.save(content, path);
        this.withTojoAttribute(AssembleMojo.ATTR_EO, this.workspace.absolute(path));
        return this;
    }

    /**
     * Creates of the result map with all files and folders that was created
     *  or compiled during mojo execution.
     *
     * @return Map of "relative UNIX path" (key) - "absolute path" (value).
     * @throws IOException If some problem with filesystem have happened.
     */
    private Map<String, Path> result() throws IOException {
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
}
