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
package org.eolang.maven.testapi;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.maven.plugin.AbstractMojo;
import org.eolang.maven.AssembleMojo;
import org.eolang.maven.Catalogs;
import org.eolang.maven.Home;
import org.eolang.maven.Moja;

/**
 * Fake maven workspace that executes Mojos in order to test
 * their behaviour and results.
 * @since 0.28.12
 */
public final class FakeMaven {

    /**
     * Default eo program id.
     */
    private static final String PROGRAM_ID = "foo.x.main";

    /**
     * Default eo program path.
     */
    private static final String PROGRAM_PATH = "foo/x/main.eo";

    /**
     * Default eo-foreign.csv file format.
     */
    private static final String FOREIGN_FORMAT = "csv";

    /**
     * Default eo-foreign.csv file path.
     */
    private static final Path FOREIGN_PATH = Paths.get("eo-foreign.csv");

    /**
     * Test workspace where we place all programs, files, compilation results, etc.
     */
    private final Home workspace;

    /**
     * Path to a program in workspace.
     */
    private Path prog;

    /**
     * The main constructor.
     *
     * @param workspace Test temporary directory.
     */
    public FakeMaven(final Path workspace) {
        this.workspace = new Home(workspace);
    }

    /**
     * Adds eo program to a workspace.
     * @param program Program as a raw string.
     * @return Workspace with eo program.
     * @throws IOException If can't save eo program in workspace.
     */
    public FakeMaven program(final String... program) throws IOException {
        final Path path = Paths.get(FakeMaven.PROGRAM_PATH);
        this.workspace.save(String.join("\n", program), path);
        this.prog = path;
        return this;
    }

    /**
     * Executes Mojo in the workspace.
     *
     * @param mojo Mojo to execute.
     * @param <T> Template for descendants of Mojo.
     * @return Workspace after executing Mojo.
     */
    public <T extends AbstractMojo> FakeMaven execute(final Class<T> mojo) {
        this.withEoForeign();
        new Moja<>(mojo)
            .with("targetDir", this.targetPath().toFile())
            .with("foreign", this.foreignPath().toFile())
            .with("foreignFormat", FakeMaven.FOREIGN_FORMAT)
            .execute();
        return this;
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
        return this.workspace.absolute(FakeMaven.FOREIGN_PATH);
    }

    /**
     * Creates eo-foreign.* file. In the future it is going to be a method for `MavenWorkspace`
     * interface.
     */
    private void withEoForeign() {
        Catalogs.INSTANCE.make(this.foreignPath())
            .add(FakeMaven.PROGRAM_ID)
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, this.workspace.absolute(this.prog));
    }
}
